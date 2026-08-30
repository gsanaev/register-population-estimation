from pathlib import Path

import numpy as np
import pandas as pd
import yaml

from simulation.education_sources import (
    ZENSUS_2022_COLUMNS,
    generate_zensus_2022_delivery,
)
from simulation.education_truth import (
    generate_education_truth,
)
from simulation.world import (
    build_age_bands,
    build_population_truth,
    build_regions,
    generate_address_register,
    generate_former_residents,
    generate_households,
    generate_true_residents,
)


REPO_ROOT = Path(__file__).resolve().parents[2]
CONFIG_PATH = REPO_ROOT / "config" / "simulation.yml"


def load_config() -> dict:
    with CONFIG_PATH.open(
        encoding="utf-8"
    ) as config_file:
        return yaml.safe_load(
            config_file
        )


def build_education_inputs():
    config = load_config()

    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        config[
            "population"
        ]["n_true_residents"],
        addresses,
        config,
        np.random.default_rng(20261),
    )

    true_residents = generate_true_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20262),
    )

    former_residents = generate_former_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20263),
    )

    population_truth = build_population_truth(
        true_residents,
        former_residents,
        config,
    )

    education_truth = generate_education_truth(
        population_truth,
        config,
        np.random.default_rng(
            config[
                "education"
            ][
                "truth_seed"
            ]
        ),
    )

    return (
        config,
        education_truth,
    )


def expected_zensus_counts(
    config: dict,
    education_truth: pd.DataFrame,
) -> tuple[int, dict[str, int]]:
    source_config = config[
        "education"
    ][
        "zensus_2022"
    ]

    truth_2022 = education_truth[
        education_truth[
            "reference_year"
        ].eq(2022)
    ]

    n_base = int(
        np.floor(
            len(truth_2022)
            * source_config[
                "coverage"
            ]
        )
    )

    counts = {
        "measurement_error": int(
            np.floor(
                n_base
                * source_config[
                    "measurement_error_rate"
                ]
            )
        ),
        "missing_qualification": int(
            np.floor(
                n_base
                * source_config[
                    "missing_qualification_rate"
                ]
            )
        ),
        "unknown_code": int(
            np.floor(
                n_base
                * source_config[
                    "unknown_code_rate"
                ]
            )
        ),
        "invalid_year": int(
            np.floor(
                n_base
                * source_config[
                    "invalid_year_rate"
                ]
            )
        ),
        "missing_person_id": int(
            np.floor(
                n_base
                * source_config[
                    "missing_person_id_rate"
                ]
            )
        ),
        "duplicate_records": int(
            np.floor(
                n_base
                * source_config[
                    "duplicate_rate"
                ]
            )
        ),
    }

    return (
        n_base,
        counts,
    )


def test_zensus_schema_and_row_counts() -> None:
    (
        config,
        education_truth,
    ) = build_education_inputs()

    delivery = generate_zensus_2022_delivery(
        education_truth,
        config,
        np.random.default_rng(20221),
    )

    n_base, counts = expected_zensus_counts(
        config,
        education_truth,
    )

    assert list(
        delivery.columns
    ) == ZENSUS_2022_COLUMNS

    assert len(delivery) == (
        n_base
        + counts[
            "duplicate_records"
        ]
    )


def test_zensus_defect_counts_are_exact() -> None:
    (
        config,
        education_truth,
    ) = build_education_inputs()

    delivery = generate_zensus_2022_delivery(
        education_truth,
        config,
        np.random.default_rng(20221),
    )

    _, counts = expected_zensus_counts(
        config,
        education_truth,
    )

    assert int(
        delivery[
            "person_id"
        ].isna().sum()
    ) == counts[
        "missing_person_id"
    ]

    assert int(
        delivery[
            "highest_qualification"
        ].isna().sum()
    ) == counts[
        "missing_qualification"
    ]

    assert int(
        delivery[
            "highest_qualification"
        ].eq(
            "UNKNOWN_CODE"
        ).sum()
    ) == counts[
        "unknown_code"
    ]

    assert int(
        delivery[
            "reference_year"
        ].ne(2022).sum()
    ) == counts[
        "invalid_year"
    ]


def test_zensus_measurement_errors_are_exact_and_plausible() -> None:
    (
        config,
        education_truth,
    ) = build_education_inputs()

    delivery = generate_zensus_2022_delivery(
        education_truth,
        config,
        np.random.default_rng(20221),
    )

    _, counts = expected_zensus_counts(
        config,
        education_truth,
    )

    codes = config[
        "education"
    ][
        "zensus_2022"
    ][
        "qualification_codes"
    ]

    code_to_level = {
        code: level
        for level, code in enumerate(
            codes,
            start=1,
        )
    }

    known = delivery.loc[
        delivery[
            "person_id"
        ].notna()
        & delivery[
            "highest_qualification"
        ].isin(
            code_to_level
        )
    ].copy()

    known[
        "observed_level"
    ] = known[
        "highest_qualification"
    ].map(
        code_to_level
    )

    truth_2022 = education_truth.loc[
        education_truth[
            "reference_year"
        ].eq(2022),
        [
            "person_id",
            "age_at_reference_year",
            "true_attainment_level",
        ],
    ]

    checked = known.merge(
        truth_2022,
        on="person_id",
        how="left",
        validate="many_to_one",
    )

    mismatch = (
        checked[
            "observed_level"
        ]
        != checked[
            "true_attainment_level"
        ]
    )

    assert int(
        mismatch.sum()
    ) == counts[
        "measurement_error"
    ]

    differences = (
        checked.loc[
            mismatch,
            "observed_level",
        ].to_numpy()
        - checked.loc[
            mismatch,
            "true_attainment_level",
        ].to_numpy()
    )

    assert (
        np.abs(
            differences
        )
        == 1
    ).all()

    minimum_ages = np.asarray(
        config[
            "education"
        ][
            "truth"
        ][
            "minimum_age_by_level"
        ],
        dtype=np.int64,
    )

    observed_levels = checked[
        "observed_level"
    ].to_numpy(
        dtype=np.int64
    )

    assert (
        checked[
            "age_at_reference_year"
        ].to_numpy()
        >= minimum_ages[
            observed_levels - 1
        ]
    ).all()


def test_zensus_persons_belong_to_2022_truth() -> None:
    (
        config,
        education_truth,
    ) = build_education_inputs()

    delivery = generate_zensus_2022_delivery(
        education_truth,
        config,
        np.random.default_rng(20221),
    )

    n_base, counts = expected_zensus_counts(
        config,
        education_truth,
    )

    truth_ids = set(
        education_truth.loc[
            education_truth[
                "reference_year"
            ].eq(2022),
            "person_id",
        ]
    )

    observed_ids = set(
        delivery[
            "person_id"
        ].dropna()
    )

    assert observed_ids.issubset(
        truth_ids
    )

    assert delivery[
        "person_id"
    ].nunique(
        dropna=True
    ) == (
        n_base
        - counts[
            "missing_person_id"
        ]
    )


def test_zensus_duplicate_keys_are_exact() -> None:
    (
        config,
        education_truth,
    ) = build_education_inputs()

    delivery = generate_zensus_2022_delivery(
        education_truth,
        config,
        np.random.default_rng(20221),
    )

    _, counts = expected_zensus_counts(
        config,
        education_truth,
    )

    key_counts = (
        delivery.loc[
            delivery[
                "person_id"
            ].notna()
        ]
        .groupby(
            [
                "person_id",
                "reference_year",
            ]
        )
        .size()
    )

    duplicated_keys = key_counts[
        key_counts > 1
    ]

    assert len(
        duplicated_keys
    ) == counts[
        "duplicate_records"
    ]

    assert (
        duplicated_keys == 2
    ).all()


def test_zensus_generation_is_reproducible() -> None:
    (
        config,
        education_truth,
    ) = build_education_inputs()

    first = generate_zensus_2022_delivery(
        education_truth,
        config,
        np.random.default_rng(20221),
    )

    second = generate_zensus_2022_delivery(
        education_truth,
        config,
        np.random.default_rng(20221),
    )

    pd.testing.assert_frame_equal(
        first,
        second,
    )
