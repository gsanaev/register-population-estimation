from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import yaml

from simulation.education_truth import (
    ATTAINMENT_LABELS,
    EDUCATION_TRUTH_COLUMNS,
    attainment_label,
    generate_education_truth,
    sample_attainment_by_age,
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


def build_population_truth_input():
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

    return (
        config,
        population_truth,
    )


def test_attainment_labels() -> None:
    levels = np.arange(
        1,
        7,
        dtype=np.int64,
    )

    labels = attainment_label(
        levels
    )

    np.testing.assert_array_equal(
        labels,
        np.asarray(
            ATTAINMENT_LABELS,
            dtype=object,
        ),
    )

    with pytest.raises(ValueError):
        attainment_label(
            np.array(
                [0],
                dtype=np.int64,
            )
        )


def test_attainment_sampling_respects_age_thresholds() -> None:
    config = load_config()

    ages = np.repeat(
        np.array(
            [
                15,
                16,
                20,
                22,
                25,
            ],
            dtype=np.int64,
        ),
        2_000,
    )

    levels = sample_attainment_by_age(
        ages,
        config,
        np.random.default_rng(2027),
    )

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

    assert (
        ages
        >= minimum_ages[
            levels - 1
        ]
    ).all()


def test_attainment_sampling_probabilities_are_plausible() -> None:
    config = load_config()

    profiles = {
        17: "age_15_17",
        24: "age_18_24",
        39: "age_25_39",
        64: "age_40_64",
        65: "age_65_plus",
    }

    rng = np.random.default_rng(
        2027
    )

    configured = config[
        "education"
    ][
        "truth"
    ][
        "base_attainment_probabilities"
    ]

    for age, profile in profiles.items():
        ages = np.full(
            8_000,
            age,
            dtype=np.int64,
        )

        levels = sample_attainment_by_age(
            ages,
            config,
            rng,
        )

        realised = np.bincount(
            levels,
            minlength=7,
        )[1:] / len(levels)

        expected = np.asarray(
            configured[
                profile
            ],
            dtype=float,
        )

        assert np.max(
            np.abs(
                realised
                - expected
            )
        ) < 0.025


def test_education_truth_structure_and_scope() -> None:
    (
        config,
        population_truth,
    ) = build_population_truth_input()

    education_truth = (
        generate_education_truth(
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
    )

    assert list(
        education_truth.columns
    ) == EDUCATION_TRUTH_COLUMNS

    assert not education_truth.duplicated(
        subset=[
            "person_id",
            "reference_year",
        ]
    ).any()

    assert set(
        education_truth[
            "reference_year"
        ]
    ) == {
        2022,
        2024,
    }

    residents = population_truth.loc[
        population_truth[
            "true_resident"
        ].eq(1),
        [
            "person_id",
            "age",
        ],
    ]

    expected_2022 = int(
        (
            residents["age"] - 3
            >= 15
        ).sum()
    )

    expected_2024 = int(
        (
            residents["age"] - 1
            >= 15
        ).sum()
    )

    actual_2022 = int(
        education_truth[
            "reference_year"
        ].eq(2022).sum()
    )

    actual_2024 = int(
        education_truth[
            "reference_year"
        ].eq(2024).sum()
    )

    assert actual_2022 == expected_2022
    assert actual_2024 == expected_2024

    truth_ids = set(
        education_truth["person_id"]
    )

    resident_ids = set(
        residents["person_id"]
    )

    assert truth_ids.issubset(
        resident_ids
    )


def test_education_truth_ages_levels_and_labels_are_valid() -> None:
    (
        config,
        population_truth,
    ) = build_population_truth_input()

    education_truth = (
        generate_education_truth(
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
    )

    checked = education_truth.merge(
        population_truth[
            [
                "person_id",
                "age",
            ]
        ],
        on="person_id",
        how="left",
        validate="many_to_one",
    )

    expected_age = np.where(
        checked[
            "reference_year"
        ].eq(2022),
        checked["age"] - 3,
        checked["age"] - 1,
    )

    np.testing.assert_array_equal(
        checked[
            "age_at_reference_year"
        ].to_numpy(),
        expected_age,
    )

    assert checked[
        "age_at_reference_year"
    ].ge(15).all()

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

    levels = checked[
        "true_attainment_level"
    ].to_numpy(
        dtype=np.int64
    )

    assert (
        checked[
            "age_at_reference_year"
        ].to_numpy()
        >= minimum_ages[
            levels - 1
        ]
    ).all()

    np.testing.assert_array_equal(
        checked[
            "true_attainment_label"
        ].to_numpy(dtype=object),
        attainment_label(
            levels
        ),
    )


def test_education_truth_progression_is_monotone_and_one_level() -> None:
    (
        config,
        population_truth,
    ) = build_population_truth_input()

    education_truth = (
        generate_education_truth(
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
    )

    truth_2022 = (
        education_truth.loc[
            education_truth[
                "reference_year"
            ].eq(2022),
            [
                "person_id",
                "true_attainment_level",
            ],
        ]
        .rename(
            columns={
                "true_attainment_level":
                    "level_2022",
            }
        )
    )

    truth_2024 = (
        education_truth.loc[
            education_truth[
                "reference_year"
            ].eq(2024),
            [
                "person_id",
                "true_attainment_level",
            ],
        ]
        .rename(
            columns={
                "true_attainment_level":
                    "level_2024",
            }
        )
    )

    existing = truth_2022.merge(
        truth_2024,
        on="person_id",
        how="inner",
        validate="one_to_one",
    )

    difference = (
        existing["level_2024"]
        - existing["level_2022"]
    )

    assert difference.ge(0).all()
    assert difference.le(1).all()

    newly_eligible = (
        set(
            truth_2024["person_id"]
        )
        - set(
            truth_2022["person_id"]
        )
    )

    residents = population_truth.loc[
        population_truth[
            "true_resident"
        ].eq(1)
    ]

    expected_new = set(
        residents.loc[
            residents[
                "age"
            ].between(16, 17),
            "person_id",
        ]
    )

    assert newly_eligible == expected_new


def test_education_truth_progression_rates_are_plausible() -> None:
    (
        config,
        population_truth,
    ) = build_population_truth_input()

    education_truth = (
        generate_education_truth(
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
    )

    truth_2022 = (
        education_truth.loc[
            education_truth[
                "reference_year"
            ].eq(2022),
            [
                "person_id",
                "age_at_reference_year",
                "true_attainment_level",
            ],
        ]
        .rename(
            columns={
                "age_at_reference_year":
                    "age_2022",
                "true_attainment_level":
                    "level_2022",
            }
        )
    )

    truth_2024 = (
        education_truth.loc[
            education_truth[
                "reference_year"
            ].eq(2024),
            [
                "person_id",
                "age_at_reference_year",
                "true_attainment_level",
            ],
        ]
        .rename(
            columns={
                "age_at_reference_year":
                    "age_2024",
                "true_attainment_level":
                    "level_2024",
            }
        )
    )

    existing = truth_2022.merge(
        truth_2024,
        on="person_id",
        how="inner",
        validate="one_to_one",
    )

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

    current_level = existing[
        "level_2022"
    ].to_numpy(
        dtype=np.int64
    )

    next_level = np.minimum(
        current_level + 1,
        6,
    )

    can_progress = (
        (current_level < 6)
        & (
            existing[
                "age_2024"
            ].to_numpy()
            >= minimum_ages[
                next_level - 1
            ]
        )
    )

    progressed = (
        existing[
            "level_2024"
        ].to_numpy()
        > current_level
    )

    age_2022 = existing[
        "age_2022"
    ].to_numpy()

    groups = {
        "age_15_17":
            (age_2022 >= 15)
            & (age_2022 <= 17),
        "age_18_24":
            (age_2022 >= 18)
            & (age_2022 <= 24),
        "age_25_30":
            (age_2022 >= 25)
            & (age_2022 <= 30),
        "age_31_39":
            (age_2022 >= 31)
            & (age_2022 <= 39),
        "other":
            age_2022 >= 40,
    }

    expected = config[
        "education"
    ][
        "truth"
    ][
        "progression_probabilities"
    ]

    tolerance = {
        "age_15_17": 0.05,
        "age_18_24": 0.04,
        "age_25_30": 0.035,
        "age_31_39": 0.025,
        "other": 0.008,
    }

    for name, age_mask in groups.items():
        eligible = (
            age_mask
            & can_progress
        )

        assert eligible.sum() > 0

        realised = progressed[
            eligible
        ].mean()

        assert abs(
            realised
            - expected[name]
        ) < tolerance[name]


def test_education_truth_generation_is_reproducible() -> None:
    (
        config,
        population_truth,
    ) = build_population_truth_input()

    seed = config[
        "education"
    ][
        "truth_seed"
    ]

    first = generate_education_truth(
        population_truth,
        config,
        np.random.default_rng(seed),
    )

    second = generate_education_truth(
        population_truth,
        config,
        np.random.default_rng(seed),
    )

    pd.testing.assert_frame_equal(
        first,
        second,
    )
