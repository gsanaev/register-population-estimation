from pathlib import Path

import numpy as np
import pandas as pd
import yaml

from simulation.population_sources import (
    generate_population_register,
)
from simulation.world import (
    age_group_from_age,
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


def build_population_inputs():
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
        age_bands,
        population_truth,
    )


def test_population_register_schema_and_uniqueness() -> None:
    (
        config,
        age_bands,
        population_truth,
    ) = build_population_inputs()

    register = generate_population_register(
        population_truth,
        age_bands,
        config,
        np.random.default_rng(20264),
    )

    assert list(register.columns) == [
        "person_id",
        "household_id",
        "address_id",
        "region_code",
        "municipality_code",
        "sex",
        "age",
        "age_group",
        "citizenship_group",
        "registration_status",
        "registration_date",
        "last_move_date",
    ]

    assert register["person_id"].is_unique

    assert set(register["person_id"]).issubset(
        set(population_truth["person_id"])
    )

    assert not {
        "true_resident",
        "true_address_id",
        "former_address_id",
    }.intersection(register.columns)


def test_population_register_coverage_is_plausible() -> None:
    (
        config,
        age_bands,
        population_truth,
    ) = build_population_inputs()

    register = generate_population_register(
        population_truth,
        age_bands,
        config,
        np.random.default_rng(20264),
    )

    observed = population_truth[
        population_truth["person_id"].isin(
            register["person_id"]
        )
    ]

    resident_coverage = (
        observed.loc[
            observed["true_resident"] == 1
        ].shape[0]
        / 50_000
    )

    former_coverage = (
        observed.loc[
            observed["true_resident"] == 0
        ].shape[0]
        / 3_000
    )

    assert abs(
        resident_coverage - 0.98
    ) < 0.01

    assert abs(
        former_coverage - 0.80
    ) < 0.04


def test_population_register_uses_correct_location_state() -> None:
    (
        config,
        age_bands,
        population_truth,
    ) = build_population_inputs()

    register = generate_population_register(
        population_truth,
        age_bands,
        config,
        np.random.default_rng(20264),
    )

    checked = register.merge(
        population_truth,
        on="person_id",
        how="left",
        validate="one_to_one",
        suffixes=("_register", "_truth"),
    )

    current = checked[
        "true_resident"
    ] == 1

    former = checked[
        "true_resident"
    ] == 0

    assert (
        checked.loc[
            current,
            "household_id",
        ]
        == checked.loc[
            current,
            "true_household_id",
        ]
    ).all()

    assert (
        checked.loc[
            current,
            "address_id",
        ]
        == checked.loc[
            current,
            "true_address_id",
        ]
    ).all()

    assert (
        checked.loc[
            former,
            "household_id",
        ]
        == checked.loc[
            former,
            "former_household_id",
        ]
    ).all()

    assert (
        checked.loc[
            former,
            "address_id",
        ]
        == checked.loc[
            former,
            "former_address_id",
        ]
    ).all()

    assert (
        checked.loc[
            current,
            "region_code",
        ]
        == checked.loc[
            current,
            "true_region_code",
        ]
    ).all()

    assert (
        checked.loc[
            former,
            "region_code",
        ]
        == checked.loc[
            former,
            "former_region_code",
        ]
    ).all()


def test_population_register_dates_and_vocabularies() -> None:
    (
        config,
        age_bands,
        population_truth,
    ) = build_population_inputs()

    register = generate_population_register(
        population_truth,
        age_bands,
        config,
        np.random.default_rng(20264),
    )

    valid_statuses = {
        "main_residence",
        "secondary_residence",
    }

    assert set(
        register[
            "registration_status"
        ].dropna()
    ).issubset(valid_statuses)

    assert set(
        register[
            "citizenship_group"
        ].dropna()
    ).issubset(
        {
            "DE",
            "EU",
            "Non-EU",
        }
    )

    registration_start = pd.Timestamp(
        config[
            "population_register"
        ]["registration_date_start"]
    )

    registration_end = pd.Timestamp(
        config[
            "population_register"
        ]["registration_date_end"]
    )

    assert (
        register["registration_date"]
        >= registration_start
    ).all()

    assert (
        register["registration_date"]
        <= registration_end
    ).all()

    assert (
        register["last_move_date"]
        >= register["registration_date"]
    ).all()

    expected_age_groups = (
        age_group_from_age(
            register[
                "age"
            ].to_numpy(),
            age_bands,
        )
    )

    np.testing.assert_array_equal(
        register[
            "age_group"
        ].to_numpy(),
        expected_age_groups,
    )


def test_population_register_imperfections_are_plausible() -> None:
    (
        config,
        age_bands,
        population_truth,
    ) = build_population_inputs()

    register = generate_population_register(
        population_truth,
        age_bands,
        config,
        np.random.default_rng(20264),
    )

    citizenship_missing_share = (
        register[
            "citizenship_group"
        ].isna().mean()
    )

    status_missing_share = (
        register[
            "registration_status"
        ].isna().mean()
    )

    age_outlier_share = (
        register["age"] >= 97
    ).mean()

    assert abs(
        citizenship_missing_share
        - 0.005
    ) < 0.003

    assert abs(
        status_missing_share
        - 0.003
    ) < 0.002

    assert abs(
        age_outlier_share
        - 0.002
    ) < 0.0015

    nonmissing_status = (
        register[
            "registration_status"
        ].dropna()
    )

    main_share = (
        nonmissing_status
        .eq("main_residence")
        .mean()
    )

    assert abs(
        main_share - 0.93
    ) < 0.01

    assert register["age"].between(
        0,
        110,
    ).all()


def test_population_register_generation_is_reproducible() -> None:
    (
        config,
        age_bands,
        population_truth,
    ) = build_population_inputs()

    first = generate_population_register(
        population_truth,
        age_bands,
        config,
        np.random.default_rng(20264),
    )

    second = generate_population_register(
        population_truth,
        age_bands,
        config,
        np.random.default_rng(20264),
    )

    pd.testing.assert_frame_equal(
        first,
        second,
    )
