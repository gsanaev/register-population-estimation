from pathlib import Path

import numpy as np
import pandas as pd
import yaml

from simulation.population_sources import (
    generate_employment_register,
    generate_population_register,
    generate_tax_register,
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
        addresses,
        population_truth,
    )


def test_population_register_schema_and_uniqueness() -> None:
    (
        config,
        age_bands,
        _,
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
        _,
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
        _,
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
        _,
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
        _,
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
        _,
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


def test_employment_register_schema_and_uniqueness() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    employment = generate_employment_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20265),
        np.random.default_rng(202651),
    )

    assert list(employment.columns) == [
        "person_id",
        "ref_date",
        "employment_status",
        "days_employed_last_12m",
        "annual_employment_income",
        "contact_address_id",
    ]

    assert employment["person_id"].is_unique

    assert set(employment["person_id"]).issubset(
        set(population_truth["person_id"])
    )

    assert set(
        employment[
            "employment_status"
        ]
    ).issubset(
        {
            "employed",
            "marginal",
            "self_employed",
            "no_record",
        }
    )


def test_employment_source_presence_differs_from_signal() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    employment = generate_employment_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20265),
        np.random.default_rng(202651),
    )

    no_record = employment[
        "employment_status"
    ].eq("no_record")

    active = ~no_record

    assert no_record.any()
    assert active.any()

    assert (
        employment.loc[
            no_record,
            "days_employed_last_12m",
        ]
        .dropna()
        .eq(0)
        .all()
    )

    assert (
        employment.loc[
            no_record,
            "annual_employment_income",
        ]
        .dropna()
        .eq(0)
        .all()
    )

    assert (
        employment.loc[
            active,
            "days_employed_last_12m",
        ]
        .dropna()
        .gt(0)
        .all()
    )

    assert (
        employment.loc[
            active,
            "annual_employment_income",
        ]
        .dropna()
        .gt(0)
        .all()
    )


def test_employment_activity_rates_are_plausible() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    employment = generate_employment_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20265),
        np.random.default_rng(202651),
    )

    active_ids = set(
        employment.loc[
            employment[
                "employment_status"
            ].ne("no_record"),
            "person_id",
        ]
    )

    truth = population_truth.copy()

    truth["active"] = truth[
        "person_id"
    ].isin(active_ids)

    groups = {
        "resident_18_24": (
            (truth["true_resident"] == 1)
            & truth["age"].between(18, 24)
        ),
        "resident_25_39": (
            (truth["true_resident"] == 1)
            & truth["age"].between(25, 39)
        ),
        "resident_40_64": (
            (truth["true_resident"] == 1)
            & truth["age"].between(40, 64)
        ),
        "resident_65_67": (
            (truth["true_resident"] == 1)
            & truth["age"].between(65, 67)
        ),
        "nonresident_18_67": (
            (truth["true_resident"] == 0)
            & truth["age"].between(18, 67)
        ),
    }

    expected = {
        "resident_18_24": 0.48,
        "resident_25_39": 0.78,
        "resident_40_64": 0.73,
        "resident_65_67": 0.18,
        "nonresident_18_67": 0.05,
    }

    tolerance = {
        "resident_18_24": 0.03,
        "resident_25_39": 0.02,
        "resident_40_64": 0.02,
        "resident_65_67": 0.05,
        "nonresident_18_67": 0.025,
    }

    for name, mask in groups.items():
        realised = truth.loc[
            mask,
            "active",
        ].mean()

        assert abs(
            realised
            - expected[name]
        ) < tolerance[name]

    outside_eligible = ~truth[
        "age"
    ].between(18, 67)

    assert not truth.loc[
        outside_eligible,
        "active",
    ].any()


def test_employment_status_and_amounts_are_plausible() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    employment = generate_employment_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20265),
        np.random.default_rng(202651),
    )

    active = employment[
        employment[
            "employment_status"
        ].ne("no_record")
    ]

    status_shares = (
        active[
            "employment_status"
        ]
        .value_counts(normalize=True)
    )

    assert abs(
        status_shares["employed"]
        - 0.78
    ) < 0.02

    assert abs(
        status_shares["marginal"]
        - 0.12
    ) < 0.015

    assert abs(
        status_shares["self_employed"]
        - 0.10
    ) < 0.015

    limits = {
        "employed": (20, 365),
        "marginal": (5, 250),
        "self_employed": (30, 365),
    }

    for status, (
        minimum,
        maximum,
    ) in limits.items():
        values = active.loc[
            active[
                "employment_status"
            ].eq(status),
            "days_employed_last_12m",
        ].dropna()

        assert values.between(
            minimum,
            maximum,
        ).all()

    assert abs(
        employment[
            "days_employed_last_12m"
        ].isna().mean()
        - 0.01
    ) < 0.003

    assert abs(
        employment[
            "annual_employment_income"
        ].isna().mean()
        - 0.01
    ) < 0.003


def test_employment_contact_addresses_are_valid() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    employment = generate_employment_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20265),
        np.random.default_rng(202651),
    )

    valid_addresses = set(
        addresses["address_id"]
    )

    nonmissing_contacts = employment[
        "contact_address_id"
    ].dropna()

    assert set(
        nonmissing_contacts
    ).issubset(valid_addresses)

    missing_share = employment[
        "contact_address_id"
    ].isna().mean()

    assert abs(
        missing_share - 0.03
    ) < 0.005


def test_employment_generation_is_reproducible() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    first = generate_employment_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20265),
        np.random.default_rng(202651),
    )

    second = generate_employment_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20265),
        np.random.default_rng(202651),
    )

    pd.testing.assert_frame_equal(
        first,
        second,
    )


def test_tax_register_schema_and_uniqueness() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    tax = generate_tax_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20266),
        np.random.default_rng(202661),
    )

    assert list(tax.columns) == [
        "person_id",
        "tax_year",
        "tax_filing_flag",
        "taxable_income",
        "contact_address_id",
    ]

    assert tax["person_id"].is_unique

    assert set(tax["person_id"]).issubset(
        set(population_truth["person_id"])
    )

    assert set(
        tax["tax_filing_flag"]
    ).issubset(
        {
            0,
            1,
        }
    )

    assert (
        tax["tax_year"] == 2025
    ).all()


def test_tax_source_presence_differs_from_filing_signal() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    tax = generate_tax_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20266),
        np.random.default_rng(202661),
    )

    filers = tax[
        "tax_filing_flag"
    ].eq(1)

    nonfilers = ~filers

    assert filers.any()
    assert nonfilers.any()

    nonfiler_income = tax.loc[
        nonfilers,
        "taxable_income",
    ].dropna()

    assert (
        nonfiler_income <= 0
    ).all()


def test_tax_filing_rates_are_plausible() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    tax = generate_tax_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20266),
        np.random.default_rng(202661),
    )

    filer_ids = set(
        tax.loc[
            tax[
                "tax_filing_flag"
            ].eq(1),
            "person_id",
        ]
    )

    truth = population_truth.copy()

    truth["filer"] = truth[
        "person_id"
    ].isin(filer_ids)

    groups = {
        "resident_18_24": (
            (truth["true_resident"] == 1)
            & truth["age"].between(18, 24)
        ),
        "resident_25_39": (
            (truth["true_resident"] == 1)
            & truth["age"].between(25, 39)
        ),
        "resident_40_64": (
            (truth["true_resident"] == 1)
            & truth["age"].between(40, 64)
        ),
        "resident_65_79": (
            (truth["true_resident"] == 1)
            & truth["age"].between(65, 79)
        ),
        "resident_80_plus": (
            (truth["true_resident"] == 1)
            & truth["age"].ge(80)
        ),
        "nonresident": (
            truth["true_resident"] == 0
        ),
    }

    expected = {
        "resident_18_24": 0.22,
        "resident_25_39": 0.62,
        "resident_40_64": 0.68,
        "resident_65_79": 0.28,
        "resident_80_plus": 0.12,
        "nonresident": 0.04,
    }

    tolerance = {
        "resident_18_24": 0.03,
        "resident_25_39": 0.02,
        "resident_40_64": 0.02,
        "resident_65_79": 0.025,
        "resident_80_plus": 0.035,
        "nonresident": 0.02,
    }

    for name, mask in groups.items():
        realised = truth.loc[
            mask,
            "filer",
        ].mean()

        assert abs(
            realised
            - expected[name]
        ) < tolerance[name]


def test_tax_income_imperfections_are_plausible() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    tax = generate_tax_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20266),
        np.random.default_rng(202661),
    )

    missing_share = (
        tax[
            "taxable_income"
        ].isna().mean()
    )

    assert abs(
        missing_share - 0.01
    ) < 0.003

    nonmissing_income = tax[
        "taxable_income"
    ].dropna()

    negative_share = (
        nonmissing_income < 0
    ).mean()

    assert abs(
        negative_share - 0.005
    ) < 0.002

    checked = tax.merge(
        population_truth[
            [
                "person_id",
                "age",
            ]
        ],
        on="person_id",
        how="left",
        validate="one_to_one",
    )

    adult_filers = checked.loc[
        checked[
            "tax_filing_flag"
        ].eq(1)
        & checked["age"].ge(18),
        "taxable_income",
    ].dropna()

    under_18_filers = checked.loc[
        checked[
            "tax_filing_flag"
        ].eq(1)
        & checked["age"].lt(18),
        "taxable_income",
    ].dropna()

    assert (
        adult_filers != 0
    ).all()

    assert (
        under_18_filers == 0
    ).all()


def test_tax_contact_addresses_are_valid() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    tax = generate_tax_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20266),
        np.random.default_rng(202661),
    )

    valid_addresses = set(
        addresses["address_id"]
    )

    assert set(
        tax[
            "contact_address_id"
        ].dropna()
    ).issubset(valid_addresses)

    missing_share = tax[
        "contact_address_id"
    ].isna().mean()

    assert abs(
        missing_share - 0.03
    ) < 0.006


def test_tax_generation_is_reproducible() -> None:
    (
        config,
        _,
        addresses,
        population_truth,
    ) = build_population_inputs()

    first = generate_tax_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20266),
        np.random.default_rng(202661),
    )

    second = generate_tax_register(
        population_truth,
        addresses,
        config,
        np.random.default_rng(20266),
        np.random.default_rng(202661),
    )

    pd.testing.assert_frame_equal(
        first,
        second,
    )
