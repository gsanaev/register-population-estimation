from pathlib import Path

import numpy as np
import pandas as pd
import yaml

from simulation.world import (
    age_group_from_age,
    build_address_sampling_weights,
    build_age_bands,
    build_regions,
    generate_address_register,
    generate_former_residents,
    generate_households,
    generate_true_residents,
    sample_age,
)


REPO_ROOT = Path(__file__).resolve().parents[2]
CONFIG_PATH = REPO_ROOT / "config" / "simulation.yml"


def load_config() -> dict:
    with CONFIG_PATH.open(encoding="utf-8") as config_file:
        return yaml.safe_load(config_file)


def test_region_reference_structure() -> None:
    config = load_config()

    regions = build_regions(config)

    assert list(regions.columns) == [
        "region_code",
        "region_name",
        "urbanicity",
        "population_weight",
    ]
    assert len(regions) == 12
    assert regions["region_code"].is_unique
    assert regions["region_code"].tolist() == [
        f"R{number:02d}" for number in range(1, 13)
    ]
    assert np.isclose(regions["population_weight"].sum(), 1.0)
    assert set(regions["urbanicity"]) == {
        "urban",
        "mixed",
        "rural",
    }


def test_age_band_reference_structure() -> None:
    config = load_config()

    age_bands = build_age_bands(config)

    assert age_bands["age_band"].tolist() == [
        "0-5",
        "6-17",
        "18-24",
        "25-39",
        "40-64",
        "65-79",
        "80+",
    ]
    assert np.isclose(
        age_bands["resident_probability"].sum(),
        1.0,
    )
    assert np.isclose(
        age_bands["former_probability"].sum(),
        1.0,
    )
    assert age_bands.iloc[-1]["min_age"] == 80
    assert age_bands.iloc[-1]["max_sample_age"] == 95


def test_sample_age_is_reproducible() -> None:
    config = load_config()
    age_bands = build_age_bands(config)

    first_rng = np.random.default_rng(2026)
    second_rng = np.random.default_rng(2026)

    first_sample = sample_age(
        10_000,
        age_bands,
        "resident_probability",
        first_rng,
    )
    second_sample = sample_age(
        10_000,
        age_bands,
        "resident_probability",
        second_rng,
    )

    np.testing.assert_array_equal(
        first_sample,
        second_sample,
    )

    assert first_sample.min() >= 0
    assert first_sample.max() <= 95


def test_former_age_sampling_uses_valid_support() -> None:
    config = load_config()
    age_bands = build_age_bands(config)

    rng = np.random.default_rng(2026)

    ages = sample_age(
        5_000,
        age_bands,
        "former_probability",
        rng,
    )

    assert ages.min() >= 0
    assert ages.max() <= 95


def test_age_group_boundaries() -> None:
    config = load_config()
    age_bands = build_age_bands(config)

    ages = np.array(
        [
            0,
            5,
            6,
            17,
            18,
            24,
            25,
            39,
            40,
            64,
            65,
            79,
            80,
            95,
            100,
            110,
        ]
    )

    expected = np.array(
        [
            "0-5",
            "0-5",
            "6-17",
            "6-17",
            "18-24",
            "18-24",
            "25-39",
            "25-39",
            "40-64",
            "40-64",
            "65-79",
            "65-79",
            "80+",
            "80+",
            "80+",
            "80+",
        ],
        dtype=object,
    )

    np.testing.assert_array_equal(
        age_group_from_age(
            ages,
            age_bands,
        ),
        expected,
    )


def test_address_register_structure() -> None:
    config = load_config()
    regions = build_regions(config)

    rng = np.random.default_rng(2026)

    addresses = generate_address_register(
        regions,
        config,
        rng,
    )

    assert len(addresses) == 18_000
    assert list(addresses.columns) == [
        "address_id",
        "region_code",
        "region_name",
        "municipality_code",
        "urbanicity",
        "address_type",
    ]

    assert addresses["address_id"].is_unique
    assert addresses.iloc[0]["address_id"] == "A000001"
    assert addresses.iloc[-1]["address_id"] == "A018000"

    assert set(addresses["region_code"]).issubset(
        set(regions["region_code"])
    )

    assert set(addresses["urbanicity"]).issubset(
        {
            "urban",
            "mixed",
            "rural",
        }
    )

    assert set(addresses["address_type"]).issubset(
        {
            "single_family",
            "multi_family",
            "large_residential",
        }
    )

    assert addresses["municipality_code"].str.match(
        r"^R\d{2}-M0[1-4]$"
    ).all()


def test_address_geography_matches_region_reference() -> None:
    config = load_config()
    regions = build_regions(config)

    rng = np.random.default_rng(2026)

    addresses = generate_address_register(
        regions,
        config,
        rng,
    )

    expected = addresses[
        [
            "address_id",
            "region_code",
            "region_name",
            "urbanicity",
        ]
    ].merge(
        regions[
            [
                "region_code",
                "region_name",
                "urbanicity",
            ]
        ],
        on="region_code",
        how="left",
        suffixes=("_address", "_reference"),
        validate="many_to_one",
    )

    assert (
        expected["region_name_address"]
        == expected["region_name_reference"]
    ).all()

    assert (
        expected["urbanicity_address"]
        == expected["urbanicity_reference"]
    ).all()


def test_address_generation_is_reproducible() -> None:
    config = load_config()
    regions = build_regions(config)

    first_rng = np.random.default_rng(2026)
    second_rng = np.random.default_rng(2026)

    first = generate_address_register(
        regions,
        config,
        first_rng,
    )

    second = generate_address_register(
        regions,
        config,
        second_rng,
    )

    pd.testing.assert_frame_equal(
        first,
        second,
    )


def test_address_sampling_weights_match_address_type() -> None:
    config = load_config()

    addresses = pd.DataFrame(
        {
            "address_type": [
                "single_family",
                "multi_family",
                "large_residential",
            ]
        }
    )

    weights = build_address_sampling_weights(
        addresses,
        config,
    )

    np.testing.assert_array_equal(
        weights,
        np.array(
            [
                1.0,
                2.5,
                6.0,
            ]
        ),
    )


def test_households_reconcile_to_true_resident_total() -> None:
    config = load_config()
    regions = build_regions(config)

    address_rng = np.random.default_rng(2026)
    addresses = generate_address_register(
        regions,
        config,
        address_rng,
    )

    household_rng = np.random.default_rng(20261)
    households = generate_households(
        config["population"]["n_true_residents"],
        addresses,
        config,
        household_rng,
    )

    assert households["household_id"].is_unique

    assert (
        households["household_size"].sum()
        == config["population"]["n_true_residents"]
    )

    assert households["household_size"].between(
        1,
        6,
    ).all()

    assert households.iloc[0]["household_id"] == "H000001"

    expected_last_id = (
        f"H{len(households):06d}"
    )

    assert (
        households.iloc[-1]["household_id"]
        == expected_last_id
    )


def test_household_addresses_exist_and_geography_matches() -> None:
    config = load_config()
    regions = build_regions(config)

    address_rng = np.random.default_rng(2026)
    addresses = generate_address_register(
        regions,
        config,
        address_rng,
    )

    household_rng = np.random.default_rng(20261)
    households = generate_households(
        5_000,
        addresses,
        config,
        household_rng,
    )

    assert set(households["address_id"]).issubset(
        set(addresses["address_id"])
    )

    checked = households.merge(
        addresses[
            [
                "address_id",
                "region_code",
                "municipality_code",
            ]
        ],
        on="address_id",
        how="left",
        suffixes=("_household", "_address"),
        validate="many_to_one",
    )

    assert (
        checked["region_code_household"]
        == checked["region_code_address"]
    ).all()

    assert (
        checked["municipality_code_household"]
        == checked["municipality_code_address"]
    ).all()


def test_household_generation_is_reproducible() -> None:
    config = load_config()
    regions = build_regions(config)

    address_rng = np.random.default_rng(2026)
    addresses = generate_address_register(
        regions,
        config,
        address_rng,
    )

    first_rng = np.random.default_rng(20261)
    second_rng = np.random.default_rng(20261)

    first = generate_households(
        10_000,
        addresses,
        config,
        first_rng,
    )

    second = generate_households(
        10_000,
        addresses,
        config,
        second_rng,
    )

    pd.testing.assert_frame_equal(
        first,
        second,
    )


def test_household_size_distribution_is_plausible() -> None:
    config = load_config()
    regions = build_regions(config)

    address_rng = np.random.default_rng(2026)
    addresses = generate_address_register(
        regions,
        config,
        address_rng,
    )

    household_rng = np.random.default_rng(20261)
    households = generate_households(
        50_000,
        addresses,
        config,
        household_rng,
    )

    realised = (
        households["household_size"]
        .value_counts(normalize=True)
        .sort_index()
    )

    expected = config[
        "households"
    ]["size_probabilities"]

    for size, probability in expected.items():
        assert abs(
            realised.get(size, 0.0)
            - probability
        ) < 0.02


def test_true_resident_population_structure() -> None:
    config = load_config()
    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        config["population"]["n_true_residents"],
        addresses,
        config,
        np.random.default_rng(20261),
    )

    residents = generate_true_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20262),
    )

    assert len(residents) == 50_000
    assert residents["person_id"].is_unique

    assert residents.iloc[0]["person_id"] == "P000001"
    assert residents.iloc[-1]["person_id"] == "P050000"

    assert (residents["true_resident"] == 1).all()

    assert residents[
        [
            "true_household_id",
            "true_address_id",
            "true_region_code",
            "true_municipality_code",
        ]
    ].notna().all().all()

    assert residents[
        [
            "former_household_id",
            "former_address_id",
            "former_region_code",
            "former_municipality_code",
            "departure_date_true",
        ]
    ].isna().all().all()


def test_true_resident_household_assignments_reconcile() -> None:
    config = load_config()
    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        50_000,
        addresses,
        config,
        np.random.default_rng(20261),
    )

    residents = generate_true_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20262),
    )

    realised_counts = (
        residents["true_household_id"]
        .value_counts()
        .sort_index()
    )

    expected_counts = (
        households
        .set_index("household_id")[
            "household_size"
        ]
        .sort_index()
    )

    pd.testing.assert_series_equal(
        realised_counts,
        expected_counts,
        check_names=False,
    )

    geography = residents.merge(
        households[
            [
                "household_id",
                "address_id",
                "region_code",
                "municipality_code",
            ]
        ],
        left_on="true_household_id",
        right_on="household_id",
        how="left",
        validate="many_to_one",
        suffixes=("_resident", "_household"),
    )

    assert (
        geography["true_address_id"]
        == geography["address_id"]
    ).all()

    assert (
        geography["true_region_code"]
        == geography["region_code"]
    ).all()

    assert (
        geography["true_municipality_code"]
        == geography["municipality_code"]
    ).all()


def test_true_resident_demographics_are_plausible() -> None:
    config = load_config()
    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        50_000,
        addresses,
        config,
        np.random.default_rng(20261),
    )

    residents = generate_true_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20262),
    )

    assert set(residents["sex"]) == {
        "F",
        "M",
    }

    assert set(
        residents["citizenship_group"]
    ) == {
        "DE",
        "EU",
        "Non-EU",
    }

    assert residents["age"].between(
        0,
        95,
    ).all()

    expected_age_group = age_group_from_age(
        residents["age"].to_numpy(),
        age_bands,
    )

    np.testing.assert_array_equal(
        residents["age_group"].to_numpy(),
        expected_age_group,
    )

    sex_shares = residents[
        "sex"
    ].value_counts(normalize=True)

    assert abs(
        sex_shares["F"] - 0.50
    ) < 0.02

    citizenship_shares = residents[
        "citizenship_group"
    ].value_counts(normalize=True)

    expected_citizenship = config[
        "demographics"
    ]["resident_citizenship_probabilities"]

    for category, probability in (
        expected_citizenship.items()
    ):
        assert abs(
            citizenship_shares[category]
            - probability
        ) < 0.02


def test_true_resident_generation_is_reproducible() -> None:
    config = load_config()
    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        50_000,
        addresses,
        config,
        np.random.default_rng(20261),
    )

    first = generate_true_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20262),
    )

    second = generate_true_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20262),
    )

    pd.testing.assert_frame_equal(
        first,
        second,
    )


def test_former_resident_population_structure() -> None:
    config = load_config()
    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        config["population"]["n_true_residents"],
        addresses,
        config,
        np.random.default_rng(20261),
    )

    former = generate_former_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20263),
    )

    assert len(former) == 3_000
    assert former["person_id"].is_unique

    assert former.iloc[0]["person_id"] == "P050001"
    assert former.iloc[-1]["person_id"] == "P053000"

    assert (former["true_resident"] == 0).all()

    assert former[
        [
            "true_household_id",
            "true_address_id",
            "true_region_code",
            "true_municipality_code",
        ]
    ].isna().all().all()

    assert former[
        [
            "former_household_id",
            "former_address_id",
            "former_region_code",
            "former_municipality_code",
            "departure_date_true",
        ]
    ].notna().all().all()


def test_former_resident_geography_matches_households() -> None:
    config = load_config()
    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        50_000,
        addresses,
        config,
        np.random.default_rng(20261),
    )

    former = generate_former_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20263),
    )

    checked = former.merge(
        households[
            [
                "household_id",
                "address_id",
                "region_code",
                "municipality_code",
            ]
        ],
        left_on="former_household_id",
        right_on="household_id",
        how="left",
        validate="many_to_one",
    )

    assert (
        checked["former_address_id"]
        == checked["address_id"]
    ).all()

    assert (
        checked["former_region_code"]
        == checked["region_code"]
    ).all()

    assert (
        checked["former_municipality_code"]
        == checked["municipality_code"]
    ).all()


def test_former_resident_demographics_and_dates() -> None:
    config = load_config()
    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        50_000,
        addresses,
        config,
        np.random.default_rng(20261),
    )

    former = generate_former_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20263),
    )

    assert former["age"].between(
        0,
        95,
    ).all()

    expected_age_group = age_group_from_age(
        former["age"].to_numpy(),
        age_bands,
    )

    np.testing.assert_array_equal(
        former["age_group"].to_numpy(),
        expected_age_group,
    )

    sex_shares = former[
        "sex"
    ].value_counts(normalize=True)

    assert abs(
        sex_shares["F"] - 0.50
    ) < 0.04

    citizenship_shares = former[
        "citizenship_group"
    ].value_counts(normalize=True)

    expected_citizenship = config[
        "demographics"
    ]["former_citizenship_probabilities"]

    for category, probability in (
        expected_citizenship.items()
    ):
        assert abs(
            citizenship_shares[category]
            - probability
        ) < 0.04

    start = pd.Timestamp(
        config[
            "former_residents"
        ]["departure_date_start"]
    )
    end = pd.Timestamp(
        config[
            "former_residents"
        ]["departure_date_end"]
    )

    assert (
        former["departure_date_true"] >= start
    ).all()

    assert (
        former["departure_date_true"] <= end
    ).all()


def test_former_resident_age_distribution_is_plausible() -> None:
    config = load_config()
    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        50_000,
        addresses,
        config,
        np.random.default_rng(20261),
    )

    former = generate_former_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20263),
    )

    realised = (
        former["age_group"]
        .value_counts(normalize=True)
        .reindex(
            age_bands["age_band"]
        )
    )

    expected = age_bands.set_index(
        "age_band"
    )["former_probability"]

    for age_band in age_bands["age_band"]:
        assert abs(
            realised[age_band]
            - expected[age_band]
        ) < 0.03


def test_former_resident_generation_is_reproducible() -> None:
    config = load_config()
    regions = build_regions(config)
    age_bands = build_age_bands(config)

    addresses = generate_address_register(
        regions,
        config,
        np.random.default_rng(2026),
    )

    households = generate_households(
        50_000,
        addresses,
        config,
        np.random.default_rng(20261),
    )

    first = generate_former_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20263),
    )

    second = generate_former_residents(
        households,
        age_bands,
        config,
        np.random.default_rng(20263),
    )

    pd.testing.assert_frame_equal(
        first,
        second,
    )
