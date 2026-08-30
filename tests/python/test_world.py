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
