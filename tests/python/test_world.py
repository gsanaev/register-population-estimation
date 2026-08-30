from pathlib import Path

import numpy as np
import yaml

from simulation.world import (
    age_group_from_age,
    build_age_bands,
    build_regions,
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
