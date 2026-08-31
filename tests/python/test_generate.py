from pathlib import Path

import numpy as np
import pytest

from simulation.generate import (
    DEFAULT_CONFIG_PATH,
    OUTPUT_PATHS,
    POPULATION_RNG_COMPONENT_IDS,
    load_config,
    population_rng,
)


EXPECTED_OUTPUT_PATHS = {
    "population_truth":
        Path(
            "data/raw/synthetic_population_truth.csv"
        ),
    "address_register":
        Path(
            "data/raw/address_register.csv"
        ),
    "population_register":
        Path(
            "data/raw/population_register.csv"
        ),
    "employment_register":
        Path(
            "data/raw/employment_register.csv"
        ),
    "tax_register":
        Path(
            "data/raw/tax_register.csv"
        ),
    "education_register":
        Path(
            "data/raw/education_register.csv"
        ),
    "education_truth":
        Path(
            "data/education/raw/"
            "synthetic_education_truth.csv"
        ),
    "zensus_2022":
        Path(
            "data/education/raw/"
            "zensus_2022_like_delivery.csv"
        ),
    "ba_2024":
        Path(
            "data/education/raw/"
            "ba_2024_like_delivery.csv"
        ),
    "mikrozensus_2024":
        Path(
            "data/education/raw/"
            "mikrozensus_2024_like_delivery.csv"
        ),
}

EXPECTED_COMPONENT_IDS = {
    "address_register": 1,
    "households": 2,
    "true_residents": 3,
    "former_residents": 4,
    "population_register": 5,
    "employment_register": 6,
    "employment_contact_address": 7,
    "tax_register": 8,
    "tax_contact_address": 9,
    "education_register": 10,
    "education_contact_address": 11,
}


def test_default_config_path_exists() -> None:
    assert DEFAULT_CONFIG_PATH.is_file()


def test_load_config_reads_population_seed() -> None:
    config = load_config()

    assert (
        config[
            "simulation"
        ][
            "population_seed"
        ]
        == 2026
    )


def test_load_config_rejects_non_mapping(
    tmp_path: Path,
) -> None:
    path = (
        tmp_path
        / "simulation.yml"
    )

    path.write_text(
        "- invalid\n"
        "- configuration\n",
        encoding="utf-8",
    )

    with pytest.raises(
        ValueError,
        match="must be a mapping",
    ):
        load_config(path)


def test_load_config_rejects_invalid_population_seed(
    tmp_path: Path,
) -> None:
    path = (
        tmp_path
        / "simulation.yml"
    )

    path.write_text(
        "simulation:\n"
        "  population_seed: invalid\n",
        encoding="utf-8",
    )

    with pytest.raises(
        ValueError,
        match="population_seed",
    ):
        load_config(path)


def test_output_paths_match_existing_contract() -> None:
    assert OUTPUT_PATHS == EXPECTED_OUTPUT_PATHS


def test_population_rng_component_ids_are_stable() -> None:
    assert (
        POPULATION_RNG_COMPONENT_IDS
        == EXPECTED_COMPONENT_IDS
    )


def test_population_rng_is_reproducible() -> None:
    config = load_config()

    first = population_rng(
        config,
        "true_residents",
    ).random(20)

    second = population_rng(
        config,
        "true_residents",
    ).random(20)

    np.testing.assert_array_equal(
        first,
        second,
    )


def test_population_rng_components_are_independent() -> None:
    config = load_config()

    address_draws = population_rng(
        config,
        "address_register",
    ).random(20)

    household_draws = population_rng(
        config,
        "households",
    ).random(20)

    assert not np.array_equal(
        address_draws,
        household_draws,
    )


def test_population_rng_rejects_unknown_component() -> None:
    config = load_config()

    with pytest.raises(
        ValueError,
        match="Unknown population RNG component",
    ):
        population_rng(
            config,
            "not_a_component",
        )
