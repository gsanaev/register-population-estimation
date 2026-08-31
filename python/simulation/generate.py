"""Orchestrate synthetic population and education data generation."""

from collections.abc import Mapping
from pathlib import Path
from typing import Any

import numpy as np
import yaml

from .world import (
    build_age_bands,
    build_population_truth,
    build_regions,
    generate_address_register,
    generate_former_residents,
    generate_households,
    generate_true_residents,
)


REPO_ROOT = Path(__file__).resolve().parents[2]

DEFAULT_CONFIG_PATH = (
    REPO_ROOT
    / "config"
    / "simulation.yml"
)

OUTPUT_PATHS = {
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

POPULATION_RNG_COMPONENT_IDS = {
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


def load_config(
    path: Path | str = DEFAULT_CONFIG_PATH,
) -> dict[str, Any]:
    """Load and minimally validate the simulation configuration."""

    config_path = Path(path)

    with config_path.open(
        encoding="utf-8"
    ) as config_file:
        loaded = yaml.safe_load(
            config_file
        )

    if not isinstance(
        loaded,
        Mapping,
    ):
        raise ValueError(
            "Simulation configuration must be a mapping."
        )

    config = dict(
        loaded
    )

    simulation_config = config.get(
        "simulation"
    )

    if not isinstance(
        simulation_config,
        Mapping,
    ):
        raise ValueError(
            "Simulation configuration must contain "
            "a simulation section."
        )

    population_seed = (
        simulation_config.get(
            "population_seed"
        )
    )

    if (
        isinstance(
            population_seed,
            bool,
        )
        or not isinstance(
            population_seed,
            int,
        )
        or population_seed < 0
    ):
        raise ValueError(
            "simulation.population_seed must be "
            "a non-negative integer."
        )

    return config


def population_rng(
    config: Mapping[str, Any],
    component: str,
) -> np.random.Generator:
    """Create one stable independent population simulation RNG."""

    if component not in POPULATION_RNG_COMPONENT_IDS:
        raise ValueError(
            f"Unknown population RNG component: {component}"
        )

    simulation_config = config.get(
        "simulation"
    )

    if not isinstance(
        simulation_config,
        Mapping,
    ):
        raise ValueError(
            "Configuration must contain a simulation section."
        )

    population_seed = (
        simulation_config.get(
            "population_seed"
        )
    )

    if (
        isinstance(
            population_seed,
            bool,
        )
        or not isinstance(
            population_seed,
            int,
        )
        or population_seed < 0
    ):
        raise ValueError(
            "simulation.population_seed must be "
            "a non-negative integer."
        )

    component_id = (
        POPULATION_RNG_COMPONENT_IDS[
            component
        ]
    )

    seed_sequence = np.random.SeedSequence(
        [
            population_seed,
            component_id,
        ]
    )

    return np.random.default_rng(
        seed_sequence
    )


def generate_population_world(
    config: Mapping[str, Any],
) -> dict[str, Any]:
    """Generate the complete hidden synthetic population world in memory."""

    population_config = config.get(
        "population"
    )

    if not isinstance(
        population_config,
        Mapping,
    ):
        raise ValueError(
            "Configuration must contain a population section."
        )

    n_true_residents = (
        population_config.get(
            "n_true_residents"
        )
    )

    if (
        isinstance(
            n_true_residents,
            bool,
        )
        or not isinstance(
            n_true_residents,
            int,
        )
        or n_true_residents <= 0
    ):
        raise ValueError(
            "population.n_true_residents must be "
            "a positive integer."
        )

    regions = build_regions(
        config
    )

    age_bands = build_age_bands(
        config
    )

    address_register = (
        generate_address_register(
            regions,
            config,
            population_rng(
                config,
                "address_register",
            ),
        )
    )

    households = generate_households(
        n_true_residents,
        address_register,
        config,
        population_rng(
            config,
            "households",
        ),
    )

    true_residents = (
        generate_true_residents(
            households,
            age_bands,
            config,
            population_rng(
                config,
                "true_residents",
            ),
        )
    )

    former_residents = (
        generate_former_residents(
            households,
            age_bands,
            config,
            population_rng(
                config,
                "former_residents",
            ),
        )
    )

    population_truth = (
        build_population_truth(
            true_residents,
            former_residents,
            config,
        )
    )

    return {
        "regions": regions,
        "age_bands": age_bands,
        "address_register":
            address_register,
        "households":
            households,
        "true_residents":
            true_residents,
        "former_residents":
            former_residents,
        "population_truth":
            population_truth,
    }
