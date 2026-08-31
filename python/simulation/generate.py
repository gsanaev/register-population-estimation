"""Orchestrate synthetic population and education data generation."""

from collections.abc import Mapping
from pathlib import Path
from typing import Any

import numpy as np
import yaml

from .education_sources import (
    generate_ba_2024_delivery,
    generate_mikrozensus_2024_delivery,
    generate_zensus_2022_delivery,
)
from .education_truth import (
    generate_education_truth,
)
from .population_sources import (
    generate_education_register,
    generate_employment_register,
    generate_population_register,
    generate_tax_register,
)
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


def generate_population_sources(
    config: Mapping[str, Any],
    population_world: Mapping[str, Any],
) -> dict[str, Any]:
    """Generate all observable population-side sources in memory."""

    required_world_keys = {
        "age_bands",
        "address_register",
        "population_truth",
    }

    missing_keys = (
        required_world_keys
        - set(population_world)
    )

    if missing_keys:
        raise ValueError(
            "Population world is missing source-generation fields: "
            + ", ".join(
                sorted(missing_keys)
            )
        )

    age_bands = population_world[
        "age_bands"
    ]

    address_register = population_world[
        "address_register"
    ]

    population_truth = population_world[
        "population_truth"
    ]

    population_register = (
        generate_population_register(
            population_truth,
            age_bands,
            config,
            population_rng(
                config,
                "population_register",
            ),
        )
    )

    employment_register = (
        generate_employment_register(
            population_truth,
            address_register,
            config,
            population_rng(
                config,
                "employment_register",
            ),
            population_rng(
                config,
                "employment_contact_address",
            ),
        )
    )

    tax_register = (
        generate_tax_register(
            population_truth,
            address_register,
            config,
            population_rng(
                config,
                "tax_register",
            ),
            population_rng(
                config,
                "tax_contact_address",
            ),
        )
    )

    education_register = (
        generate_education_register(
            population_truth,
            address_register,
            config,
            population_rng(
                config,
                "education_register",
            ),
            population_rng(
                config,
                "education_contact_address",
            ),
        )
    )

    return {
        "population_register":
            population_register,
        "employment_register":
            employment_register,
        "tax_register":
            tax_register,
        "education_register":
            education_register,
    }


def education_rng(
    config: Mapping[str, Any],
    component: str,
) -> np.random.Generator:
    """Create a configured RNG for one education simulation component."""

    education_config = config.get(
        "education"
    )

    if not isinstance(
        education_config,
        Mapping,
    ):
        raise ValueError(
            "Configuration must contain an education section."
        )

    if component == "education_truth":
        seed = education_config.get(
            "truth_seed"
        )
    elif component in {
        "zensus_2022",
        "ba_2024",
        "mikrozensus_2024",
    }:
        source_config = education_config.get(
            component
        )

        if not isinstance(
            source_config,
            Mapping,
        ):
            raise ValueError(
                f"education must contain a {component} section."
            )

        seed = source_config.get(
            "seed"
        )
    else:
        raise ValueError(
            f"Unknown education RNG component: {component}"
        )

    if (
        isinstance(
            seed,
            bool,
        )
        or not isinstance(
            seed,
            int,
        )
        or seed < 0
    ):
        raise ValueError(
            f"Seed for {component} must be "
            "a non-negative integer."
        )

    return np.random.default_rng(
        seed
    )


def generate_education_attainment_sources(
    config: Mapping[str, Any],
    population_world: Mapping[str, Any],
) -> dict[str, Any]:
    """Generate hidden attainment truth and all raw attainment sources."""

    if "population_truth" not in population_world:
        raise ValueError(
            "Population world is missing population_truth."
        )

    population_truth = population_world[
        "population_truth"
    ]

    education_truth = (
        generate_education_truth(
            population_truth,
            config,
            education_rng(
                config,
                "education_truth",
            ),
        )
    )

    zensus_2022 = (
        generate_zensus_2022_delivery(
            education_truth,
            config,
            education_rng(
                config,
                "zensus_2022",
            ),
        )
    )

    ba_2024 = (
        generate_ba_2024_delivery(
            education_truth,
            config,
            education_rng(
                config,
                "ba_2024",
            ),
        )
    )

    mikrozensus_2024 = (
        generate_mikrozensus_2024_delivery(
            education_truth,
            config,
            education_rng(
                config,
                "mikrozensus_2024",
            ),
        )
    )

    return {
        "education_truth":
            education_truth,
        "zensus_2022":
            zensus_2022,
        "ba_2024":
            ba_2024,
        "mikrozensus_2024":
            mikrozensus_2024,
    }


def generate_simulation_outputs(
    config: Mapping[str, Any],
) -> dict[str, Any]:
    """Generate the ten final simulation outputs in memory."""

    population_world = (
        generate_population_world(
            config
        )
    )

    population_sources = (
        generate_population_sources(
            config,
            population_world,
        )
    )

    education_sources = (
        generate_education_attainment_sources(
            config,
            population_world,
        )
    )

    persisted_population_truth = (
        build_persisted_population_truth(
            population_world[
                "population_truth"
            ],
            population_sources[
                "population_register"
            ],
        )
    )

    outputs = {
        "population_truth":
            persisted_population_truth,
        "address_register":
            population_world[
                "address_register"
            ],
        "population_register":
            population_sources[
                "population_register"
            ],
        "employment_register":
            population_sources[
                "employment_register"
            ],
        "tax_register":
            population_sources[
                "tax_register"
            ],
        "education_register":
            population_sources[
                "education_register"
            ],
        "education_truth":
            education_sources[
                "education_truth"
            ],
        "zensus_2022":
            education_sources[
                "zensus_2022"
            ],
        "ba_2024":
            education_sources[
                "ba_2024"
            ],
        "mikrozensus_2024":
            education_sources[
                "mikrozensus_2024"
            ],
    }

    if set(outputs) != set(
        OUTPUT_PATHS
    ):
        raise RuntimeError(
            "Simulation outputs do not match "
            "the persistence contract."
        )

    return outputs


def write_simulation_outputs(
    outputs: Mapping[str, Any],
    output_root: Path | str,
) -> dict[str, Path]:
    """Write simulation outputs beneath a caller-supplied output root."""

    root = Path(
        output_root
    ).resolve()

    if root == REPO_ROOT.resolve():
        raise ValueError(
            "Repository-root persistence is disabled "
            "during migration validation."
        )

    missing_outputs = (
        set(OUTPUT_PATHS)
        - set(outputs)
    )

    extra_outputs = (
        set(outputs)
        - set(OUTPUT_PATHS)
    )

    if missing_outputs or extra_outputs:
        details = []

        if missing_outputs:
            details.append(
                "missing: "
                + ", ".join(
                    sorted(
                        missing_outputs
                    )
                )
            )

        if extra_outputs:
            details.append(
                "unexpected: "
                + ", ".join(
                    sorted(
                        extra_outputs
                    )
                )
            )

        raise ValueError(
            "Simulation outputs do not match "
            "the persistence contract ("
            + "; ".join(details)
            + ")."
        )

    written_paths: dict[
        str,
        Path,
    ] = {}

    for name, relative_path in (
        OUTPUT_PATHS.items()
    ):
        output_path = (
            root
            / relative_path
        )

        output_path.parent.mkdir(
            parents=True,
            exist_ok=True,
        )

        frame = outputs[
            name
        ]

        if not hasattr(
            frame,
            "to_csv",
        ):
            raise TypeError(
                f"Simulation output {name} "
                "is not tabular."
            )

        frame.to_csv(
            output_path,
            index=False,
        )

        written_paths[
            name
        ] = output_path

    return written_paths


def build_persisted_population_truth(
    population_truth: Any,
    population_register: Any,
) -> Any:
    """Add evaluation-only population-register truth fields."""

    persisted_truth = population_truth.copy()

    registered_ids = set(
        population_register[
            "person_id"
        ]
    )

    persisted_truth[
        "in_population_register"
    ] = (
        persisted_truth[
            "person_id"
        ]
        .isin(
            registered_ids
        )
        .astype(int)
    )

    true_resident = persisted_truth[
        "true_resident"
    ].eq(1)

    in_register = persisted_truth[
        "in_population_register"
    ].eq(1)

    persisted_truth[
        "coverage_status_true"
    ] = np.select(
        [
            true_resident & in_register,
            true_resident & ~in_register,
            ~true_resident & in_register,
        ],
        [
            "correctly_registered",
            "undercoverage",
            "overcoverage",
        ],
        default="correctly_absent",
    )

    persisted_truth[
        "overcoverage_flag_true"
    ] = (
        persisted_truth[
            "coverage_status_true"
        ]
        .eq(
            "overcoverage"
        )
        .astype(int)
    )

    persisted_truth[
        "undercoverage_flag_true"
    ] = (
        persisted_truth[
            "coverage_status_true"
        ]
        .eq(
            "undercoverage"
        )
        .astype(int)
    )

    persisted_truth[
        "registered_household_id_sim"
    ] = persisted_truth[
        "true_household_id"
    ].where(
        true_resident,
        persisted_truth[
            "former_household_id"
        ],
    )

    persisted_truth[
        "registered_address_id_sim"
    ] = persisted_truth[
        "true_address_id"
    ].where(
        true_resident,
        persisted_truth[
            "former_address_id"
        ],
    )

    persisted_truth[
        "registered_region_code_sim"
    ] = persisted_truth[
        "true_region_code"
    ].where(
        true_resident,
        persisted_truth[
            "former_region_code"
        ],
    )

    persisted_truth[
        "registered_municipality_code_sim"
    ] = persisted_truth[
        "true_municipality_code"
    ].where(
        true_resident,
        persisted_truth[
            "former_municipality_code"
        ],
    )

    return persisted_truth
