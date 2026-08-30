"""Generate the hidden synthetic population world."""

from collections.abc import Mapping
from typing import Any

import numpy as np
import pandas as pd


REGION_COLUMNS = [
    "region_code",
    "region_name",
    "urbanicity",
    "population_weight",
]

AGE_BAND_COLUMNS = [
    "age_band",
    "min_age",
    "max_sample_age",
    "resident_probability",
    "former_probability",
]

VALID_URBANICITY = (
    "urban",
    "mixed",
    "rural",
)

ADDRESS_TYPES = (
    "single_family",
    "multi_family",
    "large_residential",
)

ADDRESS_COLUMNS = [
    "address_id",
    "region_code",
    "region_name",
    "municipality_code",
    "urbanicity",
    "address_type",
]

HOUSEHOLD_SIZES = (
    1,
    2,
    3,
    4,
    5,
    6,
)

HOUSEHOLD_COLUMNS = [
    "household_id",
    "household_size",
    "address_id",
    "region_code",
    "municipality_code",
]


def build_regions(config: Mapping[str, Any]) -> pd.DataFrame:
    """Build and validate the synthetic regional reference table."""

    region_rows = config.get("regions")

    if not isinstance(region_rows, list) or not region_rows:
        raise ValueError("Configuration must contain a non-empty regions list.")

    regions = pd.DataFrame(region_rows)

    missing_columns = set(REGION_COLUMNS) - set(regions.columns)

    if missing_columns:
        raise ValueError(
            "Region configuration is missing required fields: "
            + ", ".join(sorted(missing_columns))
        )

    regions = regions[REGION_COLUMNS].copy()

    if regions["region_code"].isna().any():
        raise ValueError("Region codes must not be missing.")

    if regions["region_code"].duplicated().any():
        raise ValueError("Region codes must be unique.")

    if not set(regions["urbanicity"]).issubset(VALID_URBANICITY):
        raise ValueError("Region configuration contains invalid urbanicity values.")

    weights = regions["population_weight"].to_numpy(dtype=float)

    if np.any(weights < 0):
        raise ValueError("Region population weights must be non-negative.")

    if not np.isclose(weights.sum(), 1.0):
        raise ValueError("Region population weights must sum to 1.")

    return regions


def build_age_bands(config: Mapping[str, Any]) -> pd.DataFrame:
    """Build and validate the synthetic age-band reference table."""

    demographics = config.get("demographics")

    if not isinstance(demographics, Mapping):
        raise ValueError("Configuration must contain a demographics section.")

    age_band_rows = demographics.get("age_bands")

    if not isinstance(age_band_rows, list) or not age_band_rows:
        raise ValueError(
            "Demographics configuration must contain a non-empty age_bands list."
        )

    age_bands = pd.DataFrame(age_band_rows)

    missing_columns = set(AGE_BAND_COLUMNS) - set(age_bands.columns)

    if missing_columns:
        raise ValueError(
            "Age-band configuration is missing required fields: "
            + ", ".join(sorted(missing_columns))
        )

    age_bands = age_bands[AGE_BAND_COLUMNS].copy()

    if age_bands["age_band"].duplicated().any():
        raise ValueError("Age-band labels must be unique.")

    min_ages = age_bands["min_age"].to_numpy(dtype=int)
    max_sample_ages = age_bands["max_sample_age"].to_numpy(dtype=int)

    if np.any(min_ages < 0):
        raise ValueError("Minimum ages must be non-negative.")

    if np.any(max_sample_ages < min_ages):
        raise ValueError("Maximum sampling ages must not be below minimum ages.")

    if np.any(np.diff(min_ages) <= 0):
        raise ValueError("Age bands must be ordered by strictly increasing minimum age.")

    for probability_column in (
        "resident_probability",
        "former_probability",
    ):
        probabilities = age_bands[probability_column].to_numpy(dtype=float)

        if np.any(probabilities < 0):
            raise ValueError(
                f"{probability_column} values must be non-negative."
            )

        if not np.isclose(probabilities.sum(), 1.0):
            raise ValueError(
                f"{probability_column} values must sum to 1."
            )

    return age_bands


def sample_age(
    n: int,
    age_bands: pd.DataFrame,
    probability_column: str,
    rng: np.random.Generator,
) -> np.ndarray:
    """Sample integer ages via an age-band distribution."""

    if n < 0:
        raise ValueError("Sample size must be non-negative.")

    if probability_column not in age_bands.columns:
        raise KeyError(
            f"Unknown age-band probability column: {probability_column}"
        )

    probabilities = age_bands[probability_column].to_numpy(dtype=float)

    if np.any(probabilities < 0) or not np.isclose(
        probabilities.sum(),
        1.0,
    ):
        raise ValueError(
            f"{probability_column} must contain non-negative probabilities "
            "that sum to 1."
        )

    sampled_band_positions = rng.choice(
        len(age_bands),
        size=n,
        replace=True,
        p=probabilities,
    )

    ages = np.empty(n, dtype=np.int64)

    for position in range(len(age_bands)):
        selected = sampled_band_positions == position
        n_selected = int(selected.sum())

        if n_selected == 0:
            continue

        lower = int(age_bands.iloc[position]["min_age"])
        upper = int(age_bands.iloc[position]["max_sample_age"])

        ages[selected] = rng.integers(
            lower,
            upper + 1,
            size=n_selected,
        )

    return ages


def age_group_from_age(
    ages: np.ndarray,
    age_bands: pd.DataFrame,
) -> np.ndarray:
    """Assign configured age-group labels from non-negative integer ages."""

    age_values = np.asarray(ages, dtype=np.int64)

    if age_values.ndim != 1:
        raise ValueError("Ages must be a one-dimensional array.")

    if np.any(age_values < 0):
        raise ValueError("Ages must be non-negative.")

    minimum_ages = age_bands["min_age"].to_numpy(dtype=np.int64)
    labels = age_bands["age_band"].to_numpy(dtype=object)

    band_positions = np.searchsorted(
        minimum_ages,
        age_values,
        side="right",
    ) - 1

    if np.any(band_positions < 0):
        raise ValueError("Unable to assign an age group.")

    return labels[band_positions]


def generate_address_register(
    regions: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate the synthetic address reference register."""

    address_config = config.get("addresses")

    if not isinstance(address_config, Mapping):
        raise ValueError("Configuration must contain an addresses section.")

    n_addresses = address_config.get("n_addresses")
    municipalities_per_region = address_config.get(
        "municipalities_per_region"
    )
    type_probabilities = address_config.get(
        "address_type_probabilities"
    )

    if not isinstance(n_addresses, int) or n_addresses <= 0:
        raise ValueError("n_addresses must be a positive integer.")

    if (
        not isinstance(municipalities_per_region, int)
        or municipalities_per_region <= 0
    ):
        raise ValueError(
            "municipalities_per_region must be a positive integer."
        )

    if not isinstance(type_probabilities, Mapping):
        raise ValueError(
            "addresses must define address_type_probabilities."
        )

    for urbanicity in VALID_URBANICITY:
        probabilities_by_type = type_probabilities.get(
            urbanicity
        )

        if not isinstance(probabilities_by_type, Mapping):
            raise ValueError(
                "Missing address-type probabilities for "
                f"{urbanicity}."
            )

        if set(probabilities_by_type) != set(ADDRESS_TYPES):
            raise ValueError(
                "Address-type probability definitions must contain "
                "exactly the configured address types."
            )

        probabilities = np.array(
            [
                probabilities_by_type[address_type]
                for address_type in ADDRESS_TYPES
            ],
            dtype=float,
        )

        if np.any(probabilities < 0):
            raise ValueError(
                "Address-type probabilities must be non-negative."
            )

        if not np.isclose(probabilities.sum(), 1.0):
            raise ValueError(
                "Address-type probabilities must sum to 1."
            )

    region_codes = regions["region_code"].to_numpy(
        dtype=object
    )
    region_weights = regions["population_weight"].to_numpy(
        dtype=float
    )

    sampled_region_codes = rng.choice(
        region_codes,
        size=n_addresses,
        replace=True,
        p=region_weights,
    )

    address_register = pd.DataFrame(
        {
            "address_id": [
                f"A{number:06d}"
                for number in range(
                    1,
                    n_addresses + 1,
                )
            ],
            "region_code": sampled_region_codes,
        }
    )

    address_register = address_register.merge(
        regions[
            [
                "region_code",
                "region_name",
                "urbanicity",
            ]
        ],
        on="region_code",
        how="left",
        validate="many_to_one",
        sort=False,
    )

    municipality_numbers = rng.integers(
        1,
        municipalities_per_region + 1,
        size=n_addresses,
    )

    address_register["municipality_code"] = [
        f"{region_code}-M{municipality_number:02d}"
        for region_code, municipality_number in zip(
            address_register["region_code"],
            municipality_numbers,
            strict=True,
        )
    ]

    address_types = np.empty(
        n_addresses,
        dtype=object,
    )

    for urbanicity in VALID_URBANICITY:
        selected = (
            address_register["urbanicity"].to_numpy()
            == urbanicity
        )
        n_selected = int(selected.sum())

        if n_selected == 0:
            continue

        probabilities_by_type = type_probabilities[
            urbanicity
        ]
        probabilities = np.array(
            [
                probabilities_by_type[address_type]
                for address_type in ADDRESS_TYPES
            ],
            dtype=float,
        )

        address_types[selected] = rng.choice(
            ADDRESS_TYPES,
            size=n_selected,
            replace=True,
            p=probabilities,
        )

    address_register["address_type"] = address_types

    return address_register[ADDRESS_COLUMNS]


def build_address_sampling_weights(
    address_register: pd.DataFrame,
    config: Mapping[str, Any],
) -> np.ndarray:
    """Return household-sampling weights for synthetic addresses."""

    address_config = config.get("addresses")

    if not isinstance(address_config, Mapping):
        raise ValueError("Configuration must contain an addresses section.")

    weight_config = address_config.get(
        "household_sampling_weights"
    )

    if not isinstance(weight_config, Mapping):
        raise ValueError(
            "addresses must define household_sampling_weights."
        )

    if set(weight_config) != set(ADDRESS_TYPES):
        raise ValueError(
            "Household sampling weights must contain exactly "
            "the configured address types."
        )

    weights_by_type = {
        address_type: float(
            weight_config[address_type]
        )
        for address_type in ADDRESS_TYPES
    }

    if any(
        weight <= 0
        for weight in weights_by_type.values()
    ):
        raise ValueError(
            "Household sampling weights must be positive."
        )

    unknown_types = set(
        address_register["address_type"].dropna()
    ) - set(ADDRESS_TYPES)

    if unknown_types:
        raise ValueError(
            "Address register contains unknown address types."
        )

    if address_register["address_type"].isna().any():
        raise ValueError(
            "Address register contains missing address types."
        )

    return (
        address_register["address_type"]
        .map(weights_by_type)
        .to_numpy(dtype=float)
    )


def generate_households(
    n_residents: int,
    address_register: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate households whose sizes reconcile exactly to residents."""

    if not isinstance(n_residents, int) or n_residents <= 0:
        raise ValueError("n_residents must be a positive integer.")

    household_config = config.get("households")

    if not isinstance(household_config, Mapping):
        raise ValueError("Configuration must contain a households section.")

    size_probability_config = household_config.get(
        "size_probabilities"
    )

    if not isinstance(size_probability_config, Mapping):
        raise ValueError(
            "households must define size_probabilities."
        )

    if set(size_probability_config) != set(HOUSEHOLD_SIZES):
        raise ValueError(
            "Household-size probabilities must contain "
            "exactly sizes 1 through 6."
        )

    size_probabilities = np.array(
        [
            size_probability_config[size]
            for size in HOUSEHOLD_SIZES
        ],
        dtype=float,
    )

    if np.any(size_probabilities < 0):
        raise ValueError(
            "Household-size probabilities must be non-negative."
        )

    if not np.isclose(size_probabilities.sum(), 1.0):
        raise ValueError(
            "Household-size probabilities must sum to 1."
        )

    # Because the minimum household size is one, n_residents candidate
    # draws are always sufficient to reach the target population.
    candidate_sizes = rng.choice(
        HOUSEHOLD_SIZES,
        size=n_residents,
        replace=True,
        p=size_probabilities,
    ).astype(np.int64)

    cumulative_sizes = np.cumsum(candidate_sizes)

    final_position = int(
        np.searchsorted(
            cumulative_sizes,
            n_residents,
            side="left",
        )
    )

    household_sizes = candidate_sizes[
        : final_position + 1
    ].copy()

    overshoot = int(
        household_sizes.sum()
        - n_residents
    )

    household_sizes[-1] -= overshoot

    if household_sizes[-1] < 1:
        raise RuntimeError(
            "Final household adjustment produced an invalid size."
        )

    n_households = len(household_sizes)

    address_ids = address_register[
        "address_id"
    ].to_numpy(dtype=object)

    if len(address_ids) == 0:
        raise ValueError(
            "Address register must contain at least one address."
        )

    if pd.Series(address_ids).duplicated().any():
        raise ValueError(
            "Address register contains duplicated address IDs."
        )

    address_weights = build_address_sampling_weights(
        address_register,
        config,
    )

    address_probabilities = (
        address_weights
        / address_weights.sum()
    )

    sampled_address_ids = rng.choice(
        address_ids,
        size=n_households,
        replace=True,
        p=address_probabilities,
    )

    households = pd.DataFrame(
        {
            "household_id": [
                f"H{number:06d}"
                for number in range(
                    1,
                    n_households + 1,
                )
            ],
            "household_size": household_sizes,
            "address_id": sampled_address_ids,
        }
    )

    households = households.merge(
        address_register[
            [
                "address_id",
                "region_code",
                "municipality_code",
            ]
        ],
        on="address_id",
        how="left",
        validate="many_to_one",
        sort=False,
    )

    if households[
        [
            "region_code",
            "municipality_code",
        ]
    ].isna().any().any():
        raise RuntimeError(
            "Household geography could not be resolved "
            "from the address register."
        )

    if int(households["household_size"].sum()) != n_residents:
        raise RuntimeError(
            "Household sizes do not reconcile to the resident total."
        )

    return households[HOUSEHOLD_COLUMNS]
