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

SEX_CATEGORIES = (
    "F",
    "M",
)

CITIZENSHIP_GROUPS = (
    "DE",
    "EU",
    "Non-EU",
)

POPULATION_WORLD_COLUMNS = [
    "person_id",
    "true_resident",
    "sex",
    "age",
    "age_group",
    "citizenship_group",
    "true_household_id",
    "true_address_id",
    "true_region_code",
    "true_municipality_code",
    "former_household_id",
    "former_address_id",
    "former_region_code",
    "former_municipality_code",
    "departure_date_true",
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


def _validated_probability_vector(
    probability_config: Mapping[str, Any],
    categories: tuple[str, ...],
    name: str,
) -> np.ndarray:
    """Validate named probabilities and return them in category order."""

    if not isinstance(probability_config, Mapping):
        raise ValueError(
            f"{name} must be a mapping of categories to probabilities."
        )

    if set(probability_config) != set(categories):
        raise ValueError(
            f"{name} must contain exactly: "
            + ", ".join(categories)
        )

    probabilities = np.array(
        [
            probability_config[category]
            for category in categories
        ],
        dtype=float,
    )

    if np.any(probabilities < 0):
        raise ValueError(
            f"{name} probabilities must be non-negative."
        )

    if not np.isclose(probabilities.sum(), 1.0):
        raise ValueError(
            f"{name} probabilities must sum to 1."
        )

    return probabilities


def generate_true_residents(
    households: pd.DataFrame,
    age_bands: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate the hidden synthetic true-resident population."""

    population_config = config.get("population")

    if not isinstance(population_config, Mapping):
        raise ValueError("Configuration must contain a population section.")

    n_true_residents = population_config.get(
        "n_true_residents"
    )

    if (
        not isinstance(n_true_residents, int)
        or n_true_residents <= 0
    ):
        raise ValueError(
            "n_true_residents must be a positive integer."
        )

    missing_household_columns = (
        set(HOUSEHOLD_COLUMNS)
        - set(households.columns)
    )

    if missing_household_columns:
        raise ValueError(
            "Households are missing required fields: "
            + ", ".join(
                sorted(missing_household_columns)
            )
        )

    if households["household_id"].duplicated().any():
        raise ValueError(
            "Households contain duplicated household IDs."
        )

    household_sizes = households[
        "household_size"
    ].to_numpy(dtype=np.int64)

    if np.any(
        (household_sizes < 1)
        | (household_sizes > 6)
    ):
        raise ValueError(
            "Household sizes must be between 1 and 6."
        )

    if int(household_sizes.sum()) != n_true_residents:
        raise ValueError(
            "Household sizes do not reconcile to "
            "n_true_residents."
        )

    household_assignments = np.repeat(
        households["household_id"].to_numpy(
            dtype=object
        ),
        household_sizes,
    )

    resident_households = pd.DataFrame(
        {
            "person_id": [
                f"P{number:06d}"
                for number in range(
                    1,
                    n_true_residents + 1,
                )
            ],
            "true_household_id":
                household_assignments,
        }
    )

    resident_households = resident_households.merge(
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
        sort=False,
    ).drop(
        columns="household_id"
    )

    if resident_households[
        [
            "address_id",
            "region_code",
            "municipality_code",
        ]
    ].isna().any().any():
        raise RuntimeError(
            "Resident household geography could not be resolved."
        )

    demographics = config.get("demographics")

    if not isinstance(demographics, Mapping):
        raise ValueError(
            "Configuration must contain a demographics section."
        )

    sex_probabilities = _validated_probability_vector(
        demographics.get(
            "sex_probabilities"
        ),
        SEX_CATEGORIES,
        "sex_probabilities",
    )

    citizenship_probabilities = (
        _validated_probability_vector(
            demographics.get(
                "resident_citizenship_probabilities"
            ),
            CITIZENSHIP_GROUPS,
            "resident_citizenship_probabilities",
        )
    )

    # Preserve the current generator's conceptual draw order:
    # sex, age, then citizenship.
    sex = rng.choice(
        SEX_CATEGORIES,
        size=n_true_residents,
        replace=True,
        p=sex_probabilities,
    )

    age = sample_age(
        n_true_residents,
        age_bands,
        "resident_probability",
        rng,
    )

    age_group = age_group_from_age(
        age,
        age_bands,
    )

    citizenship_group = rng.choice(
        CITIZENSHIP_GROUPS,
        size=n_true_residents,
        replace=True,
        p=citizenship_probabilities,
    )

    true_residents = pd.DataFrame(
        {
            "person_id":
                resident_households["person_id"],
            "true_resident":
                np.ones(
                    n_true_residents,
                    dtype=np.int64,
                ),
            "sex":
                sex,
            "age":
                age,
            "age_group":
                age_group,
            "citizenship_group":
                citizenship_group,
            "true_household_id":
                resident_households[
                    "true_household_id"
                ],
            "true_address_id":
                resident_households[
                    "address_id"
                ],
            "true_region_code":
                resident_households[
                    "region_code"
                ],
            "true_municipality_code":
                resident_households[
                    "municipality_code"
                ],
        }
    )

    true_residents["former_household_id"] = pd.NA
    true_residents["former_address_id"] = pd.NA
    true_residents["former_region_code"] = pd.NA
    true_residents["former_municipality_code"] = pd.NA
    true_residents["departure_date_true"] = pd.Series(
        pd.NaT,
        index=true_residents.index,
        dtype="datetime64[ns]",
    )

    return true_residents[POPULATION_WORLD_COLUMNS]


def generate_former_residents(
    households: pd.DataFrame,
    age_bands: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate synthetic former residents with historical locations."""

    population_config = config.get("population")

    if not isinstance(population_config, Mapping):
        raise ValueError(
            "Configuration must contain a population section."
        )

    n_true_residents = population_config.get(
        "n_true_residents"
    )
    n_former_residents = population_config.get(
        "n_former_residents"
    )

    if (
        not isinstance(n_true_residents, int)
        or n_true_residents <= 0
    ):
        raise ValueError(
            "n_true_residents must be a positive integer."
        )

    if (
        not isinstance(n_former_residents, int)
        or n_former_residents <= 0
    ):
        raise ValueError(
            "n_former_residents must be a positive integer."
        )

    missing_household_columns = (
        set(HOUSEHOLD_COLUMNS)
        - set(households.columns)
    )

    if missing_household_columns:
        raise ValueError(
            "Households are missing required fields: "
            + ", ".join(
                sorted(missing_household_columns)
            )
        )

    if households["household_id"].duplicated().any():
        raise ValueError(
            "Households contain duplicated household IDs."
        )

    household_ids = households[
        "household_id"
    ].to_numpy(dtype=object)

    if len(household_ids) == 0:
        raise ValueError(
            "Households must contain at least one household."
        )

    # The original generator samples former households uniformly,
    # without using household-size or address-type weights.
    former_household_ids = rng.choice(
        household_ids,
        size=n_former_residents,
        replace=True,
    )

    first_former_id = n_true_residents + 1
    last_former_id = (
        n_true_residents
        + n_former_residents
    )

    former_households = pd.DataFrame(
        {
            "person_id": [
                f"P{number:06d}"
                for number in range(
                    first_former_id,
                    last_former_id + 1,
                )
            ],
            "former_household_id":
                former_household_ids,
        }
    )

    former_households = former_households.merge(
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
        sort=False,
    ).drop(
        columns="household_id"
    )

    if former_households[
        [
            "address_id",
            "region_code",
            "municipality_code",
        ]
    ].isna().any().any():
        raise RuntimeError(
            "Former-resident household geography "
            "could not be resolved."
        )

    demographics = config.get("demographics")

    if not isinstance(demographics, Mapping):
        raise ValueError(
            "Configuration must contain a demographics section."
        )

    sex_probabilities = _validated_probability_vector(
        demographics.get(
            "sex_probabilities"
        ),
        SEX_CATEGORIES,
        "sex_probabilities",
    )

    citizenship_probabilities = (
        _validated_probability_vector(
            demographics.get(
                "former_citizenship_probabilities"
            ),
            CITIZENSHIP_GROUPS,
            "former_citizenship_probabilities",
        )
    )

    former_config = config.get(
        "former_residents"
    )

    if not isinstance(former_config, Mapping):
        raise ValueError(
            "Configuration must contain a former_residents section."
        )

    departure_start = pd.Timestamp(
        former_config.get(
            "departure_date_start"
        )
    )
    departure_end = pd.Timestamp(
        former_config.get(
            "departure_date_end"
        )
    )

    if departure_end < departure_start:
        raise ValueError(
            "Former-resident departure-date range is invalid."
        )

    # Preserve the conceptual draw order of the R generator:
    # household, sex, age, citizenship, departure date.
    sex = rng.choice(
        SEX_CATEGORIES,
        size=n_former_residents,
        replace=True,
        p=sex_probabilities,
    )

    age = sample_age(
        n_former_residents,
        age_bands,
        "former_probability",
        rng,
    )

    age_group = age_group_from_age(
        age,
        age_bands,
    )

    citizenship_group = rng.choice(
        CITIZENSHIP_GROUPS,
        size=n_former_residents,
        replace=True,
        p=citizenship_probabilities,
    )

    n_departure_days = (
        departure_end - departure_start
    ).days + 1

    departure_offsets = rng.integers(
        0,
        n_departure_days,
        size=n_former_residents,
    )

    departure_dates = (
        departure_start
        + pd.to_timedelta(
            departure_offsets,
            unit="D",
        )
    )

    former_residents = pd.DataFrame(
        {
            "person_id":
                former_households["person_id"],
            "true_resident":
                np.zeros(
                    n_former_residents,
                    dtype=np.int64,
                ),
            "sex":
                sex,
            "age":
                age,
            "age_group":
                age_group,
            "citizenship_group":
                citizenship_group,
        }
    )

    former_residents["true_household_id"] = pd.NA
    former_residents["true_address_id"] = pd.NA
    former_residents["true_region_code"] = pd.NA
    former_residents["true_municipality_code"] = pd.NA

    former_residents["former_household_id"] = (
        former_households[
            "former_household_id"
        ]
    )
    former_residents["former_address_id"] = (
        former_households[
            "address_id"
        ]
    )
    former_residents["former_region_code"] = (
        former_households[
            "region_code"
        ]
    )
    former_residents[
        "former_municipality_code"
    ] = former_households[
        "municipality_code"
    ]

    former_residents[
        "departure_date_true"
    ] = pd.Series(
        departure_dates,
        index=former_residents.index,
        dtype="datetime64[ns]",
    )

    return former_residents[
        POPULATION_WORLD_COLUMNS
    ]


def build_population_truth(
    true_residents: pd.DataFrame,
    former_residents: pd.DataFrame,
    config: Mapping[str, Any],
) -> pd.DataFrame:
    """Combine and validate the complete hidden population truth."""

    population_config = config.get("population")

    if not isinstance(population_config, Mapping):
        raise ValueError(
            "Configuration must contain a population section."
        )

    n_true_residents = population_config.get(
        "n_true_residents"
    )
    n_former_residents = population_config.get(
        "n_former_residents"
    )

    if (
        not isinstance(n_true_residents, int)
        or n_true_residents <= 0
    ):
        raise ValueError(
            "n_true_residents must be a positive integer."
        )

    if (
        not isinstance(n_former_residents, int)
        or n_former_residents <= 0
    ):
        raise ValueError(
            "n_former_residents must be a positive integer."
        )

    components = (
        (
            "true residents",
            true_residents,
            n_true_residents,
            1,
        ),
        (
            "former residents",
            former_residents,
            n_former_residents,
            0,
        ),
    )

    for (
        component_name,
        component,
        expected_rows,
        expected_residence_state,
    ) in components:
        missing_columns = (
            set(POPULATION_WORLD_COLUMNS)
            - set(component.columns)
        )

        if missing_columns:
            raise ValueError(
                f"{component_name} are missing required fields: "
                + ", ".join(
                    sorted(missing_columns)
                )
            )

        if len(component) != expected_rows:
            raise ValueError(
                f"{component_name} must contain "
                f"{expected_rows} rows."
            )

        if component["person_id"].duplicated().any():
            raise ValueError(
                f"{component_name} contain duplicated person IDs."
            )

        if not (
            component["true_resident"]
            == expected_residence_state
        ).all():
            raise ValueError(
                f"{component_name} contain an invalid "
                "true_resident state."
            )

    departure_column = "departure_date_true"

    non_date_columns = [
        column
        for column in POPULATION_WORLD_COLUMNS
        if column != departure_column
    ]

    population_truth = pd.concat(
        [
            true_residents[
                non_date_columns
            ],
            former_residents[
                non_date_columns
            ],
        ],
        ignore_index=True,
    )

    population_truth[
        departure_column
    ] = np.concatenate(
        [
            true_residents[
                departure_column
            ].to_numpy(
                dtype="datetime64[ns]"
            ),
            former_residents[
                departure_column
            ].to_numpy(
                dtype="datetime64[ns]"
            ),
        ]
    )

    if population_truth["person_id"].duplicated().any():
        raise ValueError(
            "Population truth contains duplicated person IDs."
        )

    population_truth = (
        population_truth
        .sort_values(
            "person_id",
            kind="stable",
        )
        .reset_index(drop=True)
    )

    expected_total = (
        n_true_residents
        + n_former_residents
    )

    if len(population_truth) != expected_total:
        raise RuntimeError(
            "Population truth does not reconcile "
            "to the configured universe size."
        )

    expected_person_ids = [
        f"P{number:06d}"
        for number in range(
            1,
            expected_total + 1,
        )
    ]

    if (
        population_truth["person_id"].tolist()
        != expected_person_ids
    ):
        raise ValueError(
            "Population truth person IDs are not "
            "complete and sequential."
        )

    common_fields = [
        "sex",
        "age",
        "age_group",
        "citizenship_group",
    ]

    if population_truth[
        common_fields
    ].isna().any().any():
        raise ValueError(
            "Population truth contains missing "
            "demographic attributes."
        )

    current = (
        population_truth["true_resident"]
        == 1
    )
    former = (
        population_truth["true_resident"]
        == 0
    )

    true_location_fields = [
        "true_household_id",
        "true_address_id",
        "true_region_code",
        "true_municipality_code",
    ]

    former_location_fields = [
        "former_household_id",
        "former_address_id",
        "former_region_code",
        "former_municipality_code",
    ]

    if population_truth.loc[
        current,
        true_location_fields,
    ].isna().any().any():
        raise ValueError(
            "True residents must have complete "
            "current residence geography."
        )

    if population_truth.loc[
        current,
        former_location_fields,
    ].notna().any().any():
        raise ValueError(
            "True residents must not have former "
            "residence geography."
        )

    if population_truth.loc[
        current,
        "departure_date_true",
    ].notna().any():
        raise ValueError(
            "True residents must not have a departure date."
        )

    if population_truth.loc[
        former,
        true_location_fields,
    ].notna().any().any():
        raise ValueError(
            "Former residents must not have current "
            "residence geography."
        )

    if population_truth.loc[
        former,
        former_location_fields,
    ].isna().any().any():
        raise ValueError(
            "Former residents must have complete "
            "former residence geography."
        )

    if population_truth.loc[
        former,
        "departure_date_true",
    ].isna().any():
        raise ValueError(
            "Former residents must have a departure date."
        )

    return population_truth[
        POPULATION_WORLD_COLUMNS
    ]
