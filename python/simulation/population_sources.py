"""Generate imperfect population and administrative activity sources."""

from collections.abc import Mapping
from typing import Any

import numpy as np
import pandas as pd

from simulation.world import age_group_from_age


REGISTRATION_STATUS_CATEGORIES = (
    "main_residence",
    "secondary_residence",
)

POPULATION_REGISTER_COLUMNS = [
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


EMPLOYMENT_STATUS_CATEGORIES = (
    "employed",
    "marginal",
    "self_employed",
)

EMPLOYMENT_REGISTER_COLUMNS = [
    "person_id",
    "ref_date",
    "employment_status",
    "days_employed_last_12m",
    "annual_employment_income",
    "contact_address_id",
]


TAX_REGISTER_COLUMNS = [
    "person_id",
    "tax_year",
    "tax_filing_flag",
    "taxable_income",
    "contact_address_id",
]


EDUCATION_REGISTER_COLUMNS = [
    "person_id",
    "school_year",
    "enrolment_flag",
    "institution_type",
    "contact_address_id",
]

POPULATION_TRUTH_REQUIRED_COLUMNS = {
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
}


def _validate_probability(
    value: Any,
    name: str,
) -> float:
    """Validate a scalar probability."""

    probability = float(value)

    if not 0.0 <= probability <= 1.0:
        raise ValueError(
            f"{name} must be between 0 and 1."
        )

    return probability


def _validated_probability_vector(
    probability_config: Mapping[str, Any],
    categories: tuple[str, ...],
    name: str,
) -> np.ndarray:
    """Validate named probabilities in a stable category order."""

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


def _sample_uniform_dates(
    start_value: Any,
    end_value: Any,
    size: int,
    rng: np.random.Generator,
) -> pd.Series:
    """Sample dates uniformly over an inclusive calendar interval."""

    if start_value is None or end_value is None:
        raise ValueError(
            "Date-range endpoints must be configured."
        )

    start = pd.Timestamp(start_value)
    end = pd.Timestamp(end_value)

    if end < start:
        raise ValueError(
            "Date-range end must not precede its start."
        )

    n_days = (end - start).days + 1

    offsets = rng.integers(
        0,
        n_days,
        size=size,
    )

    return pd.Series(
        start
        + pd.to_timedelta(
            offsets,
            unit="D",
        ),
        dtype="datetime64[ns]",
    )


def generate_population_register(
    population_truth: pd.DataFrame,
    age_bands: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate an imperfect population-register delivery."""

    missing_truth_columns = (
        POPULATION_TRUTH_REQUIRED_COLUMNS
        - set(population_truth.columns)
    )

    if missing_truth_columns:
        raise ValueError(
            "Population truth is missing required fields: "
            + ", ".join(
                sorted(missing_truth_columns)
            )
        )

    if population_truth["person_id"].duplicated().any():
        raise ValueError(
            "Population truth contains duplicated person IDs."
        )

    residence_states = set(
        population_truth[
            "true_resident"
        ].dropna().unique()
    )

    if residence_states != {0, 1}:
        raise ValueError(
            "Population truth must contain true_resident states 0 and 1."
        )

    register_config = config.get(
        "population_register"
    )

    if not isinstance(register_config, Mapping):
        raise ValueError(
            "Configuration must contain a population_register section."
        )

    undercoverage_probability = _validate_probability(
        register_config.get(
            "undercoverage_probability"
        ),
        "undercoverage_probability",
    )

    stale_former_probability = _validate_probability(
        register_config.get(
            "stale_former_probability"
        ),
        "stale_former_probability",
    )

    status_probabilities = (
        _validated_probability_vector(
            register_config.get(
                "registration_status_probabilities"
            ),
            REGISTRATION_STATUS_CATEGORIES,
            "registration_status_probabilities",
        )
    )

    imperfections = register_config.get(
        "imperfections"
    )

    if not isinstance(imperfections, Mapping):
        raise ValueError(
            "population_register must define imperfections."
        )

    citizenship_missing_probability = (
        _validate_probability(
            imperfections.get(
                "citizenship_missing_probability"
            ),
            "citizenship_missing_probability",
        )
    )

    status_missing_probability = (
        _validate_probability(
            imperfections.get(
                "registration_status_missing_probability"
            ),
            "registration_status_missing_probability",
        )
    )

    last_move_shift_probability = (
        _validate_probability(
            imperfections.get(
                "last_move_shift_probability"
            ),
            "last_move_shift_probability",
        )
    )

    age_outlier_probability = (
        _validate_probability(
            imperfections.get(
                "age_outlier_probability"
            ),
            "age_outlier_probability",
        )
    )

    shift_min = imperfections.get(
        "last_move_shift_days_min"
    )
    shift_max = imperfections.get(
        "last_move_shift_days_max"
    )
    outlier_min = imperfections.get(
        "age_outlier_min"
    )
    outlier_max = imperfections.get(
        "age_outlier_max"
    )

    if (
        not isinstance(shift_min, int)
        or not isinstance(shift_max, int)
        or shift_min < 1
        or shift_max < shift_min
    ):
        raise ValueError(
            "Last-move shift limits are invalid."
        )

    if (
        not isinstance(outlier_min, int)
        or not isinstance(outlier_max, int)
        or outlier_min < 0
        or outlier_max < outlier_min
    ):
        raise ValueError(
            "Age-outlier limits are invalid."
        )

    coverage_draw = rng.random(
        len(population_truth)
    )

    true_resident = (
        population_truth[
            "true_resident"
        ].to_numpy(dtype=np.int64)
    )

    included = np.where(
        true_resident == 1,
        coverage_draw >= undercoverage_probability,
        coverage_draw < stale_former_probability,
    )

    observed = (
        population_truth.loc[
            included
        ]
        .reset_index(drop=True)
        .copy()
    )

    is_current = (
        observed["true_resident"]
        .to_numpy(dtype=np.int64)
        == 1
    )

    population_register = pd.DataFrame(
        {
            "person_id":
                observed["person_id"].copy(),
            "sex":
                observed["sex"].copy(),
            "age":
                observed["age"].to_numpy(
                    dtype=np.int64
                ),
            "age_group":
                observed["age_group"].copy(),
            "citizenship_group":
                observed[
                    "citizenship_group"
                ].copy(),
        }
    )

    location_pairs = (
        (
            "household_id",
            "true_household_id",
            "former_household_id",
        ),
        (
            "address_id",
            "true_address_id",
            "former_address_id",
        ),
        (
            "region_code",
            "true_region_code",
            "former_region_code",
        ),
        (
            "municipality_code",
            "true_municipality_code",
            "former_municipality_code",
        ),
    )

    for (
        output_column,
        current_column,
        former_column,
    ) in location_pairs:
        values = observed[
            former_column
        ].copy()

        values.loc[is_current] = (
            observed.loc[
                is_current,
                current_column,
            ]
        )

        population_register[
            output_column
        ] = values.to_numpy(dtype=object)

    n_records = len(population_register)

    population_register[
        "registration_status"
    ] = rng.choice(
        REGISTRATION_STATUS_CATEGORIES,
        size=n_records,
        replace=True,
        p=status_probabilities,
    )

    registration_dates = _sample_uniform_dates(
        register_config.get(
            "registration_date_start"
        ),
        register_config.get(
            "registration_date_end"
        ),
        n_records,
        rng,
    )

    last_move_dates = _sample_uniform_dates(
        register_config.get(
            "last_move_date_start"
        ),
        register_config.get(
            "last_move_date_end"
        ),
        n_records,
        rng,
    )

    last_move_dates = pd.Series(
        np.maximum(
            last_move_dates.to_numpy(
                dtype="datetime64[ns]"
            ),
            registration_dates.to_numpy(
                dtype="datetime64[ns]"
            ),
        ),
        dtype="datetime64[ns]",
    )

    population_register[
        "registration_date"
    ] = registration_dates

    population_register[
        "last_move_date"
    ] = last_move_dates

    citizenship_missing = (
        rng.random(n_records)
        < citizenship_missing_probability
    )

    population_register.loc[
        citizenship_missing,
        "citizenship_group",
    ] = pd.NA

    status_missing = (
        rng.random(n_records)
        < status_missing_probability
    )

    population_register.loc[
        status_missing,
        "registration_status",
    ] = pd.NA

    move_shifted = (
        rng.random(n_records)
        < last_move_shift_probability
    )

    n_shifted = int(
        move_shifted.sum()
    )

    if n_shifted > 0:
        shift_days = rng.integers(
            shift_min,
            shift_max + 1,
            size=n_shifted,
        )

        population_register.loc[
            move_shifted,
            "last_move_date",
        ] = (
            population_register.loc[
                move_shifted,
                "last_move_date",
            ].to_numpy(
                dtype="datetime64[ns]"
            )
            + shift_days.astype(
                "timedelta64[D]"
            )
        )

    age_outlier = (
        rng.random(n_records)
        < age_outlier_probability
    )

    n_age_outliers = int(
        age_outlier.sum()
    )

    if n_age_outliers > 0:
        outlier_ages = rng.integers(
            outlier_min,
            outlier_max + 1,
            size=n_age_outliers,
        )

        population_register.loc[
            age_outlier,
            "age",
        ] = outlier_ages

        population_register.loc[
            age_outlier,
            "age_group",
        ] = age_group_from_age(
            outlier_ages,
            age_bands,
        )

    if population_register[
        "person_id"
    ].duplicated().any():
        raise RuntimeError(
            "Population register contains duplicated person IDs."
        )

    if population_register[
        [
            "household_id",
            "address_id",
            "region_code",
            "municipality_code",
        ]
    ].isna().any().any():
        raise RuntimeError(
            "Population-register geography is incomplete."
        )

    return population_register[
        POPULATION_REGISTER_COLUMNS
    ]


def generate_contact_addresses(
    source_population: pd.DataFrame,
    address_register: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> np.ndarray:
    """Generate imperfect source-specific contact addresses."""

    required_population_columns = {
        "true_resident",
        "true_address_id",
        "true_region_code",
        "former_address_id",
        "former_region_code",
    }

    missing_columns = (
        required_population_columns
        - set(source_population.columns)
    )

    if missing_columns:
        raise ValueError(
            "Source population is missing contact-address fields: "
            + ", ".join(sorted(missing_columns))
        )

    required_address_columns = {
        "address_id",
        "region_code",
    }

    if not required_address_columns.issubset(
        address_register.columns
    ):
        raise ValueError(
            "Address register is missing required contact-address fields."
        )

    if address_register["address_id"].duplicated().any():
        raise ValueError(
            "Address register contains duplicated address IDs."
        )

    contact_config = config.get(
        "contact_address"
    )

    if not isinstance(contact_config, Mapping):
        raise ValueError(
            "Configuration must contain a contact_address section."
        )

    reference_probability = _validate_probability(
        contact_config.get(
            "reference_probability"
        ),
        "reference_probability",
    )

    alternative_probability = _validate_probability(
        contact_config.get(
            "alternative_same_region_probability"
        ),
        "alternative_same_region_probability",
    )

    if (
        reference_probability
        + alternative_probability
        > 1.0
    ):
        raise ValueError(
            "Contact-address probabilities must not exceed 1."
        )

    n_records = len(source_population)

    if n_records == 0:
        return np.empty(
            0,
            dtype=object,
        )

    is_current = (
        source_population[
            "true_resident"
        ].to_numpy(dtype=np.int64)
        == 1
    )

    reference_address = (
        source_population[
            "former_address_id"
        ].copy()
    )

    reference_address.loc[
        is_current
    ] = source_population.loc[
        is_current,
        "true_address_id",
    ]

    reference_region = (
        source_population[
            "former_region_code"
        ].copy()
    )

    reference_region.loc[
        is_current
    ] = source_population.loc[
        is_current,
        "true_region_code",
    ]

    if (
        reference_address.isna().any()
        or reference_region.isna().any()
    ):
        raise ValueError(
            "Reference contact geography is incomplete."
        )

    addresses_by_region = {
        region_code: group[
            "address_id"
        ].to_numpy(dtype=object)
        for region_code, group in (
            address_register.groupby(
                "region_code",
                sort=False,
            )
        )
    }

    contact_draw = rng.random(
        n_records
    )

    use_reference = (
        contact_draw
        < reference_probability
    )

    use_alternative = (
        (contact_draw >= reference_probability)
        & (
            contact_draw
            < (
                reference_probability
                + alternative_probability
            )
        )
    )

    contact_addresses = np.full(
        n_records,
        pd.NA,
        dtype=object,
    )

    contact_addresses[
        use_reference
    ] = reference_address.loc[
        use_reference
    ].to_numpy(dtype=object)

    alternative_positions = np.flatnonzero(
        use_alternative
    )

    for position in alternative_positions:
        region_code = reference_region.iloc[
            position
        ]
        current_address = reference_address.iloc[
            position
        ]

        region_addresses = addresses_by_region.get(
            region_code
        )

        if region_addresses is None:
            raise ValueError(
                "No addresses available for contact-address region."
            )

        alternatives = region_addresses[
            region_addresses != current_address
        ]

        if len(alternatives) == 0:
            contact_addresses[position] = (
                current_address
            )
        else:
            contact_addresses[position] = (
                rng.choice(alternatives)
            )

    return contact_addresses


def generate_employment_register(
    population_truth: pd.DataFrame,
    address_register: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
    contact_rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate an imperfect employment-register delivery."""

    required_columns = (
        POPULATION_TRUTH_REQUIRED_COLUMNS
        | {
            "age",
        }
    )

    missing_columns = (
        required_columns
        - set(population_truth.columns)
    )

    if missing_columns:
        raise ValueError(
            "Population truth is missing employment fields: "
            + ", ".join(sorted(missing_columns))
        )

    if population_truth["person_id"].duplicated().any():
        raise ValueError(
            "Population truth contains duplicated person IDs."
        )

    employment_config = config.get(
        "employment"
    )

    if not isinstance(employment_config, Mapping):
        raise ValueError(
            "Configuration must contain an employment section."
        )

    eligible_age_min = employment_config.get(
        "eligible_age_min"
    )
    eligible_age_max = employment_config.get(
        "eligible_age_max"
    )

    if (
        not isinstance(eligible_age_min, int)
        or not isinstance(eligible_age_max, int)
        or eligible_age_min < 0
        or eligible_age_max < eligible_age_min
    ):
        raise ValueError(
            "Employment eligibility ages are invalid."
        )

    active_config = employment_config.get(
        "active_probabilities"
    )

    if not isinstance(active_config, Mapping):
        raise ValueError(
            "employment must define active_probabilities."
        )

    active_probabilities = {
        name: _validate_probability(
            active_config.get(name),
            name,
        )
        for name in (
            "nonresident",
            "resident_18_24",
            "resident_25_39",
            "resident_40_64",
            "resident_65_67",
            "fallback",
        )
    }

    status_probabilities = (
        _validated_probability_vector(
            employment_config.get(
                "status_probabilities"
            ),
            EMPLOYMENT_STATUS_CATEGORIES,
            "employment status probabilities",
        )
    )

    no_record_retention_probability = (
        _validate_probability(
            employment_config.get(
                "no_record_retention_probability"
            ),
            "no_record_retention_probability",
        )
    )

    missing_days_probability = (
        _validate_probability(
            employment_config.get(
                "missing_days_probability"
            ),
            "missing_days_probability",
        )
    )

    missing_income_probability = (
        _validate_probability(
            employment_config.get(
                "missing_income_probability"
            ),
            "missing_income_probability",
        )
    )

    ages = population_truth[
        "age"
    ].to_numpy(dtype=np.int64)

    true_resident = population_truth[
        "true_resident"
    ].to_numpy(dtype=np.int64)

    eligible = (
        (ages >= eligible_age_min)
        & (ages <= eligible_age_max)
    )

    active_probability = np.full(
        len(population_truth),
        active_probabilities["fallback"],
        dtype=float,
    )

    active_probability[
        true_resident == 0
    ] = active_probabilities[
        "nonresident"
    ]

    resident = (
        true_resident == 1
    )

    active_probability[
        resident
        & (ages >= 18)
        & (ages <= 24)
    ] = active_probabilities[
        "resident_18_24"
    ]

    active_probability[
        resident
        & (ages >= 25)
        & (ages <= 39)
    ] = active_probabilities[
        "resident_25_39"
    ]

    active_probability[
        resident
        & (ages >= 40)
        & (ages <= 64)
    ] = active_probabilities[
        "resident_40_64"
    ]

    active_probability[
        resident
        & (ages >= 65)
        & (ages <= 67)
    ] = active_probabilities[
        "resident_65_67"
    ]

    active = (
        eligible
        & (
            rng.random(
                len(population_truth)
            )
            < active_probability
        )
    )

    employment_status = np.full(
        len(population_truth),
        "no_record",
        dtype=object,
    )

    n_active = int(
        active.sum()
    )

    employment_status[
        active
    ] = rng.choice(
        EMPLOYMENT_STATUS_CATEGORIES,
        size=n_active,
        replace=True,
        p=status_probabilities,
    )

    days = np.zeros(
        len(population_truth),
        dtype=float,
    )

    income = np.zeros(
        len(population_truth),
        dtype=float,
    )

    days_config = employment_config.get(
        "days_employed"
    )
    income_config = employment_config.get(
        "annual_income"
    )

    if (
        not isinstance(days_config, Mapping)
        or not isinstance(income_config, Mapping)
    ):
        raise ValueError(
            "Employment amount configurations are missing."
        )

    for status in EMPLOYMENT_STATUS_CATEGORIES:
        selected = (
            employment_status == status
        )

        n_selected = int(
            selected.sum()
        )

        if n_selected == 0:
            continue

        status_days = days_config.get(
            status
        )
        status_income = income_config.get(
            status
        )

        if (
            not isinstance(status_days, Mapping)
            or not isinstance(status_income, Mapping)
        ):
            raise ValueError(
                f"Employment parameters missing for {status}."
            )

        mean_days = float(
            status_days["mean"]
        )
        sd_days = float(
            status_days["sd"]
        )
        min_days = int(
            status_days["min"]
        )
        max_days = int(
            status_days["max"]
        )

        sampled_days = np.rint(
            rng.normal(
                mean_days,
                sd_days,
                size=n_selected,
            )
        )

        days[
            selected
        ] = np.clip(
            sampled_days,
            min_days,
            max_days,
        )

        income[
            selected
        ] = np.round(
            rng.lognormal(
                mean=float(
                    status_income[
                        "log_mean"
                    ]
                ),
                sigma=float(
                    status_income[
                        "log_sd"
                    ]
                ),
                size=n_selected,
            ),
            2,
        )

    keep = (
        active
        | (
            rng.random(
                len(population_truth)
            )
            < no_record_retention_probability
        )
    )

    source_population = (
        population_truth.loc[
            keep
        ]
        .reset_index(drop=True)
        .copy()
    )

    source_status = employment_status[
        keep
    ]
    source_days = days[
        keep
    ].copy()
    source_income = income[
        keep
    ].copy()

    n_source_records = len(
        source_population
    )

    days_missing = (
        rng.random(n_source_records)
        < missing_days_probability
    )

    income_missing = (
        rng.random(n_source_records)
        < missing_income_probability
    )

    source_days[
        days_missing
    ] = np.nan

    source_income[
        income_missing
    ] = np.nan

    contact_address_id = (
        generate_contact_addresses(
            source_population,
            address_register,
            config,
            contact_rng,
        )
    )

    employment_register = pd.DataFrame(
        {
            "person_id":
                source_population[
                    "person_id"
                ].to_numpy(dtype=object),
            "ref_date":
                pd.Timestamp(
                    employment_config.get(
                        "reference_date"
                    )
                ),
            "employment_status":
                source_status,
            "days_employed_last_12m":
                source_days,
            "annual_employment_income":
                source_income,
            "contact_address_id":
                contact_address_id,
        }
    )

    if employment_register[
        "person_id"
    ].duplicated().any():
        raise RuntimeError(
            "Employment register contains duplicated person IDs."
        )

    return employment_register[
        EMPLOYMENT_REGISTER_COLUMNS
    ]


def generate_tax_register(
    population_truth: pd.DataFrame,
    address_register: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
    contact_rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate an imperfect synthetic tax-register delivery."""

    required_columns = (
        POPULATION_TRUTH_REQUIRED_COLUMNS
        | {
            "age",
        }
    )

    missing_columns = (
        required_columns
        - set(population_truth.columns)
    )

    if missing_columns:
        raise ValueError(
            "Population truth is missing tax fields: "
            + ", ".join(sorted(missing_columns))
        )

    if population_truth["person_id"].duplicated().any():
        raise ValueError(
            "Population truth contains duplicated person IDs."
        )

    tax_config = config.get("tax")

    if not isinstance(tax_config, Mapping):
        raise ValueError(
            "Configuration must contain a tax section."
        )

    tax_year = tax_config.get("tax_year")

    if not isinstance(tax_year, int):
        raise ValueError(
            "tax_year must be an integer."
        )

    filing_config = tax_config.get(
        "filing_probabilities"
    )

    if not isinstance(filing_config, Mapping):
        raise ValueError(
            "tax must define filing_probabilities."
        )

    filing_probabilities = {
        name: _validate_probability(
            filing_config.get(name),
            name,
        )
        for name in (
            "nonresident",
            "resident_18_24",
            "resident_25_39",
            "resident_40_64",
            "resident_65_79",
            "resident_80_plus",
            "fallback",
        )
    }

    income_config = tax_config.get(
        "taxable_income"
    )

    if not isinstance(income_config, Mapping):
        raise ValueError(
            "tax must define taxable_income."
        )

    required_income_groups = (
        "age_18_24",
        "age_25_64",
        "age_65_plus",
    )

    for group in required_income_groups:
        parameters = income_config.get(group)

        if not isinstance(parameters, Mapping):
            raise ValueError(
                f"Taxable-income parameters missing for {group}."
            )

        if (
            "log_mean" not in parameters
            or "log_sd" not in parameters
        ):
            raise ValueError(
                f"Taxable-income parameters incomplete for {group}."
            )

    nonfiler_retention_probability = (
        _validate_probability(
            tax_config.get(
                "nonfiler_retention_probability"
            ),
            "nonfiler_retention_probability",
        )
    )

    missing_income_probability = (
        _validate_probability(
            tax_config.get(
                "missing_income_probability"
            ),
            "tax missing_income_probability",
        )
    )

    negative_income_probability = (
        _validate_probability(
            tax_config.get(
                "negative_income_probability"
            ),
            "negative_income_probability",
        )
    )

    ages = population_truth[
        "age"
    ].to_numpy(dtype=np.int64)

    true_resident = population_truth[
        "true_resident"
    ].to_numpy(dtype=np.int64)

    filing_probability = np.full(
        len(population_truth),
        filing_probabilities["fallback"],
        dtype=float,
    )

    filing_probability[
        true_resident == 0
    ] = filing_probabilities[
        "nonresident"
    ]

    resident = (
        true_resident == 1
    )

    filing_probability[
        resident
        & (ages >= 18)
        & (ages <= 24)
    ] = filing_probabilities[
        "resident_18_24"
    ]

    filing_probability[
        resident
        & (ages >= 25)
        & (ages <= 39)
    ] = filing_probabilities[
        "resident_25_39"
    ]

    filing_probability[
        resident
        & (ages >= 40)
        & (ages <= 64)
    ] = filing_probabilities[
        "resident_40_64"
    ]

    filing_probability[
        resident
        & (ages >= 65)
        & (ages <= 79)
    ] = filing_probabilities[
        "resident_65_79"
    ]

    filing_probability[
        resident
        & (ages >= 80)
    ] = filing_probabilities[
        "resident_80_plus"
    ]

    filer = (
        rng.random(
            len(population_truth)
        )
        < filing_probability
    )

    taxable_income = np.zeros(
        len(population_truth),
        dtype=float,
    )

    income_groups = (
        (
            (ages >= 18)
            & (ages <= 24),
            "age_18_24",
        ),
        (
            (ages >= 25)
            & (ages <= 64),
            "age_25_64",
        ),
        (
            ages >= 65,
            "age_65_plus",
        ),
    )

    for age_mask, group in income_groups:
        selected = (
            filer
            & age_mask
        )

        n_selected = int(
            selected.sum()
        )

        if n_selected == 0:
            continue

        parameters = income_config[
            group
        ]

        taxable_income[
            selected
        ] = np.round(
            rng.lognormal(
                mean=float(
                    parameters[
                        "log_mean"
                    ]
                ),
                sigma=float(
                    parameters[
                        "log_sd"
                    ]
                ),
                size=n_selected,
            ),
            2,
        )

    keep = (
        filer
        | (
            rng.random(
                len(population_truth)
            )
            < nonfiler_retention_probability
        )
    )

    source_population = (
        population_truth.loc[
            keep
        ]
        .reset_index(drop=True)
        .copy()
    )

    source_filing_flag = (
        filer[
            keep
        ].astype(np.int64)
    )

    source_income = taxable_income[
        keep
    ].copy()

    n_source_records = len(
        source_population
    )

    income_missing = (
        rng.random(
            n_source_records
        )
        < missing_income_probability
    )

    source_income[
        income_missing
    ] = np.nan

    negative_income = (
        rng.random(
            n_source_records
        )
        < negative_income_probability
    )

    source_income[
        negative_income
    ] = -np.abs(
        source_income[
            negative_income
        ]
    )

    contact_address_id = (
        generate_contact_addresses(
            source_population,
            address_register,
            config,
            contact_rng,
        )
    )

    tax_register = pd.DataFrame(
        {
            "person_id":
                source_population[
                    "person_id"
                ].to_numpy(dtype=object),
            "tax_year":
                np.full(
                    n_source_records,
                    tax_year,
                    dtype=np.int64,
                ),
            "tax_filing_flag":
                source_filing_flag,
            "taxable_income":
                source_income,
            "contact_address_id":
                contact_address_id,
        }
    )

    if tax_register[
        "person_id"
    ].duplicated().any():
        raise RuntimeError(
            "Tax register contains duplicated person IDs."
        )

    return tax_register[
        TAX_REGISTER_COLUMNS
    ]


def generate_education_register(
    population_truth: pd.DataFrame,
    address_register: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
    contact_rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate a synthetic educational-participation register."""

    required_columns = (
        POPULATION_TRUTH_REQUIRED_COLUMNS
        | {
            "age",
        }
    )

    missing_columns = (
        required_columns
        - set(population_truth.columns)
    )

    if missing_columns:
        raise ValueError(
            "Population truth is missing education-participation fields: "
            + ", ".join(sorted(missing_columns))
        )

    if population_truth["person_id"].duplicated().any():
        raise ValueError(
            "Population truth contains duplicated person IDs."
        )

    education_config = config.get(
        "education_participation"
    )

    if not isinstance(education_config, Mapping):
        raise ValueError(
            "Configuration must contain "
            "an education_participation section."
        )

    school_year = education_config.get(
        "school_year"
    )

    if not isinstance(school_year, str) or not school_year:
        raise ValueError(
            "education_participation.school_year "
            "must be a non-empty string."
        )

    enrolment_config = education_config.get(
        "enrolment_probabilities"
    )

    if not isinstance(enrolment_config, Mapping):
        raise ValueError(
            "education_participation must define "
            "enrolment_probabilities."
        )

    enrolment_probabilities = {
        name: _validate_probability(
            enrolment_config.get(name),
            name,
        )
        for name in (
            "nonresident",
            "resident_6_15",
            "resident_16_17",
            "resident_18_24",
            "resident_25_30",
            "fallback",
        )
    }

    institution_config = education_config.get(
        "institution_probabilities"
    )

    if not isinstance(institution_config, Mapping):
        raise ValueError(
            "education_participation must define "
            "institution_probabilities."
        )

    school_probabilities = (
        _validated_probability_vector(
            institution_config.get(
                "age_6_17"
            ),
            (
                "school",
                "vocational_school",
            ),
            "age_6_17 institution probabilities",
        )
    )

    adult_probabilities = (
        _validated_probability_vector(
            institution_config.get(
                "age_18_30"
            ),
            (
                "university",
                "vocational_school",
            ),
            "age_18_30 institution probabilities",
        )
    )

    missing_institution_probability = (
        _validate_probability(
            education_config.get(
                "missing_institution_probability"
            ),
            "missing_institution_probability",
        )
    )

    ages = population_truth[
        "age"
    ].to_numpy(dtype=np.int64)

    true_resident = population_truth[
        "true_resident"
    ].to_numpy(dtype=np.int64)

    enrolment_probability = np.full(
        len(population_truth),
        enrolment_probabilities["fallback"],
        dtype=float,
    )

    enrolment_probability[
        true_resident == 0
    ] = enrolment_probabilities[
        "nonresident"
    ]

    resident = (
        true_resident == 1
    )

    enrolment_probability[
        resident
        & (ages >= 6)
        & (ages <= 15)
    ] = enrolment_probabilities[
        "resident_6_15"
    ]

    enrolment_probability[
        resident
        & (ages >= 16)
        & (ages <= 17)
    ] = enrolment_probabilities[
        "resident_16_17"
    ]

    enrolment_probability[
        resident
        & (ages >= 18)
        & (ages <= 24)
    ] = enrolment_probabilities[
        "resident_18_24"
    ]

    enrolment_probability[
        resident
        & (ages >= 25)
        & (ages <= 30)
    ] = enrolment_probabilities[
        "resident_25_30"
    ]

    enrolled = (
        rng.random(
            len(population_truth)
        )
        < enrolment_probability
    )

    source_population = (
        population_truth.loc[
            enrolled
        ]
        .reset_index(drop=True)
        .copy()
    )

    source_ages = source_population[
        "age"
    ].to_numpy(dtype=np.int64)

    n_source_records = len(
        source_population
    )

    institution_type = np.full(
        n_source_records,
        pd.NA,
        dtype=object,
    )

    school_age = (
        (source_ages >= 6)
        & (source_ages <= 17)
    )

    n_school_age = int(
        school_age.sum()
    )

    if n_school_age > 0:
        institution_type[
            school_age
        ] = rng.choice(
            (
                "school",
                "vocational_school",
            ),
            size=n_school_age,
            replace=True,
            p=school_probabilities,
        )

    adult_age = (
        (source_ages >= 18)
        & (source_ages <= 30)
    )

    n_adult_age = int(
        adult_age.sum()
    )

    if n_adult_age > 0:
        institution_type[
            adult_age
        ] = rng.choice(
            (
                "university",
                "vocational_school",
            ),
            size=n_adult_age,
            replace=True,
            p=adult_probabilities,
        )

    institution_missing = (
        rng.random(
            n_source_records
        )
        < missing_institution_probability
    )

    institution_type[
        institution_missing
    ] = pd.NA

    contact_address_id = (
        generate_contact_addresses(
            source_population,
            address_register,
            config,
            contact_rng,
        )
    )

    education_register = pd.DataFrame(
        {
            "person_id":
                source_population[
                    "person_id"
                ].to_numpy(dtype=object),
            "school_year":
                np.full(
                    n_source_records,
                    school_year,
                    dtype=object,
                ),
            "enrolment_flag":
                np.ones(
                    n_source_records,
                    dtype=np.int64,
                ),
            "institution_type":
                institution_type,
            "contact_address_id":
                contact_address_id,
        }
    )

    if education_register[
        "person_id"
    ].duplicated().any():
        raise RuntimeError(
            "Education register contains duplicated person IDs."
        )

    return education_register[
        EDUCATION_REGISTER_COLUMNS
    ]
