from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from simulation.education_sources import (
    BA_2024_COLUMNS,
    MIKROZENSUS_2024_COLUMNS,
    ZENSUS_2022_COLUMNS,
)
from simulation.education_truth import (
    EDUCATION_TRUTH_COLUMNS,
)
from simulation.generate import (
    DEFAULT_CONFIG_PATH,
    OUTPUT_PATHS,
    POPULATION_RNG_COMPONENT_IDS,
    build_persisted_population_truth,
    education_rng,
    generate_education_attainment_sources,
    generate_population_sources,
    generate_population_world,
    generate_simulation_outputs,
    load_config,
    population_rng,
    write_simulation_outputs,
)
from simulation.population_sources import (
    EDUCATION_REGISTER_COLUMNS,
    EMPLOYMENT_REGISTER_COLUMNS,
    POPULATION_REGISTER_COLUMNS,
    TAX_REGISTER_COLUMNS,
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


def test_generate_population_world_contract_and_counts() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    assert set(world) == {
        "regions",
        "age_bands",
        "address_register",
        "households",
        "true_residents",
        "former_residents",
        "population_truth",
    }

    n_true_residents = config[
        "population"
    ][
        "n_true_residents"
    ]

    n_former_residents = config[
        "population"
    ][
        "n_former_residents"
    ]

    assert len(
        world[
            "true_residents"
        ]
    ) == n_true_residents

    assert len(
        world[
            "former_residents"
        ]
    ) == n_former_residents

    assert len(
        world[
            "population_truth"
        ]
    ) == (
        n_true_residents
        + n_former_residents
    )

    assert world[
        "population_truth"
    ][
        "person_id"
    ].is_unique

    assert int(
        world[
            "population_truth"
        ][
            "true_resident"
        ].sum()
    ) == n_true_residents

    assert world[
        "households"
    ][
        "household_id"
    ].is_unique

    assert world[
        "address_register"
    ][
        "address_id"
    ].is_unique


def test_generate_population_world_is_reproducible() -> None:
    config = load_config()

    first = generate_population_world(
        config
    )

    second = generate_population_world(
        config
    )

    for key in (
        "regions",
        "age_bands",
        "address_register",
        "households",
        "true_residents",
        "former_residents",
        "population_truth",
    ):
        pd.testing.assert_frame_equal(
            first[key],
            second[key],
        )


def test_generate_population_world_does_not_write_files(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config = load_config()

    monkeypatch.chdir(
        tmp_path
    )

    generate_population_world(
        config
    )

    assert not list(
        tmp_path.rglob(
            "*.csv"
        )
    )


def test_generate_population_sources_contract() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    sources = generate_population_sources(
        config,
        world,
    )

    assert set(sources) == {
        "population_register",
        "employment_register",
        "tax_register",
        "education_register",
    }

    assert list(
        sources[
            "population_register"
        ].columns
    ) == POPULATION_REGISTER_COLUMNS

    assert list(
        sources[
            "employment_register"
        ].columns
    ) == EMPLOYMENT_REGISTER_COLUMNS

    assert list(
        sources[
            "tax_register"
        ].columns
    ) == TAX_REGISTER_COLUMNS

    assert list(
        sources[
            "education_register"
        ].columns
    ) == EDUCATION_REGISTER_COLUMNS


def test_generate_population_sources_person_scope() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    sources = generate_population_sources(
        config,
        world,
    )

    truth_ids = set(
        world[
            "population_truth"
        ][
            "person_id"
        ]
    )

    for source_name in (
        "population_register",
        "employment_register",
        "tax_register",
        "education_register",
    ):
        source = sources[
            source_name
        ]

        assert source[
            "person_id"
        ].is_unique

        assert set(
            source[
                "person_id"
            ]
        ).issubset(
            truth_ids
        )


def test_generate_population_sources_contact_addresses_are_valid() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    sources = generate_population_sources(
        config,
        world,
    )

    valid_address_ids = set(
        world[
            "address_register"
        ][
            "address_id"
        ]
    )

    for source_name in (
        "employment_register",
        "tax_register",
        "education_register",
    ):
        observed_addresses = set(
            sources[
                source_name
            ][
                "contact_address_id"
            ].dropna()
        )

        assert observed_addresses.issubset(
            valid_address_ids
        )


def test_generate_population_sources_are_reproducible() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    first = generate_population_sources(
        config,
        world,
    )

    second = generate_population_sources(
        config,
        world,
    )

    for source_name in (
        "population_register",
        "employment_register",
        "tax_register",
        "education_register",
    ):
        pd.testing.assert_frame_equal(
            first[
                source_name
            ],
            second[
                source_name
            ],
        )


def test_generate_population_sources_require_world_contract() -> None:
    config = load_config()

    with pytest.raises(
        ValueError,
        match="missing source-generation fields",
    ):
        generate_population_sources(
            config,
            {
                "population_truth": None,
            },
        )


def test_generate_population_sources_do_not_write_files(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    monkeypatch.chdir(
        tmp_path
    )

    generate_population_sources(
        config,
        world,
    )

    assert not list(
        tmp_path.rglob(
            "*.csv"
        )
    )


def test_education_rng_uses_configured_seeds() -> None:
    config = load_config()

    expected = {
        "education_truth": 2027,
        "zensus_2022": 20221,
        "ba_2024": 20241,
        "mikrozensus_2024": 20242,
    }

    for component, seed in expected.items():
        actual = education_rng(
            config,
            component,
        ).integers(
            0,
            1_000_000,
            size=20,
        )

        direct = np.random.default_rng(
            seed
        ).integers(
            0,
            1_000_000,
            size=20,
        )

        np.testing.assert_array_equal(
            actual,
            direct,
        )


def test_education_rng_rejects_unknown_component() -> None:
    config = load_config()

    with pytest.raises(
        ValueError,
        match="Unknown education RNG component",
    ):
        education_rng(
            config,
            "not_a_component",
        )


def test_generate_education_attainment_sources_contract() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    education = (
        generate_education_attainment_sources(
            config,
            world,
        )
    )

    assert set(education) == {
        "education_truth",
        "zensus_2022",
        "ba_2024",
        "mikrozensus_2024",
    }

    assert list(
        education[
            "education_truth"
        ].columns
    ) == EDUCATION_TRUTH_COLUMNS

    assert list(
        education[
            "zensus_2022"
        ].columns
    ) == ZENSUS_2022_COLUMNS

    assert list(
        education[
            "ba_2024"
        ].columns
    ) == BA_2024_COLUMNS

    assert list(
        education[
            "mikrozensus_2024"
        ].columns
    ) == MIKROZENSUS_2024_COLUMNS


def test_generate_education_attainment_sources_scope() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    education = (
        generate_education_attainment_sources(
            config,
            world,
        )
    )

    population_truth = world[
        "population_truth"
    ]

    true_resident_ids = set(
        population_truth.loc[
            population_truth[
                "true_resident"
            ].eq(1),
            "person_id",
        ]
    )

    education_truth = education[
        "education_truth"
    ]

    assert set(
        education_truth[
            "person_id"
        ]
    ).issubset(
        true_resident_ids
    )

    truth_2022_ids = set(
        education_truth.loc[
            education_truth[
                "reference_year"
            ].eq(2022),
            "person_id",
        ]
    )

    truth_2024_ids = set(
        education_truth.loc[
            education_truth[
                "reference_year"
            ].eq(2024),
            "person_id",
        ]
    )

    assert set(
        education[
            "zensus_2022"
        ][
            "person_id"
        ].dropna()
    ).issubset(
        truth_2022_ids
    )

    for source_name in (
        "ba_2024",
        "mikrozensus_2024",
    ):
        assert set(
            education[
                source_name
            ][
                "person_id"
            ].dropna()
        ).issubset(
            truth_2024_ids
        )


def test_generate_education_attainment_sources_are_reproducible() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    first = (
        generate_education_attainment_sources(
            config,
            world,
        )
    )

    second = (
        generate_education_attainment_sources(
            config,
            world,
        )
    )

    for source_name in (
        "education_truth",
        "zensus_2022",
        "ba_2024",
        "mikrozensus_2024",
    ):
        pd.testing.assert_frame_equal(
            first[
                source_name
            ],
            second[
                source_name
            ],
        )


def test_generate_education_attainment_sources_do_not_write_files(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    monkeypatch.chdir(
        tmp_path
    )

    generate_education_attainment_sources(
        config,
        world,
    )

    assert not list(
        tmp_path.rglob(
            "*.csv"
        )
    )


def test_generate_simulation_outputs_contract() -> None:
    config = load_config()

    outputs = generate_simulation_outputs(
        config
    )

    assert set(outputs) == set(
        OUTPUT_PATHS
    )

    for frame in outputs.values():
        assert isinstance(
            frame,
            pd.DataFrame,
        )

        assert len(frame) > 0


def test_generate_simulation_outputs_are_reproducible() -> None:
    config = load_config()

    first = generate_simulation_outputs(
        config
    )

    second = generate_simulation_outputs(
        config
    )

    for name in OUTPUT_PATHS:
        pd.testing.assert_frame_equal(
            first[name],
            second[name],
        )


def test_write_simulation_outputs_to_temporary_root(
    tmp_path: Path,
) -> None:
    config = load_config()

    outputs = generate_simulation_outputs(
        config
    )

    written = write_simulation_outputs(
        outputs,
        tmp_path,
    )

    assert set(written) == set(
        OUTPUT_PATHS
    )

    for name, relative_path in (
        OUTPUT_PATHS.items()
    ):
        expected_path = (
            tmp_path
            / relative_path
        )

        assert written[
            name
        ] == expected_path

        assert expected_path.is_file()

        round_trip = pd.read_csv(
            expected_path
        )

        assert list(
            round_trip.columns
        ) == list(
            outputs[
                name
            ].columns
        )

        assert len(
            round_trip
        ) == len(
            outputs[
                name
            ]
        )


def test_write_simulation_outputs_rejects_repository_root() -> None:
    config = load_config()

    outputs = generate_simulation_outputs(
        config
    )

    with pytest.raises(
        ValueError,
        match="Repository-root persistence is disabled",
    ):
        write_simulation_outputs(
            outputs,
            DEFAULT_CONFIG_PATH.parents[1],
        )


def test_write_simulation_outputs_requires_exact_contract(
    tmp_path: Path,
) -> None:
    config = load_config()

    outputs = generate_simulation_outputs(
        config
    )

    incomplete = dict(
        outputs
    )

    incomplete.pop(
        "tax_register"
    )

    with pytest.raises(
        ValueError,
        match="persistence contract",
    ):
        write_simulation_outputs(
            incomplete,
            tmp_path,
        )


def test_build_persisted_population_truth_contract() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    sources = generate_population_sources(
        config,
        world,
    )

    truth = build_persisted_population_truth(
        world[
            "population_truth"
        ],
        sources[
            "population_register"
        ],
    )

    assert list(
        truth.columns
    ) == [
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
        "in_population_register",
        "coverage_status_true",
        "overcoverage_flag_true",
        "undercoverage_flag_true",
        "registered_household_id_sim",
        "registered_address_id_sim",
        "registered_region_code_sim",
        "registered_municipality_code_sim",
    ]


def test_build_persisted_population_truth_matches_register_membership() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    sources = generate_population_sources(
        config,
        world,
    )

    truth = build_persisted_population_truth(
        world[
            "population_truth"
        ],
        sources[
            "population_register"
        ],
    )

    registered_ids = set(
        sources[
            "population_register"
        ][
            "person_id"
        ]
    )

    expected_membership = (
        truth[
            "person_id"
        ]
        .isin(
            registered_ids
        )
        .astype(int)
    )

    np.testing.assert_array_equal(
        truth[
            "in_population_register"
        ],
        expected_membership,
    )

    expected_status = np.select(
        [
            truth[
                "true_resident"
            ].eq(1)
            & expected_membership.eq(1),

            truth[
                "true_resident"
            ].eq(1)
            & expected_membership.eq(0),

            truth[
                "true_resident"
            ].eq(0)
            & expected_membership.eq(1),
        ],
        [
            "correctly_registered",
            "undercoverage",
            "overcoverage",
        ],
        default="correctly_absent",
    )

    np.testing.assert_array_equal(
        truth[
            "coverage_status_true"
        ],
        expected_status,
    )


def test_build_persisted_population_truth_registered_locations() -> None:
    config = load_config()

    world = generate_population_world(
        config
    )

    sources = generate_population_sources(
        config,
        world,
    )

    truth = build_persisted_population_truth(
        world[
            "population_truth"
        ],
        sources[
            "population_register"
        ],
    )

    residents = truth[
        "true_resident"
    ].eq(1)

    former = ~residents

    assert (
        truth.loc[
            residents,
            "registered_household_id_sim",
        ]
        .equals(
            truth.loc[
                residents,
                "true_household_id",
            ]
        )
    )

    assert (
        truth.loc[
            former,
            "registered_household_id_sim",
        ]
        .equals(
            truth.loc[
                former,
                "former_household_id",
            ]
        )
    )

    assert (
        truth.loc[
            residents,
            "registered_address_id_sim",
        ]
        .equals(
            truth.loc[
                residents,
                "true_address_id",
            ]
        )
    )

    assert (
        truth.loc[
            former,
            "registered_address_id_sim",
        ]
        .equals(
            truth.loc[
                former,
                "former_address_id",
            ]
        )
    )
