def test_simulation_modules_import() -> None:
    from simulation import education_sources
    from simulation import education_truth
    from simulation import generate
    from simulation import population_sources
    from simulation import world

    assert world is not None
    assert population_sources is not None
    assert education_truth is not None
    assert education_sources is not None
    assert generate is not None
