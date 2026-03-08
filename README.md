# SFC-ABM Core Engine

[![Core Engine Tests](https://github.com/complexity-econ/core/actions/workflows/core-tests.yml/badge.svg)](https://github.com/complexity-econ/core/actions/workflows/core-tests.yml)

Stock-Flow Consistent Agent-Based Model engine for studying phase transitions in AI-driven labor market automation. Powers all five papers in the [complexity-econ](https://github.com/complexity-econ) series (40,000+ Monte Carlo simulations).

## Usage

### Prerequisites

- JDK 17+
- [sbt](https://www.scala-sbt.org/)

## Tech stack

- **Scala 3.5.2** / sbt
- **Zero runtime dependencies** (stdlib only)
- **ScalaTest 3.2.19** for unit tests
- **sbt-assembly** for fat JAR packaging

## Papers using this engine

| # | Paper | Sims | DOI |
|---|-------|-----:|-----|
| 1 | [The Acceleration Paradox](https://github.com/complexity-econ/paper-01-acceleration-paradox) | 6,300 | [10.5281/zenodo.18727928](https://doi.org/10.5281/zenodo.18727928) |
| 2 | [PLN vs EUR with SGP](https://github.com/complexity-econ/paper-02-monetary-regimes) | 1,260 | [10.5281/zenodo.18740933](https://doi.org/10.5281/zenodo.18740933) |
| 3 | [Empirical σ Estimation](https://github.com/complexity-econ/paper-03-empirical-sigma) | 120 | [10.5281/zenodo.18743780](https://doi.org/10.5281/zenodo.18743780) |
| 4 | [Phase Diagram & Universality](https://github.com/complexity-econ/paper-04-phase-diagram) | 18,540 | [10.5281/zenodo.18751083](https://doi.org/10.5281/zenodo.18751083) |
| 5 | [Endogenous Technology & Networks](https://github.com/complexity-econ/paper-05-endogenous) | 10,080 | [10.5281/zenodo.18758365](https://doi.org/10.5281/zenodo.18758365) |

## Citation

See [CITATION.cff](CITATION.cff)

## License

MIT
