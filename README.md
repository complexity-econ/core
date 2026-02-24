# SFC-ABM Core Engine

[![Core Engine Tests](https://github.com/complexity-econ/core/actions/workflows/core-tests.yml/badge.svg)](https://github.com/complexity-econ/core/actions/workflows/core-tests.yml)

Stock-Flow Consistent Agent-Based Model engine for studying phase transitions in AI-driven labor market automation. Powers all five papers in the [complexity-econ](https://github.com/complexity-econ) series (40,000+ Monte Carlo simulations).

## What it does

Simulates **10,000 heterogeneous firms** across **6 sectors** (GUS 2024 calibration) interacting on a configurable network topology over 120 months. Firms decide whether to adopt AI, go hybrid, or stay traditional based on profitability thresholds, network mimetic pressure, and sector-specific CES elasticity of substitution (σ). A BDP (basic disposable payment) shock at month 30 triggers the automation transition.

Every monetary flow is tracked bilaterally: government taxes and spends, banks lend and absorb losses, the central bank sets rates via Taylor rule, and the exchange rate floats via balance-of-payments (PLN) or stays fixed under SGP fiscal constraint (EUR).

## Architecture

```
src/main/scala/sfc/
├── Main.scala                    Entry point + Monte Carlo orchestrator
├── config/
│   └── SimConfig.scala           40+ parameters, 2 regimes, 4 topologies, env var overrides
├── agents/
│   ├── Firm.scala                TechState (Traditional→Hybrid→Automated→Bankrupt), decision logic
│   ├── Household.scala           Aggregate household state
│   └── CentralBank.scala         NBP/ECB rate wrapper
├── engine/
│   ├── Simulation.scala          Monthly step: labor → demand → firms → bank → inflation → forex → fiscal
│   └── World.scala               Immutable world state (macro + per-sector σ vector)
├── dynamics/
│   ├── SigmaDynamics.scala       Endogenous σ evolution (Arthur-style learning-by-doing)
│   └── DynamicNetwork.scala      Death-birth rewiring with preferential attachment
├── networks/
│   └── WattsStrogatz.scala       WS, Erdos-Renyi, Barabasi-Albert, ring lattice generators
└── sfc/
    └── BalanceSheet.scala        GovState, BankState, ForexState (SFC accounting)
```

## Key features

### Monetary regimes
- **PLN**: Floating exchange rate, endogenous NBP Taylor rule (α=1.5, β=0.8, inertia=0.70)
- **EUR**: Fixed rate at 4.33 PLN/EUR, exogenous ECB Taylor rule, SGP fiscal constraint (3% deficit, 60% debt ceiling, austerity κ=2.0)

### Network topologies
- **Watts-Strogatz** small-world (k=6, p=0.10) — default
- **Erdos-Renyi** random graph
- **Barabasi-Albert** scale-free (preferential attachment)
- **Ring lattice** (no rewiring)

### Firm decision logic
- 4-state technology transition: Traditional → Hybrid → Automated (+ Bankrupt)
- Profitability threshold via CES elasticity: `θ = min(1.0, 0.88 + 0.075·log₁₀(σ))`
- Network mimetic pressure: 40% weight on neighbors' adoption + 40% global panic
- Uncertainty discount reduced by demonstration effect (neighbors already automated)
- Sector-specific digital readiness, failure rates, and hybrid worker retention

### Endogenous dynamics (Paper-05)
- **σ evolution**: `σ(t+1) = σ(t) + λ·σ(t)·adoption(t)·(1 - σ(t)/cap)` with ratchet + hard cap
- **Network rewiring**: bankrupt firms replaced with probability ρ, new entrants wire via preferential attachment
- Both disabled by default (λ=0, ρ=0) — backward compatible with Papers 01–04

### Macroeconomic channels
- Phillips curve inflation (demand-pull + cost-push + import + tech-deflation)
- Soft deflation floor at -1.5%/mo (Bewley 1999 wage rigidity)
- Banking sector with NPL tracking, CAR constraint, endogenous lending rates
- Balance-of-payments forex with IRP arbitrage (PLN) or fixed rate (EUR)
- Government fiscal balance with SGP constraints (EUR only)

## Sectors (GUS 2024)

| Sector | Share | σ (CES) | Digital readiness | Hybrid retain |
|--------|------:|--------:|------------------:|--------------:|
| BPO/SSC | 3% | 50.0 | 0.50 | 50% |
| Manufacturing | 16% | 10.0 | 0.45 | 60% |
| Retail/Services | 45% | 5.0 | 0.40 | 65% |
| Healthcare | 6% | 2.0 | 0.25 | 75% |
| Public | 22% | 1.0 | 0.08 | 90% |
| Agriculture | 8% | 3.0 | 0.12 | 85% |

## Usage

### Prerequisites

- JDK 17+
- [sbt](https://www.scala-sbt.org/)

### Run simulation

```bash
# BDP = 2000 PLN, 30 seeds, PLN regime
sbt "run 2000 30 baseline pln"

# EUR regime with SGP
sbt "run 2000 30 baseline eur"

# Fat JAR (faster, no sbt startup overhead)
sbt assembly
java -jar target/scala-3.5.2/sfc-abm.jar 2000 30 baseline pln
```

### Environment variable overrides

```bash
# Network topology
TOPOLOGY=er java -jar sfc-abm.jar 2000 30 test pln       # Erdos-Renyi
TOPOLOGY=ba java -jar sfc-abm.jar 2000 30 test pln       # Barabasi-Albert
TOPOLOGY=lattice java -jar sfc-abm.jar 2000 30 test pln  # Ring lattice

# Custom σ per sector
SIGMAS="8.97,4.88,4.95,1.0,0.8,1.44" java -jar sfc-abm.jar 2000 30 test pln

# σ multiplier (all sectors)
SIGMA_MULT=2.0 java -jar sfc-abm.jar 2000 30 test pln

# System size
FIRMS_COUNT=1000 java -jar sfc-abm.jar 2000 30 test pln

# Decision rule tuning
DEMO_THRESH=0.25 java -jar sfc-abm.jar 2000 30 test pln

# Endogenous σ (Paper-05)
SIGMA_LAMBDA=0.02 java -jar sfc-abm.jar 2000 30 test pln

# Dynamic network (Paper-05)
REWIRE_RHO=0.1 java -jar sfc-abm.jar 2000 30 test pln

# Full endogenous (both mechanisms)
SIGMA_LAMBDA=0.02 REWIRE_RHO=0.1 java -jar sfc-abm.jar 2000 30 test pln
```

### Output

CSV files written to `mc/` (European format: semicolon separator, comma decimals):

- `mc/<prefix>_terminal.csv` — per-seed terminal values at month 120 (26 columns)
- `mc/<prefix>_timeseries.csv` — aggregated time series (mean, std, p05, p95)

**26 output columns**: Seed, Inflation, Unemployment, TotalAdoption, ExRate, MarketWage, GovDebt, NPL, RefRate, PriceLevel, AutoRatio, HybridRatio, 6× sector adoption, EffectiveBDP, 6× sector σ, MeanDegree

## Tests

93 tests across 8 suites, run on every push via GitHub Actions:

```bash
sbt test
```

| Suite | Tests | Covers |
|-------|------:|--------|
| FirmSpec | 14 | Tech states, worker counts, capacity, σ threshold |
| SimConfigSpec | 12 | Sector calibration, parameter bounds, regime detection |
| NetworkSpec | 10 | WS/ER/BA/lattice degree, symmetry, connectivity |
| BalanceSheetSpec | 10 | NPL ratio, CAR, lending rate, credit constraint |
| SimulationSpec | 12 | Labor market, inflation channels, Taylor rules |
| SigmaDynamicsSpec | 6 | Learning-by-doing, ratchet, cap, backward compat |
| DynamicNetworkSpec | 6 | Death-birth, preferential attachment, backward compat |
| IntegrationSpec | 23 | End-to-end: 120×26 output, reproducibility, bounds |

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
