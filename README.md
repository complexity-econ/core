# SFC-ABM Core

Stock-Flow Consistent Agent-Based Model engine for macroeconomic simulation.

## Overview

A type-safe, bilaterally consistent simulation engine where every monetary flow has a source and a sink. Built for studying economic phase transitions, non-ergodicity, and emergent dynamics in heterogeneous agent networks.

## Architecture

- **Engine** — Main ABM loop with SFC consistency checks
- **Agents** — Firm (with technology adoption decisions), CentralBank (Taylor rule), Household (labor supply)
- **Networks** — Watts-Strogatz small-world topology for information diffusion
- **SFC** — Balance sheet matrix ensuring stock-flow consistency across all sectors
- **Config** — HOCON-based experiment configuration

## Tech Stack

- Scala 3 / sbt
- Ammonite for scripting

## Status

🚧 Extracting core engine from [paper-01](https://github.com/complexity-econ/paper-01-acceleration-paradox) monolithic simulation. Placeholder structure — implementation in progress.

## Citation

See [CITATION.cff](CITATION.cff)

## License

MIT
