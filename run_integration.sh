#!/usr/bin/env bash
# Run integration tests with all mechanisms enabled
set -euo pipefail
OPEN_ECON=true \
NBP_QE=true \
NBP_FX_INTERVENTION=true \
IO_MODE=enabled \
IO_SCALE=0.5 \
sbt "testOnly sfc.engine.IntegrationFullSpec"
