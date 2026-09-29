#!/usr/bin/env bash
set -euo pipefail
MOFUSS_SCENARIO_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
source "$MOFUSS_SCENARIO_DIR/mofuss_linux_env.sh"
export MOFUSS_RUN_FROM="$PWD"
export MOFUSS_SEED="${MOFUSS_SEED-20260929}"
export MOFUSS_PLOT_DPI="${MOFUSS_PLOT_DPI-1000}"
cd -- "$MOFUSS_SCENARIO_DIR"
ulimit -c 0
exec python3 "$MOFUSS_SCENARIO_DIR/run_linux.py" --scenario "$MOFUSS_SCENARIO_DIR" "$@"
