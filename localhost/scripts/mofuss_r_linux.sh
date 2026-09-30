#!/usr/bin/env bash
# Called internally by the Linux EGO model; users start run_linux.sh.
set -euo pipefail
MOFUSS_SCENARIO_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
mkdir -p -- "$MOFUSS_SCENARIO_DIR/Logs"
exec 2>>"$MOFUSS_SCENARIO_DIR/Logs/linux_r_launcher.log"
trap 'printf "R launcher failed (status %s), script arguments: %s\n" "$?" "$*" >&2' ERR
MOFUSS_R="$(command -v -- "${MOFUSS_R:-R}" || true)"
if [[ -z "$MOFUSS_R" || ! -x "$MOFUSS_R" ]]; then
  printf 'R was not found. Install native R or set MOFUSS_R.\n' >&2
  exit 1
fi
MOFUSS_R="$(realpath -- "$MOFUSS_R")"
export LD_LIBRARY_PATH="${MOFUSS_R_LIBRARY_PATH:-}"
unset PYTHONHOME PYTHONPATH
export MOFUSS_SEED="${MOFUSS_SEED-20260929}"
export MOFUSS_PLOT_DPI="${MOFUSS_PLOT_DPI-1000}"
cd -- "$MOFUSS_SCENARIO_DIR"
exec "$MOFUSS_R" "$@"
