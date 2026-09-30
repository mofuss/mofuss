#!/usr/bin/env bash
set -euo pipefail
MOFUSS_SCENARIO_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

# BEGIN USER INPUTS ----------------------------------------------------------
# On each Linux computer, set the installed EGO console or AppImage here,
# or export MOFUSS_EGO in the terminal. Leave blank to use DinamicaConsole on PATH.
MOFUSS_EGO="${MOFUSS_EGO:-}"
# R and reporting tools (FFmpeg, TeX, zip) use the computer's normal installation.
MOFUSS_R="${MOFUSS_R:-R}"
MOFUSS_SEED="${MOFUSS_SEED-20260929}"
MOFUSS_PLOT_DPI="${MOFUSS_PLOT_DPI-1000}"
# END USER INPUTS ------------------------------------------------------------

if [[ -z "$MOFUSS_EGO" ]]; then
  MOFUSS_EGO="$(command -v DinamicaConsole || true)"
fi
if [[ -z "$MOFUSS_EGO" ]]; then
  printf 'Set MOFUSS_EGO in run_linux.sh to the installed Dinamica console/AppImage, or put DinamicaConsole on PATH.\n' >&2
  exit 1
fi
MOFUSS_EGO="$(command -v -- "$MOFUSS_EGO" || true)"
MOFUSS_R="$(command -v -- "$MOFUSS_R" || true)"
if [[ -z "$MOFUSS_EGO" || ! -x "$MOFUSS_EGO" || -z "$MOFUSS_R" || ! -x "$MOFUSS_R" ]]; then
  printf 'The configured Dinamica executable or R is unavailable on this computer.\n' >&2
  exit 1
fi
# Resolve user-supplied relative paths before changing to the working folder.
MOFUSS_EGO="$(realpath -- "$MOFUSS_EGO")"
MOFUSS_R="$(realpath -- "$MOFUSS_R")"
export MOFUSS_EGO MOFUSS_R MOFUSS_SEED MOFUSS_PLOT_DPI
export MOFUSS_RUN_FROM="$PWD"
# Keep R on the host libraries, independently of the EGO AppImage libraries.
export LD_LIBRARY_PATH="${MOFUSS_R_LIBRARY_PATH:-}"
unset PYTHONHOME PYTHONPATH
cd -- "$MOFUSS_SCENARIO_DIR"
ulimit -c 0
exec python3 -B "$MOFUSS_SCENARIO_DIR/run_linux.py" --scenario "$MOFUSS_SCENARIO_DIR" "$@"
