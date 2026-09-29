#!/usr/bin/env bash
# Copy current code, prepare scenario inputs, and validate; does not simulate.
set -euo pipefail
scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
if [[ $# -lt 1 || $# -gt 2 || "${1:-}" == "--help" ]]; then
  printf 'Usage: bash %s WORKING_FOLDER [RUNTIME_BUNDLE]
' "$0"
  printf 'Quote paths containing spaces. Runtime can also use MOFUSS_EGO or the machine configuration.
'
  [[ "${1:-}" == "--help" ]] && exit 0
  exit 2
fi
scenario_dir="$(cd -- "$1" && pwd)"
if [[ $# == 2 ]]; then
  export MOFUSS_RUNTIME_DIR="$(cd -- "$2" && pwd)"
fi
# Validate runtime before replacing code in the working folder.
source "$scripts_dir/mofuss_linux_env.sh"
"$MOFUSS_R" --vanilla --slave --args "$scripts_dir" "$scenario_dir" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
source(file.path(args[[1]], "deploy_runtime_bundle_v1.R"))
mofuss_copy_runtime_bundle(args[[1]], args[[2]])
RSCRIPT
bash "$scenario_dir/run_linux.sh" --prepare-inputs
bash "$scenario_dir/run_linux.sh" --check
printf '\nPreparation and checks passed. To start the simulation:\n'
printf 'bash %q\n' "$scenario_dir/run_linux.sh"
