#!/usr/bin/env bash
# Source this file from the Linux launchers. Machine paths live outside git.
MOFUSS_LOCAL_CONFIG="${MOFUSS_LINUX_CONFIG:-${XDG_CONFIG_HOME:-$HOME/.config}/mofuss/linux-runtime.env}"
if [[ -f "$MOFUSS_LOCAL_CONFIG" ]]; then
  source "$MOFUSS_LOCAL_CONFIG"
fi
if [[ -n "${MOFUSS_RUNTIME_DIR:-}" ]]; then
  if [[ ! -f "$MOFUSS_RUNTIME_DIR/tools/env.sh" ]]; then
    printf 'Missing runtime environment: %s/tools/env.sh\n' "$MOFUSS_RUNTIME_DIR" >&2
    return 1
  fi
  source "$MOFUSS_RUNTIME_DIR/tools/env.sh"
  export MOFUSS_RUNTIME_DIR
  export MOFUSS_EGO="${MOFUSS_EGO:-$MOFUSS_RUNTIME_DIR/downloads/DinamicaEGO-8130-Ubuntu-LTS.AppImage}"
fi
# Native installations can instead set MOFUSS_EGO and configure their own PATH.
if [[ -z "${MOFUSS_EGO:-}" ]]; then
  MOFUSS_EGO="$(command -v DinamicaConsole || true)"
  export MOFUSS_EGO
fi
if [[ -z "${MOFUSS_EGO:-}" || ! -x "$MOFUSS_EGO" ]]; then
  printf 'Set MOFUSS_EGO to the EGO console/AppImage or MOFUSS_RUNTIME_DIR to the runtime bundle.\n' >&2
  return 1
fi
export MOFUSS_R="${MOFUSS_R:-$(command -v R || true)}"
if [[ -z "$MOFUSS_R" || ! -x "$MOFUSS_R" ]]; then
  printf 'R was not found; install R or set MOFUSS_R.\n' >&2
  return 1
fi
# R must not inherit the private GDAL/Qt/Python libraries injected by EGO.
export LD_LIBRARY_PATH="${MOFUSS_LIBRARY_PATH:-}"
unset PYTHONHOME PYTHONPATH
