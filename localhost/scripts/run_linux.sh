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
# Sparse Telegram updates using the existing webMoFuSS bot.
MOFUSS_TELEGRAM_MSGS="${MOFUSS_TELEGRAM_MSGS:-1}" # 0 disables notifications
MOFUSS_TELEGRAM_INTERVAL_MINUTES="${MOFUSS_TELEGRAM_INTERVAL_MINUTES:-30}"
# Blank discovers the repository .env. Set an absolute path on other computers
# if the repository lives elsewhere. Credentials are never copied to scenarios.
MOFUSS_TELEGRAM_ENV_FILE="${MOFUSS_TELEGRAM_ENV_FILE:-}"
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
export MOFUSS_TELEGRAM_MSGS MOFUSS_TELEGRAM_INTERVAL_MINUTES MOFUSS_TELEGRAM_ENV_FILE
export MOFUSS_RUN_FROM="$PWD"
# Keep R on the host libraries, independently of the EGO AppImage libraries.
export LD_LIBRARY_PATH="${MOFUSS_R_LIBRARY_PATH:-}"
unset PYTHONHOME PYTHONPATH
cd -- "$MOFUSS_SCENARIO_DIR"
ulimit -c 0
# Embedded notifier keeps run_linux.sh the only file needed for this upgrade.
# The existing Python launcher still runs the model and writes its normal logs.
exec python3 -B - "$MOFUSS_SCENARIO_DIR" "$@" <<'MOFUSS_TELEGRAM_PY'
import csv
import json
import os
from pathlib import Path
import re
import signal
import socket
import subprocess
import sys
import threading
import time
import urllib.request


SECRET_KEYS = ('MOFUSS_TELEGRAM_BOT_TOKEN', 'MOFUSS_TELEGRAM_CHAT_ID')


def read_credentials(scenario):
    """Read only two .env values as data; never execute the file as shell code."""
    explicit = os.environ.get('MOFUSS_TELEGRAM_ENV_FILE', '')
    if explicit:
        candidates = [Path(explicit).expanduser()]
    else:
        candidates = []
        for parent in (scenario, *scenario.parents):
            candidates.extend((parent / '.env', parent / 'localhost/scripts/.env',
                               parent / 'mofuss/.env', parent / 'mofuss/localhost/scripts/.env'))
        home_repo = Path.home() / 'Documents/mofuss'
        candidates.extend((home_repo / '.env', home_repo / 'localhost/scripts/.env'))
    values = {key: os.environ[key] for key in SECRET_KEYS if key in os.environ}
    if len(values) == len(SECRET_KEYS):
        return values
    for path in dict.fromkeys(candidates):
        try:
            lines = path.read_text(encoding='utf-8-sig').splitlines()
        except (OSError, UnicodeError):
            continue
        found = {}
        for line in lines:
            key, separator, value = line.strip().removeprefix('export ').partition('=')
            key = key.strip()
            if not separator or key not in SECRET_KEYS:
                continue
            value = value.strip()
            if value.startswith(('"', "'")):
                quote = value[0]
                end = value.find(quote, 1)
                if end < 0 or (value[end + 1:].strip() and not value[end + 1:].lstrip().startswith('#')):
                    continue
                value = value[1:end]
            else:
                value = re.split(r'\s+#', value, maxsplit=1)[0].strip()
            found[key] = value
        if all(key in values or found.get(key) for key in SECRET_KEYS):
            for key, value in found.items():
                values.setdefault(key, value)
            break
    return values


class Telegram:
    def __init__(self, credentials):
        self.credentials = credentials
        self.warned = False

    def send(self, text):
        try:
            token = self.credentials[SECRET_KEYS[0]]
            payload = json.dumps({'chat_id': self.credentials[SECRET_KEYS[1]],
                                  'text': text, 'disable_web_page_preview': True}).encode()
            request = urllib.request.Request(
                'https://api.telegram.org/bot' + token + '/sendMessage',
                data=payload, headers={'Content-Type': 'application/json'}, method='POST')
            with urllib.request.urlopen(request, timeout=8) as response:
                if not json.load(response).get('ok'):
                    raise ValueError('Telegram rejected the message')
            return True
        except Exception:
            # Never print an exception: HTTP errors can include the secret URL.
            if not self.warned:
                print('Telegram notification unavailable; the simulation continues normally.',
                      file=sys.stderr, flush=True)
                self.warned = True
            return False


def duration(seconds):
    minutes = int(seconds // 60)
    return f'{minutes // 60}h {minutes % 60:02d}m' if minutes >= 60 else f'{minutes}m'


def latest_harvest(scenario, started):
    """Describe a saved output, without claiming a whole MC/report is finished."""
    try:
        parameters = scenario / 'LULCC/TempTables/parameters_dinamica.csv'
        with parameters.open(newline='') as stream:
            values = {row['Var']: row['ParCHR'] for row in csv.DictReader(stream)}
        start_year = int(values['start_year'])
        latest = None
        for path in scenario.glob('debugging_*/Harvest_tot*.tif'):
            match = re.fullmatch(r'debugging_(\d+)/Harvest_tot(\d+)\.tif',
                                 path.relative_to(scenario).as_posix())
            if not match or path.stat().st_mtime < started:
                continue
            candidate = (int(match[1]), int(match[2]))
            if latest is None or candidate > latest:
                latest = candidate
        if latest:
            return f'Latest saved harvest: MC{latest[0]}, year {start_year + latest[1] - 1}.'
    except (OSError, ValueError, KeyError):
        pass
    return 'Simulation or reporting is still running.'


def main(scenario, args):
    command = [sys.executable, '-B', str(scenario / 'run_linux.py'),
               '--scenario', str(scenario), *args]
    enabled = os.environ.get('MOFUSS_TELEGRAM_MSGS', '1').lower() in ('1', 'true', 'yes')
    test = args == ['--telegram-test']
    # Check/help commands produce no Telegram traffic.
    if not test and (not enabled or any(arg in ('--check', '--help', '-h') for arg in args)):
        os.execv(sys.executable, command)
    credentials = read_credentials(scenario)
    if not all(credentials.get(key) for key in SECRET_KEYS):
        print('Telegram notifications disabled: credentials were not found in the repository .env.',
              file=sys.stderr, flush=True)
        if test:
            return 1
        os.execv(sys.executable, command)
    notifier = Telegram(credentials)
    identity = f'{scenario.name} · {socket.gethostname()}'
    if test:
        sent = notifier.send(f'MoFuSS Telegram test · {socket.gethostname()}\n'
                             'Linux run notifications are ready. No simulation was started.')
        print('Telegram test delivered.' if sent else 'Telegram test failed.', flush=True)
        return 0 if sent else 1
    try:
        interval = float(os.environ.get('MOFUSS_TELEGRAM_INTERVAL_MINUTES', '30')) * 60
        if not 60 <= interval <= 86400:
            raise ValueError()
    except ValueError:
        print('Invalid Telegram interval; using 30 minutes.', file=sys.stderr, flush=True)
        interval = 1800
    started = time.time()
    clock = time.monotonic()
    stop = threading.Event()

    def updates():
        notifier.send(f'MoFuSS started\n{identity}')
        while not stop.wait(interval):
            notifier.send(f'MoFuSS running · {duration(time.monotonic() - clock)}\n'
                          f'{identity}\n{latest_harvest(scenario, started)}')

    child = subprocess.Popen(command, start_new_session=True)
    thread = threading.Thread(target=updates, daemon=True)
    thread.start()
    interrupted = False

    def interrupt(signum, frame):
        nonlocal interrupted
        interrupted = True
        # run_linux.py handles SIGINT and stops its current engine/R group.
        if child.poll() is None:
            child.send_signal(signal.SIGINT)

    signal.signal(signal.SIGINT, interrupt)
    signal.signal(signal.SIGTERM, interrupt)
    try:
        status = child.wait()
    finally:
        stop.set()
        thread.join(timeout=9)
    elapsed = duration(time.monotonic() - clock)
    if interrupted or status in (130, -signal.SIGINT, -signal.SIGTERM):
        notifier.send(f'MoFuSS stopped · {elapsed}\n{identity}')
    elif status == 0:
        notifier.send(f'MoFuSS completed · {elapsed}\n{identity}\nSimulation and reporting finished.')
    else:
        notifier.send(f'MoFuSS failed · {elapsed}\n{identity}\nExit {status}; see the working folder Logs.')
    return 130 if interrupted else (status if status >= 0 else 128 - status)


if __name__ == '__main__':
    raise SystemExit(main(Path(sys.argv[1]).resolve(), sys.argv[2:]))
MOFUSS_TELEGRAM_PY
