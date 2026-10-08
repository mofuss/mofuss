"""Stage/run the legacy web model against frozen inputs in disposable storage.

This checks Dinamica dynamics only: the four external R initialization/reporting
calls are omitted identically in both models by the existing regression harness.
It does not certify the complete web workflow or an untested engine version.
"""
from __future__ import annotations

import argparse
import importlib.util
import json
import shutil
import sys
from pathlib import Path

sys.dont_write_bytecode = True
REPO = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "frozen_regression", REPO / "localhost/scripts/tests/dinamica_runtime_regression_v1.py"
)
regression = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(regression)


def expand_decennial_idw(target: Path, years: int) -> None:
    """Optional test-data adapter, never a production model transformation.

    New localhost datasets contain years 01,11,21,...; v3 requests every year.
    Fill absent annual files from that decade's map in this fixture only.
    Existing annual files are retained. All copies and hashes are recorded.
    """
    changes = []
    frozen_path = target / "frozen_input_hashes.json"
    frozen = json.loads(frozen_path.read_text())
    for channel in ("v", "w"):
        for year in range(1, years + 1):
            destination = target / "In" / f"IDW_C++_fw_{channel}{year:02d}.tif"
            if destination.exists():
                continue
            period = ((year - 1) // 10) * 10 + 1
            source = target / "In" / f"IDW_C++_fw_{channel}{period:02d}.tif"
            if not source.is_file():
                raise FileNotFoundError(source)
            shutil.copy2(source, destination)
            digest = regression.sha256(destination)
            if digest != regression.sha256(source):
                raise IOError(f"Copy hash mismatch: {destination}")
            relative = destination.relative_to(target).as_posix()
            frozen[relative] = digest
            changes.append({"source": source.relative_to(target).as_posix(),
                            "destination": relative, "sha256": digest})
    regression.emit_json(frozen_path, frozen)
    regression.emit_json(target / "test_input_adaptations.json", {
        "scope": "Disposable fixture only; decennial IDW repeated annually.",
        "not_a_server_input_contract_change": True, "copies": changes,
    })


def stage(args: argparse.Namespace) -> None:
    regression.stage(args)
    if args.expand_decennial_idw:
        expand_decennial_idw(regression.checked_fixture(args.root, args.name), args.years)


def compare(args: argparse.Namespace) -> None:
    regression.compare(args)
    result = json.loads((args.root / f"comparison_{args.left}_vs_{args.right}.json").read_text())
    # The older harness permits added outputs; the web replacement contract does not.
    if result["additional"]:
        raise SystemExit("Candidate added scientific files; web output contract changed")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    commands = parser.add_subparsers(dest="operation", required=True)
    p = commands.add_parser("stage")
    p.add_argument("--source", type=Path, required=True)
    p.add_argument("--model", type=Path, required=True)
    p.add_argument("--name", required=True)
    p.add_argument("--years", type=int, default=3)
    p.add_argument("--mc", type=int, default=3)
    p.add_argument("--uncapped", type=int, choices=(0, 1), default=0)
    p.add_argument("--expand-decennial-idw", action="store_true",
                   help="Adapt a newer LOCAL test dataset only; not needed for real web inputs")
    p.set_defaults(function=stage)
    p = commands.add_parser("run")
    p.add_argument("--name", required=True)
    p.add_argument("--engine", type=Path, default=regression.DEFAULT_ENGINE)
    p.add_argument("--processors", type=int, default=2)
    p.add_argument("--timeout", type=int, default=600)
    p.add_argument("--verify-only", action="store_true")
    p.add_argument("--disable-native-expressions", action="store_true")
    p.set_defaults(function=regression.run)
    p = commands.add_parser("compare")
    p.add_argument("--left", required=True)
    p.add_argument("--right", required=True)
    p.set_defaults(function=compare)
    args = parser.parse_args()
    args.function(args)


if __name__ == "__main__":
    main()
