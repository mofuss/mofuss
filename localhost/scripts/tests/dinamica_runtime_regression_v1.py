"""Isolated, frozen-input Dinamica runtime regression harness.

This tests model dynamics, not R initialization/report production. Staged models
remove the four legacy external R calls identically; all scientific graph nodes
are preserved. MC inputs are copied from a completed run, never regenerated.
No function deletes a fixture or writes into the source run.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import re
import shutil
import subprocess
import time
import xml.etree.ElementTree as ET
from pathlib import Path

DEFAULT_ROOT = Path("E:/MoFuSS_Active/ecsa_sourcing_and_speed_v1/runtime_regression")
DEFAULT_ENGINE = Path("C:/Program Files/Dinamica EGO/DinamicaConsole.exe")
LEGACY_EXTERNAL = {
    "runExternalProcess1322", "runExternalProcess2510",
    "runExternalProcess3537", "runExternalProcess1801",
}
FROZEN_FILES = (
    "i_st_all.csv", "k_all.csv", "rmax_all.csv",
    "Harvest_pixels_W.csv", "Harvest_pixels_V.csv",
    "Prune_factor_W.csv", "Prune_factor_V.csv",
)
SCIENCE_DIRS = {"temp", "debugging", "sourcing"}


def sha256(path: Path) -> str:
    result = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            result.update(block)
    return result.hexdigest()


def emit_json(path: Path, value: object) -> None:
    path.write_text(json.dumps(value, indent=2) + "\n", encoding="utf-8")


def checked_fixture(root: Path, name: str, must_exist: bool = True) -> Path:
    if not re.fullmatch(r"[A-Za-z0-9_-]+", name):
        raise ValueError("Fixture name must contain letters, digits, '_' or '-'.")
    root = root.resolve()
    if root == root.parent or "MoFuSS_Active" not in root.parts:
        raise ValueError("Regression root must be a task folder under MoFuSS_Active.")
    target = (root / name).resolve()
    if target.parent != root:
        raise ValueError("Fixture escaped regression root.")
    if must_exist and not (target / "fixture_manifest.json").is_file():
        raise ValueError(f"Not a staged regression fixture: {target}")
    return target


def rewrite_parameters(path: Path, years: int, mc: int, uncapped: int) -> None:
    with path.open(encoding="utf-8-sig", newline="") as stream:
        reader = csv.DictReader(stream)
        fields = reader.fieldnames
        rows = list(reader)
    changes = {
        "start_year": "2000", "end_year": str(2000 + years - 1),
        "monte_carlo_runs": str(mc), "uncapped_regrowth": str(uncapped),
    }
    for row in rows:
        if row.get("Var") in changes:
            row["ParCHR"] = changes[row["Var"]]
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def stage(args: argparse.Namespace) -> None:
    source = args.source.resolve(strict=True)
    target = checked_fixture(args.root, args.name, must_exist=False)
    if target.exists():
        raise FileExistsError(f"Refusing to overwrite fixture: {target}")
    if source == target or source in target.parents or target in source.parents:
        raise ValueError("Source and fixture must be independent directories.")
    model = args.model.resolve(strict=True)
    for name in FROZEN_FILES:
        if not (source / "Temp" / name).is_file():
            raise FileNotFoundError(f"Completed source MC input missing: {name}")
    if not 1 <= args.years <= 51 or not 1 <= args.mc <= 3:
        raise ValueError("Bounded fixture supports 1..51 years and 1..3 MC runs.")
    target.mkdir(parents=True)
    copied = []

    def copy_one(path: Path) -> None:
        relative = path.relative_to(source)
        destination = target / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(path, destination)
        original_hash = sha256(path)
        if sha256(destination) != original_hash:
            raise IOError(f"Copy hash mismatch: {relative}")
        copied.append({"path": relative.as_posix(), "sha256": original_hash,
                       "bytes": path.stat().st_size})

    for relative in ("In", "LULCC/TempRaster", "LULCC/TempTables"):
        for path in sorted((source / relative).rglob("*")):
            if not path.is_file() or "HC_jobs" in path.parts:
                continue
            copy_one(path)
    source_global = source / "LULCC/DownloadedDatasets/SourceDataGlobal"
    copy_one(source_global / "parameters.csv")
    vector_root = source_global / "InVector"
    if vector_root.is_dir():
        for path in sorted(vector_root.rglob("*")):
            if path.is_file():
                copy_one(path)
    temp_extra = ("LULC_Categories1.csv", "MaxAGB.csv", "MaxAGB_firstMC.csv",
                  "MaxAGB_lastMC.csv", "mc_batch_ready.csv")
    for name in (*FROZEN_FILES, *temp_extra):
        path = source / "Temp" / name
        if path.is_file():
            copy_one(path)
    for index in range(1, args.mc + 1):
        (target / f"debugging_{index}").mkdir()
    for name in ("Debugging", "Out", "Logs"):
        (target / name).mkdir(exist_ok=True)
    # Candidate sourcing observers use independent outputs and have no effect
    # on the baseline dynamics. Precreate their output directories if present.
    if "Sourcing/" in model.read_text(encoding="utf-8"):
        (target / "Sourcing" / "static").mkdir(parents=True)
        for index in range(1, args.mc + 1):
            (target / "Sourcing" / f"MC{index:03d}").mkdir()
    for relative in ("LULCC/TempTables/parameters_dinamica.csv",
                     "LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"):
        rewrite_parameters(target / relative, args.years, args.mc, args.uncapped)

    tree = ET.parse(model)
    removed = []
    for parent in tree.iter():
        for node in list(parent):
            if node.tag != "functor" or node.get("name") != "RunExternalProcess":
                continue
            alias = node.find("property[@key='dff.functor.alias']")
            if alias is not None and alias.get("value") in LEGACY_EXTERNAL:
                if node.find("outputport") is not None:
                    raise ValueError("Legacy external node unexpectedly has outputs.")
                removed.append(alias.get("value"))
                parent.remove(node)
    if set(removed) != LEGACY_EXTERNAL:
        raise ValueError(f"Unexpected external-process structure: {removed}")
    # The cloned frozen MC inputs retain the completed source's 3 rows. Reducing
    # the configured MC count selects a prefix; no random draw is regenerated.
    staged_model = target / "regression_model.egoml"
    tree.write(staged_model, encoding="utf-8", xml_declaration=True)
    formulas = []
    for node in tree.iter("containerfunctor"):
        if node.get("name") not in {"CalculateMap", "CalculateValue"}:
            continue
        alias = node.find("property[@key='dff.functor.alias']")
        formulas.append({
            "type": node.get("name"),
            "alias": alias.get("value") if alias is not None else None,
            "outputs": [dict(p.attrib) for p in node.findall("outputport")],
            "ports": [{**p.attrib, "text": p.text} for p in node.findall("inputport")],
        })
    emit_json(target / "engine_formula_inventory.json", formulas)
    emit_json(target / "copied_source_inputs.json", copied)
    # Record hashes AFTER the two deliberate test-duration/mode edits.
    frozen = {item["path"]: sha256(target / item["path"]) for item in copied}
    emit_json(target / "frozen_input_hashes.json", frozen)
    manifest = {
        "source": str(source), "model_source": str(model),
        "model_source_sha256": sha256(model),
        "staged_model_sha256": sha256(staged_model),
        "years": args.years, "mc": args.mc, "uncapped": args.uncapped,
        "removed_external_calls": removed,
        "scope": "scientific dynamics with frozen MC draws; external R initialization/reporting excluded",
        "copied_input_count": len(copied),
        "copied_input_bytes": sum(item["bytes"] for item in copied),
    }
    emit_json(target / "fixture_manifest.json", manifest)
    print(json.dumps({"staged": str(target), **manifest}, indent=2))


def science_hashes(target: Path) -> dict[str, str]:
    frozen = json.loads((target / "frozen_input_hashes.json").read_text())
    result = {}
    for directory in target.iterdir():
        if not directory.is_dir() or not (
            directory.name.lower() in SCIENCE_DIRS
            or re.fullmatch(r"debugging_[0-9]+", directory.name.lower())
        ):
            continue
        for path in sorted(directory.rglob("*")):
            if path.is_file() and path.suffix.lower() in {".tif", ".tiff", ".csv"}:
                relative = path.relative_to(target).as_posix()
                current = sha256(path)
                if relative not in frozen:
                    result[relative] = current
                elif current != frozen[relative]:
                    raise ValueError(f"Frozen MC/input file was modified: {relative}")
    return result


def run(args: argparse.Namespace) -> None:
    target = checked_fixture(args.root, args.name)
    if (target / "runtime_result.json").exists():
        raise FileExistsError("Fixture already ran; stage a fresh named fixture.")
    frozen = json.loads((target / "frozen_input_hashes.json").read_text())
    for relative, expected in frozen.items():
        if sha256(target / relative) != expected:
            raise ValueError(f"Fixture input changed after staging: {relative}")
    command = [str(args.engine), "-processors", str(args.processors),
               "-predefined-seed", "-log-level", "4"]
    if args.verify_only:
        command.append("-dont-run")
    command.append(str(target / "regression_model.egoml"))
    log_path = target / ("verification.log" if args.verify_only else "runtime.log")
    # Dinamica 2.4 names native-expression DLLs with short temporary names.
    # Independent processes sharing TEMP can collide. Isolate only this child's
    # environment; leave the desktop/user environment and production runs alone.
    native_temp = target / "engine_temp"
    native_temp.mkdir(exist_ok=True)
    environment = os.environ.copy()
    environment["TEMP"] = str(native_temp)
    environment["TMP"] = str(native_temp)
    started = time.perf_counter()
    with log_path.open("wb") as log:
        completed = subprocess.run(command, cwd=target, stdout=log,
                                   stderr=subprocess.STDOUT, timeout=args.timeout,
                                   env=environment)
    result = {"command": command, "returncode": completed.returncode,
              "elapsed_seconds": time.perf_counter() - started,
              "log": str(log_path), "verify_only": args.verify_only,
              "engine_temp": str(native_temp)}
    if not args.verify_only:
        hashes = science_hashes(target)
        result["scientific_outputs"] = len(hashes)
        result["scientific_tifs"] = sum(p.lower().endswith(".tif") for p in hashes)
        emit_json(target / "scientific_output_sha256.json", hashes)
        emit_json(target / "runtime_result.json", result)
    print(json.dumps(result, indent=2))
    if completed.returncode:
        print(log_path.read_text(errors="replace")[-12000:])
        raise SystemExit(completed.returncode)


def compare(args: argparse.Namespace) -> None:
    left = checked_fixture(args.root, args.left)
    right = checked_fixture(args.root, args.right)
    inputs_left = json.loads((left / "frozen_input_hashes.json").read_text())
    inputs_right = json.loads((right / "frozen_input_hashes.json").read_text())
    if inputs_left != inputs_right:
        different_inputs = sorted(
            p for p in inputs_left.keys() | inputs_right.keys()
            if inputs_left.get(p) != inputs_right.get(p)
        )
        raise ValueError(f"Cannot compare: staged scientific inputs differ: {different_inputs}")
    for target in (left, right):
        result = json.loads((target / "runtime_result.json").read_text())
        if result["returncode"] != 0:
            raise ValueError(f"Cannot compare failed model run: {target}")
    a = json.loads((left / "scientific_output_sha256.json").read_text())
    b = json.loads((right / "scientific_output_sha256.json").read_text())
    common = sorted(a.keys() & b.keys())
    different = [p for p in common if a[p] != b[p]]
    missing = sorted(a.keys() - b.keys())
    added = sorted(b.keys() - a.keys())
    result = {"left": args.left, "right": args.right, "compared": len(common),
              "identical": len(common) - len(different), "different": different,
              "missing": missing, "additional": added,
              "all_baseline_scientific_outputs_sha256_identical":
                  bool(a) and not different and not missing}
    output = args.root / f"comparison_{args.left}_vs_{args.right}.json"
    emit_json(output, result)
    print(json.dumps(result, indent=2))
    if not result["all_baseline_scientific_outputs_sha256_identical"]:
        raise SystemExit(1)


def snapshot(args: argparse.Namespace) -> None:
    """Refresh hashes after separate, authorized sourcing postprocessing."""
    target = checked_fixture(args.root, args.name)
    result_path = target / "runtime_result.json"
    result = json.loads(result_path.read_text())
    if result["returncode"] != 0:
        raise ValueError("Cannot snapshot a failed run.")
    hashes = science_hashes(target)
    emit_json(target / "scientific_output_sha256.json", hashes)
    result["scientific_outputs"] = len(hashes)
    result["scientific_tifs"] = sum(p.lower().endswith(".tif") for p in hashes)
    emit_json(result_path, result)
    print(json.dumps({"fixture": args.name, "scientific_outputs": len(hashes)}))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=DEFAULT_ROOT)
    sub = parser.add_subparsers(dest="operation", required=True)
    p = sub.add_parser("stage")
    p.add_argument("--source", type=Path, required=True)
    p.add_argument("--model", type=Path, required=True)
    p.add_argument("--name", required=True)
    p.add_argument("--years", type=int, default=3)
    p.add_argument("--mc", type=int, default=3)
    p.add_argument("--uncapped", type=int, choices=(0, 1), default=0)
    p.set_defaults(function=stage)
    p = sub.add_parser("run")
    p.add_argument("--name", required=True)
    p.add_argument("--engine", type=Path, default=DEFAULT_ENGINE)
    p.add_argument("--processors", type=int, default=1)
    p.add_argument("--timeout", type=int, default=600)
    p.add_argument("--verify-only", action="store_true")
    p.set_defaults(function=run)
    p = sub.add_parser("compare")
    p.add_argument("--left", required=True)
    p.add_argument("--right", required=True)
    p.set_defaults(function=compare)
    p = sub.add_parser("snapshot")
    p.add_argument("--name", required=True)
    p.set_defaults(function=snapshot)
    args = parser.parse_args()
    args.function(args)


if __name__ == "__main__":
    main()
