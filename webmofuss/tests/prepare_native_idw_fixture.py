"""Generate disposable annual IDW fixtures with the supplied native MoFuSS graph.

This is NOT certification of equivalence to the server's C++ IDW preparation.
The 2026-10-08 one-origin probe produced NoData at the origin; this native
graph therefore failed the scientific gate for the new small-area case.
Retained for reproducing that finding, not for supplying its simulation inputs.
Only the two demand-table filenames and two output filenames are adapted. The
supplied graph's equations, cost settings, category order, precision, and origin
behavior are retained. Actual annual demand tables must already exist.

Nothing executes during stage. Run a one-period pilot before extending the range:
  python prepare_native_idw_fixture.py --case E:/MoFuSS_Active/TASK/prepared/webmofuss stage --start 1 --end 1
  python prepare_native_idw_fixture.py --case E:/MoFuSS_Active/TASK/prepared/webmofuss run --start 1 --end 1
  # Review outputs; then stage/run --start 2 --end 51 explicitly.

Completed periods can be revisited only when their inputs, graph, and outputs
still match their recorded hashes. Existing unverified outputs are never replaced.
"""
from __future__ import annotations

import argparse
import csv
from datetime import datetime, timezone
import hashlib
import json
import math
import os
from pathlib import Path
import subprocess
import time
import xml.etree.ElementTree as ET

ACTIVE = Path("E:/MoFuSS_Active")
DEFAULT_GRAPH = Path(__file__).resolve().parents[2] / "localhost/scripts/older_versions/IDW_Sc3.egoml"
ENGINE = Path("C:/Program Files/Dinamica EGO/DinamicaConsole.exe")
SCHEMA = "webmofuss_native_annual_idw_fixture_v1"
STATIC_INPUTS = ("In/fricc_w.tif", "In/fricc_v.tif",
                 "LULCC/TempRaster/locs_c_w.tif", "LULCC/TempRaster/locs_c_v.tif")


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def sha(path: Path) -> str:
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(block)
    return value.hexdigest()


def emit(path: Path, value: object) -> None:
    path.write_text(json.dumps(value, indent=2) + "\n", encoding="utf-8")


def read(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def now() -> str:
    return datetime.now(timezone.utc).isoformat()


def case_root(path: Path) -> Path:
    case, active = path.resolve(strict=True), ACTIVE.resolve()
    require(case.is_dir() and active in case.parents,
            "Case must be an existing prepared directory strictly below E:/MoFuSS_Active")
    require(len(case.relative_to(active).parts) >= 2,
            "Use a prepared case below a named task folder, not the task root")
    for current in (case, *case.parents):
        if current == active.parent:
            break
        require(not current.is_symlink() and not current.is_junction(),
                f"Case path contains a link: {current}")
    return case


def child(case: Path, relative: str) -> Path:
    raw = Path(relative)
    require(not raw.is_absolute() and ".." not in raw.parts, "Unsafe fixture path")
    path = case / raw
    require(case in path.resolve().parents, f"Path escapes fixture: {relative}")
    for current in (path, *path.parents):
        if current == case:
            break
        require(not current.is_symlink() and not current.is_junction(),
                f"Fixture path contains a link: {current}")
    return path


def periods(args: argparse.Namespace) -> range:
    require(1 <= args.start <= args.end <= 51, "Period range must be within 1..51")
    return range(args.start, args.end + 1)


def annual_paths(period: int) -> tuple[tuple[str, str], tuple[str, str]]:
    tables = tuple(f"In/DemandScenarios/fwuse_{channel}{period:02d}.csv" for channel in ("W", "V"))
    outputs = tuple(f"In/IDW_C++_fw_{channel}{period:02d}.tif" for channel in ("w", "v"))
    return tables, outputs


def inspect_table(path: Path) -> dict:
    with path.open(encoding="utf-8-sig", newline="") as stream:
        rows = list(csv.reader(stream))
    require(len(rows) >= 2 and len(rows[0]) == 2, f"Expected a two-column demand lookup table: {path}")
    keys = set()
    for row in rows[1:]:
        require(len(row) == 2, f"Invalid lookup row in {path}")
        key, demand = float(row[0]), float(row[1])
        require(math.isfinite(key) and key.is_integer() and math.isfinite(demand) and demand >= 0,
                f"Demand table has a noninteger key or invalid demand: {path}")
        require(key not in keys, f"Demand table contains a duplicate category: {path}")
        keys.add(key)
    return {"rows": len(keys), "columns": rows[0], "sha256": sha(path),
            "note": "Values retained exactly; key-to-raster coverage must be checked in preparation"}


def graph_for_period(original: bytes, period: int) -> tuple[bytes, list[dict]]:
    tree = ET.fromstring(original)
    expected_paths = {*STATIC_INPUTS, "In/DemandScenarios/fwuse_W01.csv",
                      "In/DemandScenarios/fwuse_V01.csv", "In/Indice_w.tif", "In/Indice_v.tif"}
    filenames = [node.text.strip().strip('"') for node in tree.iter("inputport")
                 if node.get("name") == "filename" and node.text]
    require(len(filenames) == len(expected_paths) and set(filenames) == expected_paths,
            "Native graph input/output paths do not match the inspected contract")
    saves = list(tree.iter("functor"))
    saves = [node for node in saves if node.get("name") == "SaveMap"]
    require(len(saves) == 2, "Expected exactly two native IDW SaveMap nodes")
    require({node.find("inputport[@name='map']").get("peerid") for node in saves} == {"v12", "v18"},
            "Unexpected native IDW result ports")
    require(not any(node.get("name") == "RunExternalProcess" for node in tree.iter()),
            "Native IDW graph must not invoke external processes")
    tables, outputs = annual_paths(period)
    replacements = (("In/DemandScenarios/fwuse_W01.csv", tables[0]),
                    ("In/DemandScenarios/fwuse_V01.csv", tables[1]),
                    ("In/Indice_w.tif", outputs[0]), ("In/Indice_v.tif", outputs[1]))
    adapted, changes = original, []
    for old, new in replacements:
        # Replace exact XML text bytes, retaining every other byte of the source.
        old_bytes = f"&quot;{old}&quot;".encode("ascii")
        new_bytes = f"&quot;{new}&quot;".encode("ascii")
        require(adapted.count(old_bytes) == 1, f"Expected exactly one native path literal: {old}")
        adapted = adapted.replace(old_bytes, new_bytes)
        changes.append({"before": old, "after": new, "changed": old != new})
    ET.fromstring(adapted)
    return adapted, changes


def verify_inputs(case: Path, manifest: dict) -> None:
    for relative, expected in manifest["input_sha256"].items():
        path = child(case, relative)
        require(path.is_file() and sha(path) == expected, f"Staged input missing or changed: {relative}")


def verify_completed(case: Path, directory: Path, manifest: dict) -> dict:
    result = read(directory / "result.json")
    require(result.get("success") is True, f"Period is not a verified completed run: {directory.name}")
    verify_inputs(case, manifest)
    require(sha(directory / "model.egoml") == manifest["adapted_graph_sha256"], "Staged graph changed")
    require(set(result["output_sha256"]) == set(manifest["outputs"]), "Recorded output contract differs")
    for relative, expected in result["output_sha256"].items():
        path = child(case, relative)
        require(path.is_file() and sha(path) == expected, f"Verified IDW output changed: {relative}")
    return result


def stage(args: argparse.Namespace) -> None:
    case, selection = case_root(args.case), periods(args)
    graph = args.source_graph.resolve(strict=True)
    original, graph_hash = graph.read_bytes(), sha(graph)
    audit = child(case, "_native_idw")
    existing = audit / "fixture_manifest.json"
    if audit.exists():
        require(existing.is_file(), "Existing native-IDW directory lacks its provenance manifest")
        require(read(existing)["source_graph_sha256"] == graph_hash, "Native source graph changed")
    # Validate the complete requested range before writing any staged period.
    planned, skipped = [], []
    for period in selection:
        tables, outputs = annual_paths(period)
        directory = child(case, f"_native_idw/period_{period:02d}")
        if directory.exists():
            manifest = read(directory / "manifest.json")
            require(manifest["source_graph_sha256"] == graph_hash, "Period was staged from another graph")
            verify_completed(case, directory, manifest)
            skipped.append(period)
            continue
        for relative in outputs:
            require(not child(case, relative).exists(), f"Refusing to replace existing unverified output: {relative}")
        metadata = {relative: inspect_table(child(case, relative)) for relative in tables}
        hashes = {relative: sha(child(case, relative)) for relative in (*STATIC_INPUTS, *tables)}
        adapted, changes = graph_for_period(original, period)
        planned.append((directory, adapted, {
            "schema": SCHEMA, "period": period, "created_utc": now(),
            "source_graph": str(graph), "source_graph_sha256": graph_hash,
            "adapted_graph_sha256": hashlib.sha256(adapted).hexdigest(),
            "path_edits": changes, "input_sha256": hashes, "demand_tables": metadata,
            "outputs": list(outputs), "server_cpp_equivalence": "not established",
            "scope": "Native MoFuSS test-fixture pressures from real annual demand; path changes only",
        }))
    if not existing.exists():
        audit.mkdir()
        (audit / "source_graph.egoml").write_bytes(original)
        emit(existing, {"schema": SCHEMA, "created_utc": now(), "case": str(case),
                        "source_graph": str(graph), "source_graph_sha256": graph_hash,
                        "maximum_period": 51, "server_cpp_equivalence": "not established",
                        "preserved": "All equations, native cost settings, precision, and category accumulation order"})
    for directory, adapted, manifest in planned:
        directory.mkdir()
        (directory / "model.egoml").write_bytes(adapted)
        emit(directory / "manifest.json", manifest)
    print(json.dumps({"case": str(case), "staged": [x[2]["period"] for x in planned],
                      "skipped_verified_completed": skipped, "executed": False}, indent=2))


def run(args: argparse.Namespace) -> None:
    case, selection = case_root(args.case), periods(args)
    require(1 <= args.processors <= 2, "This fixture runner is bounded to one or two workers")
    audit = child(case, "_native_idw")
    fixture = read(audit / "fixture_manifest.json")
    require(fixture["schema"] == SCHEMA, "Not a staged native-IDW fixture")
    require(sha(audit / "source_graph.egoml") == fixture["source_graph_sha256"], "Stored source graph changed")
    engine = args.engine.resolve(strict=True)
    engine_hash = sha(engine)
    pending, skipped = [], []
    for period in selection:
        directory = child(case, f"_native_idw/period_{period:02d}")
        manifest = read(directory / "manifest.json")
        require(manifest["period"] == period and manifest["source_graph_sha256"] == fixture["source_graph_sha256"],
                "Period provenance does not match the fixture")
        verify_inputs(case, manifest)
        require(sha(directory / "model.egoml") == manifest["adapted_graph_sha256"], "Adapted graph changed")
        if (directory / "result.json").exists():
            result = verify_completed(case, directory, manifest)
            require(result["engine_sha256"] == engine_hash, "Completed period used another engine build")
            skipped.append(period)
            continue
        require(not (directory / "started.json").exists(),
                f"Period {period} was already started; investigate without overwriting its files")
        for relative in manifest["outputs"]:
            require(not child(case, relative).exists(), f"Unverified output exists: {relative}")
        pending.append((directory, manifest))
    completed = []
    for directory, manifest in pending:
        verify_inputs(case, manifest)
        private_temp = directory / "engine_temp"
        private_temp.mkdir()
        environment = os.environ.copy()
        environment.update(TEMP=str(private_temp), TMP=str(private_temp), TMPDIR=str(private_temp),
                           OMP_NUM_THREADS=str(args.processors))
        command = [str(engine), "-processors", str(args.processors), "-log-level", "4",
                   str(directory / "model.egoml")]
        record = {"schema": SCHEMA, "period": manifest["period"], "started_utc": now(),
                  "command": command, "engine_sha256": engine_hash,
                  "processors": args.processors, "private_temp": str(private_temp),
                  "load_note": args.load_note, "server_cpp_equivalence": "not established"}
        emit(directory / "started.json", record)
        start = time.perf_counter()
        # The working directory is the isolated prepared case. All four adapted
        # paths and all unchanged native graph paths resolve inside that case.
        with (directory / "runtime.log").open("wb") as log:
            process = subprocess.Popen(command, cwd=case, env=environment,
                                       stdout=log, stderr=subprocess.STDOUT)
            record["pid"] = process.pid
            emit(directory / "started.json", record)
            returncode = process.wait()
        record.update(returncode=returncode, elapsed_seconds=time.perf_counter() - start,
                      ended_utc=now())
        text = (directory / "runtime.log").read_text(encoding="utf-8", errors="replace")
        record["engine_success_message"] = "Model script ran successfully" in text
        record["output_sha256"] = {
            relative: sha(child(case, relative)) for relative in manifest["outputs"]
            if child(case, relative).is_file() and child(case, relative).stat().st_size > 0}
        record["success"] = (returncode == 0 and record["engine_success_message"]
                             and len(record["output_sha256"]) == 2)
        record["validation_limit"] = "Engine completion and file hashes only; inspect raster values/geometry, including source-cost zero behavior"
        emit(directory / "result.json", record)
        print(json.dumps(record, indent=2), flush=True)
        require(record["success"], f"Native IDW period {manifest['period']} failed; later periods were not launched")
        verify_inputs(case, manifest)
        completed.append(manifest["period"])
    print(json.dumps({"case": str(case), "completed": completed,
                      "skipped_verified_completed": skipped}, indent=2))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--case", type=Path, required=True)
    commands = parser.add_subparsers(dest="operation", required=True)
    for operation, function in (("stage", stage), ("run", run)):
        item = commands.add_parser(operation)
        item.add_argument("--start", type=int, default=1)
        item.add_argument("--end", type=int, default=1)
        if operation == "stage":
            item.add_argument("--source-graph", type=Path, default=DEFAULT_GRAPH)
        else:
            item.add_argument("--engine", type=Path, default=ENGINE)
            item.add_argument("--processors", type=int, default=2)
            item.add_argument("--load-note", default="Four pre-existing Dinamica jobs observed; recheck before launch")
        item.set_defaults(function=function)
    args = parser.parse_args()
    args.function(args)


if __name__ == "__main__":
    main()
