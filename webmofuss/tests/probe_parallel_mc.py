"""Experimental three-process MC proof using an already staged frozen fixture.

This is NOT a web launcher or deployment candidate. It retains all frozen input
bytes and the configured MC count; each private worker executes one global MC.
The existing regression helper omits external R preparation/reporting. Its fixed
predefined seed is used ONLY to compare deterministic frozen-input dynamics and
is NOT an acceptable production scheme for independent stochastic workers.

Four explicit phases: stage, run, gather, compare. Nothing launches during stage.
All writes must stay below a task directory in E:/MoFuSS_Active. No function
deletes files, edits the serial fixture, or writes to the canonical source run.
"""
from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import csv
from datetime import datetime, timezone
from decimal import Decimal, InvalidOperation
import hashlib
import io
import json
from pathlib import Path, PurePosixPath
import re
import shutil
import sys
import time
import xml.etree.ElementTree as ET

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
from run_webmofuss_regression import regression  # noqa: E402

ACTIVE = Path("E:/MoFuSS_Active")
GLOBAL_MC_PORT = "v93000"
MC_IDS = (1, 2, 3)
SUMMARY_TYPES = {
    "Temp/3_NRB.csv": "SaveLookupTable",
    "Temp/3_CON_TOT.csv": "SaveLookupTable",
    "Temp/3_CON_NRB.csv": "SaveLookupTable",
    "Temp/3_EXP_CON_TOT.csv": "SaveLookupTable",
    "Temp/3_FW_DEF.csv": "SaveLookupTable",
    "Temp/x_Cons_W_all.csv": "SaveTable",
    "Temp/x_Cons_W.csv": "SaveTable",
    "Temp/x_Cons_V.csv": "SaveLookupTable",
    "Temp/x_Cons_V_all.csv": "SaveLookupTable",
}
SCHEMA = "webmofuss_frozen_three_process_mc_probe_v1"


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def read_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def sha(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def task_root(path: Path) -> Path:
    root, active = path.resolve(), ACTIVE.resolve()
    require(root != active and active in root.parents,
            "Probe root must be a task directory strictly below E:/MoFuSS_Active")
    return root


def probe_path(args: argparse.Namespace, *, existing: bool = True) -> Path:
    require(bool(re.fullmatch(r"[A-Za-z0-9_-]+", args.name)), "Invalid probe name")
    root = task_root(args.root)
    target = (root / args.name).resolve()
    require(target.parent == root, "Probe path escaped its task root")
    if existing:
        require((target / "probe_manifest.json").is_file(), "Probe has not been staged")
    return target


def inside(directory: Path, relative: str) -> Path:
    part = PurePosixPath(relative)
    require(not part.is_absolute() and ".." not in part.parts and "\\" not in relative
            and ":" not in relative, f"Unsafe relative path: {relative}")
    result = (directory / relative).resolve()
    require(directory.resolve() in result.parents, f"Path escaped directory: {relative}")
    return result


def verify_frozen(directory: Path, frozen: dict[str, str]) -> None:
    require(bool(frozen), "Empty frozen input manifest")
    for relative, digest in frozen.items():
        source = inside(directory, relative)
        require(source.is_file() and regression.sha256(source) == digest,
                f"Frozen input missing or changed: {source}")


def copy_verified(source: Path, destination: Path, expected: str) -> None:
    require(not destination.exists(), f"Refusing to overwrite: {destination}")
    require(regression.sha256(source) == expected, f"Source changed before copy: {source}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source, destination)
    require(regression.sha256(destination) == expected, f"Copied bytes differ: {destination}")


def node_alias(node: ET.Element) -> str:
    item = node.find("property[@key='dff.functor.alias']")
    return "" if item is None else item.get("value", "")


def worker_model(staged: bytes, global_mc: int) -> tuple[bytes, dict]:
    """Change scheduling/identity only, retaining all existing scientific nodes."""
    require(global_mc in MC_IDS, "This bounded proof supports only global MC IDs 1..3")
    root = ET.fromstring(staged)
    producers = {p.get("id"): n for n in root.iter() for p in n
                 if p.tag in ("outputport", "internaloutputport")}
    require(GLOBAL_MC_PORT not in producers, "Probe ID collides with existing graph")
    mc = producers["v8"]
    require(mc.get("name") == "Repeat" and node_alias(mc) == "repeat775", "Unexpected MC loop")
    require(producers["v39"] in list(mc) and producers["v39"].get("name") == "Repeat",
            "Unexpected temporal loop nesting")
    require(not any(n.get("name") == "RunExternalProcess" for n in root.iter()),
            "Use the staged regression model with all external R calls removed")
    # This experiment deliberately excludes active stochastic patchers. Frozen
    # R draws may differ by MC, but starting each worker at one predefined native
    # RNG stream must never be presented as independent stochastic sampling.
    for port, label, expected in (("v253", "deforestation", ".no"),
                                  ("v313", "harvest patcher bypass", ".yes")):
        require(producers[port].findtext("inputport[@name='constant']") == expected,
                f"Deterministic proof requires the existing {label} constant {expected}")
    iterations = mc.find("inputport[@name='iterations']")
    require(iterations is not None and iterations.get("peerid") == "v282",
            "MC loop no longer uses original total-count port")
    before_annual = ET.tostring(producers["v39"])
    references = [p for p in root.iter("inputport") if p.get("peerid") == "v8"]
    require(bool(references) and all(p in set(mc.iter()) for p in references),
            "Unexpected global MC step consumer outside the realization body")
    iterations.attrib.pop("peerid")
    iterations.text = "1"
    for port in references:
        port.set("peerid", GLOBAL_MC_PORT)
    identity = ET.SubElement(mc, "functor", name="Int")
    ET.SubElement(identity, "property", key="dff.functor.alias", value="Probe global MC identity")
    ET.SubElement(identity, "inputport", name="constant").text = str(global_mc)
    ET.SubElement(identity, "outputport", name="object", id=GLOBAL_MC_PORT)
    # Undo only the identity substitution to prove the annual scientific body
    # has not been otherwise edited, including all muxes and save nodes.
    annual_copy = ET.fromstring(ET.tostring(producers["v39"]))
    for port in annual_copy.iter("inputport"):
        if port.get("peerid") == GLOBAL_MC_PORT:
            port.set("peerid", "v8")
    require(ET.tostring(annual_copy).rstrip() == before_annual.rstrip(),
            "Temporal dynamics changed beyond MC identity")
    require(not any(p.get("peerid") == "v8" for p in root.iter("inputport")),
            "An original local-step consumer was missed")
    ids = [p.get("id") for p in root.iter() if p.get("id")]
    require(len(ids) == len(set(ids)), "Duplicate output IDs after worker rewrite")
    require(all(p.get("peerid") in ids for p in root.iter() if p.get("peerid")),
            "Unresolved worker graph input")
    output = ET.tostring(root, encoding="utf-8", xml_declaration=True)
    return output, {"global_mc_id": global_mc, "outer_repeat_iterations": 1,
                    "rerouted_v8_references": len(references), "new_identity_port": GLOBAL_MC_PORT,
                    "full_input_tables_and_configured_MC_count_unchanged": True,
                    "staged_model_sha256": sha(output)}


def baseline_for(args: argparse.Namespace) -> Path:
    baseline = args.baseline.resolve(strict=True)
    task_root(baseline.parent)
    require((baseline / "fixture_manifest.json").is_file(), "Baseline must be a staged fixture")
    return baseline


def load_probe(args: argparse.Namespace) -> tuple[Path, dict]:
    target = probe_path(args)
    manifest = read_json(target / "probe_manifest.json")
    require(manifest.get("schema") == SCHEMA, "Unrecognized probe manifest")
    require(Path(manifest["baseline"]).resolve() == baseline_for(args), "Baseline does not match staged probe")
    return target, manifest


def stage(args: argparse.Namespace) -> None:
    baseline = baseline_for(args)
    target = probe_path(args, existing=False)
    require(not target.exists(), "Probe already exists; choose a fresh name")
    require(target not in baseline.parents and baseline not in target.parents and target != baseline,
            "Probe and baseline must be independent directories")
    original = read_json(baseline / "fixture_manifest.json")
    require(original["mc"] == 3, "Baseline must have exactly three configured MCs")
    source, model = args.source.resolve(strict=True), args.model.resolve(strict=True)
    require(source == Path(original["source"]).resolve(), "Explicit source differs from baseline provenance")
    require(regression.sha256(model) == original["model_source_sha256"],
            "Explicit model does not match baseline source-model hash")
    frozen = read_json(baseline / "frozen_input_hashes.json")
    verify_frozen(baseline, frozen)
    staged = (baseline / "regression_model.egoml").read_bytes()
    require(sha(staged) == original["staged_model_sha256"], "Baseline staged model changed")
    # Validate every graph transformation before creating any fixture directories.
    models = {index: worker_model(staged, index) for index in MC_IDS}
    target.mkdir(parents=True)
    workers = []
    for index in MC_IDS:
        worker = target / "workers" / f"mc{index:03d}"
        worker.mkdir(parents=True)
        for relative, digest in frozen.items():
            copy_verified(inside(baseline, relative), inside(worker, relative), digest)
        for name in ("Debugging", "Temp", "Out", "Logs", *(f"debugging_{i}" for i in MC_IDS)):
            (worker / name).mkdir(exist_ok=True)
        data, rewrite = models[index]
        (worker / "regression_model.egoml").write_bytes(data)
        worker_manifest = {**original, **rewrite, "baseline_fixture": str(baseline),
                           "scope": "Experimental one-global-MC worker; fixed frozen inputs, no R calls"}
        regression.emit_json(worker / "fixture_manifest.json", worker_manifest)
        regression.emit_json(worker / "frozen_input_hashes.json", frozen)
        workers.append({"name": worker.name, **rewrite})
    manifest = {"schema": SCHEMA, "baseline": str(baseline), "source": str(source),
                "model": str(model), "model_source_sha256": original["model_source_sha256"],
                "baseline_staged_model_sha256": original["staged_model_sha256"],
                "baseline_parameters": {k: original[k] for k in ("years", "mc", "uncapped")},
                "frozen_input_manifest_sha256": regression.sha256(baseline / "frozen_input_hashes.json"),
                "workers": workers, "production_ready": False,
                "rng_limit": "Same predefined seed is a deterministic test control, NOT independent worker RNG",
                "scope": "Three private processes, frozen deterministic dynamics only; no web reports or launch changes"}
    regression.emit_json(target / "probe_manifest.json", manifest)
    print(json.dumps({"staged_probe": str(target), "workers": workers, "launched": False}, indent=2))


def run(args: argparse.Namespace) -> None:
    target, manifest = load_probe(args)
    engine = args.engine.resolve(strict=True)
    require(not (target / "parallel_launch.json").exists() and not (target / "parallel_runtime.json").exists(),
            "Probe already launched; use a fresh staged probe")
    for worker in manifest["workers"]:
        path = target / "workers" / worker["name"]
        require(not (path / "runtime_result.json").exists(), "A worker already ran; refuse partial rerun")
        require(regression.sha256(path / "regression_model.egoml") == worker["staged_model_sha256"],
                "Worker model changed after staging")
        verify_frozen(path, read_json(path / "frozen_input_hashes.json"))
    regression.emit_json(target / "parallel_launch.json", {
        "engine": str(engine), "processors_per_worker": args.processors_per_worker,
        "launched_utc": datetime.now(timezone.utc).isoformat(),
        "retry_policy": "A stopped or failed proof requires a freshly staged probe; no partial reruns",
    })
    started = time.perf_counter()
    results = []

    def execute(worker: dict) -> dict:
        begin = time.perf_counter()
        entry = {"global_mc_id": worker["global_mc_id"], "name": worker["name"],
                 "started_offset_seconds": begin - started,
                 "started_utc": datetime.now(timezone.utc).isoformat()}
        child = argparse.Namespace(root=target / "workers", name=worker["name"], engine=engine,
                                   processors=args.processors_per_worker, timeout=args.timeout,
                                   verify_only=False, disable_native_expressions=args.disable_native_expressions)
        try:
            regression.run(child)
            entry["success"] = True
        except (Exception, SystemExit) as error:
            entry.update(success=False, error=f"{type(error).__name__}: {error}")
        entry.update(ended_offset_seconds=time.perf_counter() - started,
                     ended_utc=datetime.now(timezone.utc).isoformat())
        return entry

    # Three independent regression subprocesses; each receives its own engine
    # TEMP/TMP directory from regression.run. No background shell helper is used.
    with ThreadPoolExecutor(max_workers=3) as pool:
        futures = [pool.submit(execute, worker) for worker in manifest["workers"]]
        for future in as_completed(futures):
            results.append(future.result())
    results.sort(key=lambda item: item["global_mc_id"])
    all_overlap = max(0.0, min(r["ended_offset_seconds"] for r in results)
                      - max(r["started_offset_seconds"] for r in results))
    report = {"engine": str(engine), "processors_per_worker": args.processors_per_worker,
              "requested_concurrent_processes": 3, "wall_seconds": time.perf_counter() - started,
              "worker_invocation_overlap_seconds": all_overlap, "workers": results,
              "all_succeeded": all(r["success"] for r in results),
              "rng_warning": manifest["rng_limit"]}
    regression.emit_json(target / "parallel_runtime.json", report)
    print(json.dumps(report, indent=2))
    require(report["all_succeeded"], "At least one worker failed; gather is forbidden")


def csv_parts(data: bytes, expected_mc: int, kind: str) -> tuple[bytes, bytes, bytes]:
    """Keep every key/value byte; inspect only native line/row punctuation."""
    lines = data.splitlines(keepends=True)
    require(len(lines) == 2, "A worker summary must contain exactly header and one data row")
    newline = b"\r\n" if lines[0].endswith(b"\r\n") else b"\n"
    require(all(line.endswith(newline) for line in lines), "Unexpected/mixed summary line endings")
    header, row = lines[0], lines[1][:-len(newline)]
    parsed = next(csv.reader(io.StringIO(row.decode("utf-8-sig")), skipinitialspace=True))
    try:
        identifier = Decimal(parsed[0])
    except (IndexError, InvalidOperation) as error:
        raise ValueError("Invalid worker MC summary key") from error
    require(identifier == expected_mc, f"Wrong summary key: expected {expected_mc}, got {identifier}")
    if kind == "SaveLookupTable":
        require(len(parsed) == 2 and not row.endswith(b","), "Unsupported single-row SaveLookupTable layout")
        require(header[:-len(newline)] == b"Key*, Value,", "Unexpected SaveLookupTable header")
    else:
        require(kind == "SaveTable" and len(parsed) == 3 and parsed[-1] == "" and row.endswith(b", "),
                "Unsupported single-row SaveTable layout")
        require(header[:-len(newline)] == b"Key*, Value, ", "Unexpected SaveTable header")
    return header, row, newline


def merge_summary(rows: list[tuple[int, bytes]], kind: str) -> bytes:
    require(sorted(index for index, _ in rows) == list(MC_IDS), "Duplicate or missing summary MC ID")
    parts = [(index, *csv_parts(data, index, kind)) for index, data in sorted(rows)]
    header, newline = parts[0][1], parts[0][3]
    require(all(item[1] == header and item[3] == newline for item in parts), "Worker summary headers differ")
    output = header
    for position, (_, _, row, _) in enumerate(parts):
        # SaveLookupTable uses a trailing comma as a row separator except on
        # its last row; SaveTable emits its trailing ', ' on every data row.
        separator = b"," if kind == "SaveLookupTable" and position < len(parts) - 1 else b""
        output += row + separator + newline
    return output


def gather(args: argparse.Namespace) -> None:
    target, manifest = load_probe(args)
    runtime = read_json(target / "parallel_runtime.json")
    require(runtime.get("all_succeeded"), "Cannot gather unsuccessful workers")
    destination = target / "gathered"
    require(not destination.exists(), "Gather already exists; refuse overwrite")
    frozen = read_json(Path(manifest["baseline"]) / "frozen_input_hashes.json")
    collected: dict[str, list[tuple[int, Path, str]]] = {}
    for worker in manifest["workers"]:
        directory = target / "workers" / worker["name"]
        require(read_json(directory / "runtime_result.json")["returncode"] == 0, "Worker did not succeed")
        require(read_json(directory / "frozen_input_hashes.json") == frozen, "Worker input identities differ")
        verify_frozen(directory, frozen)
        hashes = regression.science_hashes(directory)
        require(hashes == read_json(directory / "scientific_output_sha256.json"), "Worker outputs changed after run")
        require(set(SUMMARY_TYPES).issubset(hashes), "Worker lacks one or more summary tables")
        for relative, digest in hashes.items():
            collected.setdefault(relative, []).append((worker["global_mc_id"], inside(directory, relative), digest))
    # Build and validate an entire publication plan before creating gathered.
    plans = []
    for relative, sources in sorted(collected.items()):
        if relative in SUMMARY_TYPES:
            data = merge_summary([(index, path.read_bytes()) for index, path, _ in sources], SUMMARY_TYPES[relative])
            plans.append((relative, data, None, sha(data), [i for i, _, _ in sources]))
        elif PurePosixPath(relative).parts[0].lower() == "debugging":
            selected = [(i, p, h) for i, p, h in sources if i == max(MC_IDS)]
            require(len(selected) == 1, f"Shared diagnostic lacks final global MC: {relative}")
            index, path, digest = selected[0]
            plans.append((relative, None, path, digest, [index]))
        else:
            require(len({digest for _, _, digest in sources}) == 1,
                    f"Conflicting output filename across workers: {relative}")
            _, path, digest = sources[0]
            plans.append((relative, None, path, digest, [i for i, _, _ in sources]))
    destination.mkdir()
    provenance, output_hashes = {}, {}
    for relative, data, source, digest, ids in plans:
        output = inside(destination, relative)
        if data is None:
            copy_verified(source, output, digest)
        else:
            output.parent.mkdir(parents=True, exist_ok=True)
            output.write_bytes(data)
        require(regression.sha256(output) == digest, "Gathered output failed hash verification")
        output_hashes[relative] = digest
        provenance[relative] = {"global_mc_ids": ids, "sha256": digest,
                                "operation": "ordered_native_CSV_gather" if data is not None else "verified_copy"}
    regression.emit_json(destination / "scientific_output_sha256.json", output_hashes)
    regression.emit_json(destination / "gather_provenance.json", provenance)
    regression.emit_json(destination / "frozen_input_hashes.json", frozen)
    print(json.dumps({"gathered": str(destination), "scientific_files": len(output_hashes)}, indent=2))


def compare(args: argparse.Namespace) -> None:
    target, manifest = load_probe(args)
    baseline = Path(manifest["baseline"])
    baseline_run = read_json(baseline / "runtime_result.json")
    require(baseline_run["returncode"] == 0 and not baseline_run["verify_only"], "Serial baseline did not complete")
    frozen = read_json(baseline / "frozen_input_hashes.json")
    require(regression.sha256(baseline / "frozen_input_hashes.json") == manifest["frozen_input_manifest_sha256"],
            "Serial input manifest changed after worker staging")
    verify_frozen(baseline, frozen)
    destination = target / "gathered"
    require(read_json(destination / "frozen_input_hashes.json") == frozen, "Serial and worker input bytes differ")
    expected = regression.science_hashes(baseline)
    require(expected == read_json(baseline / "scientific_output_sha256.json"), "Serial outputs changed after its run")
    actual = read_json(destination / "scientific_output_sha256.json")
    for relative, digest in actual.items():
        require(regression.sha256(inside(destination, relative)) == digest, "Gathered output changed")
    missing, extra = sorted(expected.keys() - actual.keys()), sorted(actual.keys() - expected.keys())
    different = sorted(name for name in expected.keys() & actual.keys() if expected[name] != actual[name])
    runtime = read_json(target / "parallel_runtime.json")
    result = {"scope": manifest["scope"], "baseline": str(baseline), "probe": str(target),
              "baseline_scientific_files": len(expected), "gathered_scientific_files": len(actual),
              "missing": missing, "additional": extra, "different": different,
              "all_scientific_outputs_sha256_identical": bool(expected) and not (missing or extra or different),
              "serial_seconds": baseline_run["elapsed_seconds"], "parallel_worker_wall_seconds": runtime["wall_seconds"],
              "worker_phase_speed_ratio": baseline_run["elapsed_seconds"] / runtime["wall_seconds"],
              "timing_excludes_staging_gather_and_all_external_R": True,
              "serial_engine_command": baseline_run["command"], "parallel_engine": runtime["engine"],
              "processors_per_worker": runtime["processors_per_worker"], "production_ready": False,
              "rng_warning": manifest["rng_limit"]}
    regression.emit_json(target / "comparison.json", result)
    print(json.dumps(result, indent=2))
    require(result["all_scientific_outputs_sha256_identical"], "Parallel proof differs from serial baseline")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True, help="Task directory strictly below E:/MoFuSS_Active")
    parser.add_argument("--name", required=True, help="New disposable probe name")
    parser.add_argument("--baseline", type=Path, required=True, help="Existing serial regression fixture directory")
    commands = parser.add_subparsers(dest="phase", required=True)
    p = commands.add_parser("stage", help="Copy only frozen inputs; do not launch a simulation")
    p.add_argument("--source", type=Path, required=True, help="Canonical source run recorded in baseline manifest")
    p.add_argument("--model", type=Path, required=True, help="Source EGOML matching baseline model hash")
    p.set_defaults(function=stage)
    p = commands.add_parser("run", help="Explicitly launch three private Dinamica processes")
    p.add_argument("--engine", type=Path, required=True)
    p.add_argument("--processors-per-worker", type=int, choices=(1, 2), default=1)
    p.add_argument("--timeout", type=int, default=600)
    p.add_argument("--disable-native-expressions", action="store_true")
    p.set_defaults(function=run)
    commands.add_parser("gather", help="Gather successful worker outputs in global MC order").set_defaults(function=gather)
    commands.add_parser("compare", help="Require full TIFF/CSV inventory and byte equality with serial").set_defaults(function=compare)
    args = parser.parse_args()
    if hasattr(args, "timeout"):
        require(1 <= args.timeout <= 1800, "Probe timeout must be between 1 and 1800 seconds per worker")
    args.function(args)


if __name__ == "__main__":
    main()
