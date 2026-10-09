"""Stage, run, and compare full-callback web cases in disposable E: storage.

This harness preserves the supplied EGOML byte for byte. It copies a prepared
small case, adjusts only its R executable path and documented R compatibility
settings, and fixes the R sampling seed for a controlled comparison. It never
executes the production launcher or changes a source case. Staging does not run
R or Dinamica. Successful runs still require scientific and output review.

Example (commands are explicit; no simulation is launched by importing this):
  python run_small_web_case.py --root E:/MoFuSS_Active/TASK/cases stage \
    --source E:/MoFuSS_Active/TASK/prepared/webmofuss --model ORIGINAL.egoml --name baseline
  python run_small_web_case.py --root E:/MoFuSS_Active/TASK/cases run --name baseline
  python run_small_web_case.py --root E:/MoFuSS_Active/TASK/cases compare --left baseline --right candidate
"""
from __future__ import annotations

import argparse
import csv
from datetime import datetime, timezone
import hashlib
import io
import json
import os
from pathlib import Path
import re
import shutil
import sqlite3
import struct
import subprocess
import time
import xml.etree.ElementTree as ET

ACTIVE = Path("E:/MoFuSS_Active")
ENGINE = Path("C:/Program Files/Dinamica EGO/DinamicaConsole.exe")
R_EXE = Path("C:/Program Files/R/R-4.6.0/bin/x64/R.exe")
CALLBACKS = ("rnorm_v3", "NRB_graphs_datasets2", "maps_animations7", "finalogs")
SCIENCE_SUFFIXES = {".csv", ".tif", ".tiff", ".img", ".hdr"}
SCHEMA = "webmofuss_small_full_callback_case_v1"
MODEL_NAME = "7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml"
RESULT_SCOPE = "scientific_tables_rasters_and_semantic_geopackages_v2"
ERROR = re.compile(r"^(?:Error(?: in\b|:| during\b)|Execution halted\b|Fatal error:)", re.I)


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def now() -> str:
    return datetime.now(timezone.utc).isoformat()


def sha(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def write_json(path: Path, value: object) -> None:
    path.write_text(json.dumps(value, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")


def read_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def case_path(root: Path, name: str, existing: bool = True) -> Path:
    root, active = root.resolve(), ACTIVE.resolve()
    require(root != active and active in root.parents,
            "Root must be a named task subdirectory strictly below E:/MoFuSS_Active")
    require(bool(re.fullmatch(r"[A-Za-z0-9_-]+", name)), "Invalid case name")
    case = (root / name).resolve()
    require(case.parent == root, "Case escaped its task directory")
    if existing:
        require((case / "_audit/stage_manifest.json").is_file(), "Case has not been staged")
    return case


def files_in(directory: Path) -> list[Path]:
    result = []
    for path in sorted(directory.rglob("*")):
        require(not path.is_symlink() and not path.is_junction(),
                f"Linked paths are not permitted in an isolated case: {path}")
        if path.is_file():
            require(directory.resolve() in path.resolve().parents, "File escaped case")
            result.append(path)
    return result


def inventory(directory: Path, exclude_audit: bool = True) -> dict:
    return {path.relative_to(directory).as_posix(): {
        "sha256": sha(path), "bytes": path.stat().st_size,
    } for path in files_in(directory)
        if not (exclude_audit and path.relative_to(directory).parts[0] == "_audit")}


def adapt_rnorm(text: str, seed: int) -> str:
    lines = text.splitlines(keepends=True)
    imports = [i for i, line in enumerate(lines)
               if re.match(r"^\s*(?:library|require)\s*\(", line)]
    require(bool(imports), "rnorm_v3.R has no recognized package imports")
    first_draw = next((i for i, line in enumerate(lines)
                       if not line.lstrip().startswith("#")
                       and re.search(r"\b(?:rtnorm|rtruncnorm|rnorm|runif|sample)\s*\(", line)), None)
    require(first_draw is not None and max(imports) < first_draw,
            "Cannot safely place seed between package imports and first random draw")
    require(not re.search(r"(?m)^\s*set\.seed\s*\(", text),
            "rnorm_v3.R already sets a seed; review it explicitly")
    lines.insert(max(imports) + 1,
                 f"\n# Disposable comparison only: fixed draws after package loading.\nset.seed({seed})\n")
    return "".join(lines)


def adapt_finalogs(text: str) -> str:
    for package in ("rgdal", "rgeos", "maptools"):
        pattern = rf"(?m)^\s*library\(\s*['\"]?{package}['\"]?\s*\)\s*(?:#.*)?$"
        text, count = re.subn(pattern,
                             f"# Disposable compatibility: unused retired import {package} omitted.", text)
        require(count == 1, f"Expected exactly one {package} import in finalogs.R")
        require(not re.search(rf"\b{package}\s*::", text),
                f"finalogs.R calls {package}; omitting its import is unsafe")
    return text


def validate_model(path: Path) -> None:
    tree = ET.parse(path)
    externals = [node for node in tree.iter("functor") if node.get("name") == "RunExternalProcess"]
    require(len(externals) == 4, "Expected the original four external R calls")
    text = path.read_text(encoding="utf-8-sig")
    for callback in CALLBACKS:
        require(f"{callback}.R" in text, f"Expected callback missing from EGOML: {callback}")
    for node in tree.iter():
        if node.get("name") in {"SaveMap", "SaveTable", "SaveLookupTable"}:
            literal = node.findtext("inputport[@name='filename']")
            if literal:
                raw = literal.strip().strip('"')
                require(not re.match(r"^(?:[A-Za-z]:|[/\\]|\.\.[/\\])", raw),
                        f"Model has an absolute/escaping output path: {raw}")


def stage(args: argparse.Namespace) -> None:
    source, model = args.source.resolve(strict=True), args.model.resolve(strict=True)
    target = case_path(args.root, args.name, existing=False)
    require(not target.exists(), f"Refusing to overwrite case: {target}")
    require(source != target and source not in target.parents and target not in source.parents,
            "Source and destination must be independent directories")
    require(source.is_dir() and not source.is_symlink() and not source.is_junction(),
            "Prepared source must be a real directory")
    require(not (source / "_audit").exists(), "Prepared source already contains a harness audit directory")
    r_exe = args.r_exe.resolve(strict=True)
    validate_model(model)
    for relative in ("In", "LULCC/TempRaster", "LULCC/TempTables", "LaTeX", "ffmpeg64"):
        require((source / relative).is_dir(), f"Prepared runtime directory missing: {relative}")
    for relative in (*(f"{name}.R" for name in CALLBACKS),
                     "LaTeX/generate_modern_report.R", "ffmpeg64/bin/ffmpeg.exe",
                     "LULCC/TempTables/Rpath.csv", "LULCC/TempTables/parameters_dinamica.csv"):
        require((source / relative).is_file(), f"Prepared runtime file missing: {relative}")
    original = inventory(source, exclude_audit=False)
    size = sum(item["bytes"] for item in original.values())
    require(size <= args.max_copy_gib * 1024 ** 3,
            f"Prepared source is {size / 1024**3:.2f} GiB; exceeds --max-copy-gib")
    # Copy only this explicitly prepared small case, never a backup/global-data root.
    shutil.copytree(source, target, copy_function=shutil.copy2)
    require(inventory(target, exclude_audit=False) == original, "Prepared-case copy failed hash verification")
    audit = target / "_audit"
    audit.mkdir()
    changes = []

    def change(relative: str, content: str, reason: str) -> None:
        path = target / relative
        before = sha(path)
        path.write_text(content, encoding="utf-8", newline="")
        changes.append({"path": relative, "reason": reason,
                        "before_sha256": before, "after_sha256": sha(path)})

    relative = "LULCC/TempTables/Rpath.csv"
    rows = list(csv.DictReader(io.StringIO((target / relative).read_text(encoding="utf-8-sig"))))
    require(len(rows) == 1 and list(rows[0]) == ["Key*", "Rpath"]
            and rows[0]["Key*"] == "1", "Unexpected Rpath table shape/numeric key")
    # Dinamica 2.4 infers a quoted "1" as a string key. Preserve the original
    # numeric key when writing this fixture-only executable-path adjustment.
    rows[0]["Key*"] = 1
    rows[0]["Rpath"] = r_exe.as_posix()
    buffer = io.StringIO(newline="")
    writer = csv.DictWriter(buffer, fieldnames=list(rows[0]), lineterminator="\n", quoting=csv.QUOTE_NONNUMERIC)
    writer.writeheader()
    writer.writerows(rows)
    change(relative, buffer.getvalue(), "Use the verified local R executable in this case only")
    change("rnorm_v3.R", adapt_rnorm((target / "rnorm_v3.R").read_text(encoding="utf-8-sig"), args.seed),
           "Fixed R seed after imports and before sampling, for both sides of comparison")
    change("finalogs.R", adapt_finalogs((target / "finalogs.R").read_text(encoding="utf-8-sig")),
           "Omit three unused retired package imports from log finalization only")
    relative = "LaTeX/generate_modern_report.R"
    helper = (target / relative).read_text(encoding="utf-8-sig")
    require(helper.count('"--enable-installer"') == 1, "Unexpected report installer option")
    change(relative, helper.replace('"--enable-installer"', '"--disable-installer"'),
           "Forbid automatic MiKTeX package installation during isolated report testing")
    # Use the deployed filename so reporting/log collection sees the actual
    # selected model. Only this newly copied disposable case is changed.
    staged_model = target / MODEL_NAME
    previous_model_hash = sha(staged_model) if staged_model.exists() else None
    shutil.copy2(model, staged_model)
    require(sha(model) == sha(staged_model), "EGOML bytes changed during copy")
    changes.append({"path": MODEL_NAME,
                    "reason": "Select baseline or candidate under the deployed filename in this isolated copy",
                    "before_sha256": previous_model_hash, "after_sha256": sha(staged_model)})
    for name in ("Logs", "Out", "Summary_Report", "HTML_animation"):
        (target / name).mkdir(exist_ok=True)
    manifest = {
        "schema": SCHEMA, "created_utc": now(), "source": str(source),
        "model_source": str(model), "model_sha256": sha(model),
        "staged_model": staged_model.name, "egoml_modified": False,
        "r_executable": str(r_exe), "r_seed": args.seed,
        "source_files": len(original), "source_bytes": size,
        "adaptations": changes,
        "scope": "Full four-callback local small-case test; no production launcher or network handoff",
    }
    write_json(audit / "prepared_source_inventory.json", original)
    write_json(audit / "staged_inventory.json", inventory(target))
    write_json(audit / "stage_manifest.json", manifest)
    print(json.dumps({"staged": str(target), **manifest}, indent=2))


def callback_status(case: Path, before: dict) -> dict:
    result = {}
    for name in CALLBACKS:
        path, relative = case / f"{name}.Rout", f"{name}.Rout"
        if not path.is_file():
            result[name] = {"status": "missing", "path": relative}
            continue
        digest = sha(path)
        text = path.read_text(encoding="utf-8", errors="replace")
        errors = [line for line in text.splitlines() if ERROR.search(line)]
        fresh = relative not in before or before[relative]["sha256"] != digest
        ended = bool(re.search(r"(?m)^>\s*proc\.time\(\)\s*$", text))
        status = "failed" if errors else "complete" if fresh and ended else "unverified"
        result[name] = {"status": status, "path": relative, "sha256": digest,
                        "fresh": fresh, "batch_completion_marker": ended,
                        "errors": errors[:40]}
    return result


def collect_outputs(case: Path, before: dict) -> tuple[dict, dict]:
    current = inventory(case)
    produced = {relative: item for relative, item in current.items()
                if relative not in before or item["sha256"] != before[relative]["sha256"]}
    science = {}
    for relative, item in produced.items():
        parts = Path(relative).parts
        parent = parts[0].lower()
        in_model_tables = len(parts) > 2 and [part.lower() for part in parts[:2]] == ["lulcc", "temptables"]
        if (in_model_tables or parent in {"temp", "debugging", "out", "summary_report", "sourcing"}
                or re.fullmatch(r"debugging_[0-9]+", parent)) and Path(relative).suffix.lower() in SCIENCE_SUFFIXES:
            science[relative] = item["sha256"]
    return science, produced


def canonical_hash(value: object) -> str:
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False, allow_nan=False)
    return hashlib.sha256(encoded.encode("utf-8")).hexdigest()


def sqlite_value(value: object) -> list:
    """Preserve SQLite value types, including exact finite float values."""
    if value is None:
        return ["null"]
    if isinstance(value, int):
        return ["integer", str(value)]
    if isinstance(value, float):
        return ["real", value.hex()]
    if isinstance(value, str):
        return ["text", value]
    if isinstance(value, bytes):
        return ["blob", value.hex()]
    raise ValueError(f"Unsupported SQLite attribute value: {type(value).__name__}")


def geometry_value(blob: bytes | None, srs_id: int, declared_type: str) -> list:
    """Normalize standard 2D GeoPackage/WKB geometry, without tolerances."""
    if blob is None:
        return ["null"]
    require(isinstance(blob, bytes) and len(blob) >= 13, "Invalid GeoPackage geometry blob")
    require(blob[:3] == b"GP\x00", "Unsupported GeoPackage geometry magic/version")
    flags = blob[3]
    require(flags & 0xE0 == 0, "Extended/reserved GeoPackage geometry flags are unsupported")
    envelope = (flags >> 1) & 7
    require(envelope in (0, 1), "Only standard XY GeoPackage envelopes are supported")
    endian = "<" if flags & 1 else ">"
    require(struct.unpack_from(endian + "i", blob, 4)[0] == srs_id,
            "GeoPackage geometry SRS differs from its column declaration")
    offset = 8 + (32 if envelope else 0)
    require(len(blob) >= offset + 5, "Truncated GeoPackage WKB")
    wkb = blob[offset:]
    require(wkb[0] in (0, 1), "Unsupported WKB byte order")
    kind = struct.unpack_from(("<" if wkb[0] else ">") + "I", wkb, 1)[0]
    require(1 <= kind <= 7, "Only standard 2D OGC geometry types are supported")
    try:
        import shapely
    except ImportError as error:
        raise ValueError("Semantic GeoPackage comparison requires installed Shapely 2") from error
    require(all(hasattr(shapely, name) for name in
                ("from_wkb", "to_wkb", "normalize", "get_coordinate_dimension")),
            "Semantic GeoPackage comparison requires Shapely 2 APIs")
    geometry = shapely.from_wkb(wkb, on_invalid="raise")
    require(geometry is not None and shapely.get_coordinate_dimension(geometry) == 2,
            "Only 2D GeoPackage geometries are supported, including nested members")
    require(declared_type == "GEOMETRY" or geometry.geom_type.upper() == declared_type,
            "Geometry type differs from its GeoPackage column declaration")
    require(bool(flags & 16) == geometry.is_empty, "GeoPackage empty flag disagrees with its geometry")
    roundtrip = shapely.to_wkb(geometry, byte_order=wkb[0], output_dimension=2, include_srid=False)
    require(len(roundtrip) == len(wkb), "Nonstandard or trailing WKB geometry data is unsupported")
    # GEOS normalization handles ring/component ordering and winding. Retain
    # every coordinate and require exact equality; no rounding or simplification.
    canonical = shapely.to_wkb(shapely.normalize(geometry), byte_order=1,
                               output_dimension=2, include_srid=False)
    return ["geometry", srs_id, geometry.geom_type, geometry.is_empty,
            hashlib.sha256(canonical).hexdigest()]


def gpkg_semantics(path: Path) -> dict:
    """Read feature content without comparing SQLite layout/indexes/timestamps.

    Fail closed for tiles, nonspatial contents, Z/M/curved geometries, unknown
    extensions, or unregistered tables. CRS definitions and column order/types
    are compared exactly; this does not infer equivalence of differing CRSs.
    """
    def identifier(name: str) -> str:
        return '"' + name.replace('"', '""') + '"'

    with sqlite3.connect(path.resolve().as_uri() + "?mode=ro", uri=True) as connection:
        connection.execute("PRAGMA query_only=ON")
        require(connection.execute("PRAGMA quick_check").fetchall() == [("ok",)],
                f"GeoPackage integrity check failed: {path}")
        objects = connection.execute("SELECT name, type FROM sqlite_master WHERE type IN ('table', 'view')").fetchall()
        tables = {name for name, kind in objects if kind == "table"}
        require(not any(kind == "view" for _, kind in objects), "GeoPackage views are not supported")
        require({"gpkg_contents", "gpkg_geometry_columns", "gpkg_spatial_ref_sys"} <= tables,
                "GeoPackage core feature metadata is missing")
        columns = [row[1] for row in connection.execute("PRAGMA table_info(gpkg_contents)")]
        require("last_change" in columns, "GeoPackage contents timestamp column missing")
        kept = [column for column in columns if column != "last_change"]
        contents = connection.execute("SELECT " + ",".join(map(identifier, kept)) + " FROM gpkg_contents").fetchall()
        contents = [dict(zip(kept, row)) for row in contents]
        require(bool(contents) and all(row["data_type"] == "features" for row in contents),
                "Only nonempty feature-only GeoPackages are supported")
        registered = {row["table_name"] for row in contents}
        geometry_rows = connection.execute("SELECT table_name,column_name,geometry_type_name,srs_id,z,m FROM gpkg_geometry_columns").fetchall()
        require(len(geometry_rows) == len(registered) and {row[0] for row in geometry_rows} == registered,
                "Expected exactly one registered geometry column per feature table")
        geometry_info = {row[0]: row[1:] for row in geometry_rows}
        extensions = connection.execute("SELECT table_name,column_name,extension_name,definition,scope FROM gpkg_extensions").fetchall() if "gpkg_extensions" in tables else []
        for table, column, extension, _, scope in extensions:
            require(table in registered and column == geometry_info[table][0]
                    and extension == "gpkg_rtree_index" and scope == "write-only",
                    f"Unsupported GeoPackage extension: {extension}")
        allowed = {"gpkg_contents", "gpkg_geometry_columns", "gpkg_spatial_ref_sys", "gpkg_extensions",
                   "gpkg_ogr_contents", "gpkg_tile_matrix_set", "gpkg_tile_matrix", "sqlite_sequence"} | registered
        for table, (column, *_) in geometry_info.items():
            base = f"rtree_{table}_{column}"
            allowed.update({base, base + "_rowid", base + "_node", base + "_parent"})
        require(tables <= allowed, f"Unsupported/unregistered GeoPackage tables: {sorted(tables - allowed)}")
        for table in ("gpkg_tile_matrix_set", "gpkg_tile_matrix"):
            if table in tables:
                require(connection.execute(f"SELECT COUNT(*) FROM {identifier(table)}").fetchone()[0] == 0,
                        "GeoPackage tile metadata is unsupported")
        layers = {}
        for item in sorted(contents, key=lambda row: row["table_name"]):
            table = item["table_name"]
            column, kind, srs_id, z, m = geometry_info[table]
            require(z == 0 and m == 0, "GeoPackage Z/M geometry columns are unsupported")
            require(kind.upper() in {"GEOMETRY", "POINT", "LINESTRING", "POLYGON", "MULTIPOINT",
                                     "MULTILINESTRING", "MULTIPOLYGON", "GEOMETRYCOLLECTION"},
                    f"Unsupported declared geometry type: {kind}")
            schema = connection.execute(f"PRAGMA table_info({identifier(table)})").fetchall()
            names = [entry[1] for entry in schema]
            require(column in names, "Declared GeoPackage geometry column is absent")
            geometry_index = names.index(column)
            rows = []
            for row in connection.execute(f"SELECT * FROM {identifier(table)}"):
                values = [geometry_value(value, srs_id, kind.upper()) if index == geometry_index
                          else sqlite_value(value) for index, value in enumerate(row)]
                rows.append(canonical_hash(values))
            crs = connection.execute("SELECT * FROM gpkg_spatial_ref_sys WHERE srs_id=?", (srs_id,)).fetchall()
            require(len(crs) == 1, "Feature CRS must resolve to exactly one stored SRS definition")
            require(item["srs_id"] == srs_id, "Contents/geometry CRS declarations disagree")
            layers[table] = {
                "features": len(rows), "geometry_type": kind, "srs_id": srs_id,
                "schema_sha256": canonical_hash([schema, geometry_info[table]]),
                "contents_without_timestamp_sha256": canonical_hash(item),
                "crs_sha256": canonical_hash(crs),
                "features_sha256": canonical_hash(sorted(rows)),
            }
        return {"method": "gpkg_features_xy_geos_normalized_exact_v1", "layers": layers,
                "semantic_sha256": canonical_hash(layers)}


def collect_geopackages(case: Path, produced: dict) -> dict:
    return {relative: gpkg_semantics(case / relative) for relative in sorted(produced)
            if Path(relative).suffix.lower() == ".gpkg"}


def run(args: argparse.Namespace) -> None:
    case = case_path(args.root, args.name)
    audit = case / "_audit"
    require(not (audit / "run_started.json").exists(), "Already started; stage a fresh case instead")
    manifest = read_json(audit / "stage_manifest.json")
    before = read_json(audit / "staged_inventory.json")
    require(inventory(case) == before, "Case changed after staging")
    require(0 <= args.processors <= 8 and 1 <= args.omp_threads <= 8,
            "The confirmed local ceiling is eight; processors 0 means production automatic detection")
    engine = args.engine.resolve(strict=True)
    private_temp = audit / "engine_temp"
    private_temp.mkdir()
    environment = os.environ.copy()
    environment.update(TEMP=str(private_temp), TMP=str(private_temp), TMPDIR=str(private_temp),
                       OMP_NUM_THREADS=str(args.omp_threads))
    # Suppress startup profiles in batch R without changing user/global settings.
    # Keep package libraries as installed on this host. Locale is recorded, not rewritten.
    environment["R_ENVIRON_USER"] = str(audit / "absent_Renviron")
    environment["R_PROFILE_USER"] = str(audit / "absent_Rprofile")
    command = [str(engine), "-processors", str(args.processors), "-log-level", "4"]
    if not args.random_engine_seed:
        command.append("-predefined-seed")
    if args.disable_native_expressions:
        command.append("-disable-native-expressions")
    command.append(str(case / manifest["staged_model"]))
    details = {"schema": SCHEMA, "started_utc": now(), "command": command,
               "engine_sha256": sha(engine), "processors": args.processors,
               "omp_num_threads": args.omp_threads,
               "predefined_engine_seed": not args.random_engine_seed,
               "disable_native_expressions": args.disable_native_expressions,
               "private_temp": str(private_temp), "competing_load_note": args.load_note,
               "environment_overrides": {key: environment[key] for key in
                   ("TEMP", "TMP", "TMPDIR", "OMP_NUM_THREADS", "R_ENVIRON_USER", "R_PROFILE_USER")},
               "locale_environment": {key: environment.get(key) for key in ("LANG", "LC_ALL", "LC_CTYPE")}}
    write_json(audit / "run_started.json", details)
    started = time.perf_counter()
    with (audit / "runtime.log").open("wb") as log:
        process = subprocess.Popen(command, cwd=case, env=environment,
                                   stdout=log, stderr=subprocess.STDOUT)
        details["pid"] = process.pid
        write_json(audit / "run_started.json", details)
        try:
            returncode = process.wait(timeout=args.timeout or None)
        except subprocess.TimeoutExpired:
            # Leave the explicitly started job intact: killing only the parent
            # could leave R/LaTeX children writing after a false completion report.
            details.update(status="still_running_after_observation_timeout", returncode=None,
                           elapsed_seconds=time.perf_counter() - started,
                           warning="Job was not terminated; do not rerun or compare this case yet")
            write_json(audit / "runtime_result.json", details)
            print(json.dumps(details, indent=2))
            raise SystemExit(2)
    elapsed = time.perf_counter() - started
    log_text = (audit / "runtime.log").read_text(encoding="utf-8", errors="replace")
    engine_success = "Model script ran successfully" in log_text
    callbacks = callback_status(case, before)
    science, produced = collect_outputs(case, before)
    geopackages = collect_geopackages(case, produced)
    full_success = returncode == 0 and engine_success and bool(science) and all(
        item["status"] == "complete" for item in callbacks.values())
    details.update(status="complete" if full_success else "failed_or_incomplete",
                   ended_utc=now(), returncode=returncode, elapsed_seconds=elapsed,
                   engine_success_message=engine_success, callbacks=callbacks,
                   full_callback_success=full_success, scientific_outputs=len(science),
                   semantic_geopackage_outputs=len(geopackages), comparison_scope=RESULT_SCOPE,
                   produced_files=len(produced))
    write_json(audit / "scientific_output_sha256.json", science)
    write_json(audit / "geopackage_semantics.json", geopackages)
    write_json(audit / "produced_files.json", produced)
    write_json(audit / "runtime_result.json", details)
    print(json.dumps(details, indent=2))
    if not full_success:
        raise SystemExit(1)


def refresh_results(args: argparse.Namespace) -> None:
    """Re-audit a completed run; never launch engines or modify case inputs."""
    case = case_path(args.root, args.name)
    audit = case / "_audit"
    result = read_json(audit / "runtime_result.json")
    require(result.get("status") == "complete" and result.get("full_callback_success")
            and result.get("returncode") == 0 and result.get("ended_utc"),
            "Only completed successful cases may be refreshed; active/failed runs are refused")
    manifest = read_json(audit / "stage_manifest.json")
    require(sha(case / manifest["staged_model"]) == manifest["model_sha256"],
            "Staged model changed since the completed run")
    before = read_json(audit / "staged_inventory.json")
    callbacks = callback_status(case, before)
    require(all(item["status"] == "complete" for item in callbacks.values()),
            "Completed callback evidence is missing or now reports errors")
    science, produced = collect_outputs(case, before)
    geopackages = collect_geopackages(case, produced)
    require(bool(science), "No scientific outputs found in completed case")
    # Preserve the prior complete result, including the exact original load
    # note, before a user-requested metadata correction or scope amendment.
    refreshed = now()
    history = audit / "refresh_history"
    history.mkdir(exist_ok=True)
    write_json(history / f"runtime_result_{time.time_ns()}.json", {
        "recorded_utc": refreshed, "operation": "refresh-results",
        "previous_runtime_result": result,
        "requested_load_note": args.load_note,
    })
    result = dict(result)
    if args.load_note is not None:
        result["competing_load_note"] = args.load_note
    result.update(callbacks=callbacks, scientific_outputs=len(science),
                  semantic_geopackage_outputs=len(geopackages), produced_files=len(produced),
                  comparison_scope=RESULT_SCOPE, results_refreshed_utc=refreshed)
    write_json(audit / "scientific_output_sha256.json", science)
    write_json(audit / "geopackage_semantics.json", geopackages)
    write_json(audit / "produced_files.json", produced)
    write_json(audit / "runtime_result.json", result)
    print(json.dumps({"refreshed": str(case), "rerun": False,
                      "scientific_outputs": len(science), "semantic_geopackage_outputs": len(geopackages),
                      "competing_load_note": result["competing_load_note"],
                      "results_refreshed_utc": refreshed}, indent=2))


def compare(args: argparse.Namespace) -> None:
    left, right = (case_path(args.root, name) for name in (args.left, args.right))
    a_run, b_run = (read_json(case / "_audit/runtime_result.json") for case in (left, right))
    require(a_run.get("full_callback_success") and b_run.get("full_callback_success"),
            "Both cases must have completed all callbacks successfully")
    require(a_run.get("comparison_scope") == RESULT_SCOPE and b_run.get("comparison_scope") == RESULT_SCOPE,
            "Refresh both completed cases with refresh-results before using the expanded comparison")
    a_in, b_in = (read_json(case / "_audit/staged_inventory.json") for case in (left, right))
    # The one selected model is expected to differ. All prepared inputs and
    # callback adaptations must match exactly, including the fixed R seed.
    for values in (a_in, b_in):
        values.pop(MODEL_NAME, None)
    require(a_in == b_in, "Prepared inputs or callback adaptations differ")
    for field in ("engine_sha256", "predefined_engine_seed", "disable_native_expressions"):
        require(a_run[field] == b_run[field], f"Runtime comparability setting differs: {field}")
    a, b = (read_json(case / "_audit/scientific_output_sha256.json") for case in (left, right))
    common = sorted(a.keys() & b.keys())
    different = [path for path in common if a[path] != b[path]]
    missing, added = sorted(a.keys() - b.keys()), sorted(b.keys() - a.keys())
    a_gpkg, b_gpkg = (read_json(case / "_audit/geopackage_semantics.json") for case in (left, right))
    gpkg_common = sorted(a_gpkg.keys() & b_gpkg.keys())
    gpkg_different = [path for path in gpkg_common if a_gpkg[path] != b_gpkg[path]]
    gpkg_missing, gpkg_added = sorted(a_gpkg.keys() - b_gpkg.keys()), sorted(b_gpkg.keys() - a_gpkg.keys())
    gpkg_equal = not (gpkg_different or gpkg_missing or gpkg_added)
    a_files, b_files = (read_json(case / "_audit/produced_files.json") for case in (left, right))
    result = {"schema": SCHEMA, "left": args.left, "right": args.right,
              "scientific_common": len(common), "scientific_identical": len(common) - len(different),
              "scientific_different": different, "scientific_missing": missing, "scientific_added": added,
              "all_scientific_outputs_byte_identical": bool(a) and not (different or missing or added),
              "geopackage_common": len(gpkg_common), "geopackage_semantically_identical": len(gpkg_common) - len(gpkg_different),
              "geopackage_different": gpkg_different, "geopackage_missing": gpkg_missing, "geopackage_added": gpkg_added,
              "all_geopackage_outputs_semantically_identical": gpkg_equal,
              "all_compared_scientific_outputs_match": bool(a) and not (different or missing or added) and gpkg_equal,
              "comparison_scope": RESULT_SCOPE,
              "produced_file_names_missing": sorted(a_files.keys() - b_files.keys()),
              "produced_file_names_added": sorted(b_files.keys() - a_files.keys()),
              "baseline_seconds": a_run["elapsed_seconds"], "candidate_seconds": b_run["elapsed_seconds"],
              "baseline_processors": a_run["processors"], "candidate_processors": b_run["processors"],
              "baseline_load_note": a_run["competing_load_note"], "candidate_load_note": b_run["competing_load_note"],
              "limits": "Byte hashes cover scientific tables/rasters, including changed LULCC/TempTables files. "
                        "GeoPackages compare typed attributes, normalized exact 2D geometries, schemas and stored CRS definitions; "
                        "SQLite layout/indexes and gpkg_contents.last_change are ignored. Unsupported structures are rejected. "
                        "Equivalent CRS text or coordinate rounding differences may still be reported as differences. "
                        "Hashes do not substitute for scientific/raster review; reports may contain timestamps. "
                        "Elapsed times include all callbacks and may be affected by competing workloads."}
    write_json(args.root.resolve() / f"comparison_{args.left}_vs_{args.right}.json", result)
    print(json.dumps(result, indent=2))
    if not result["all_compared_scientific_outputs_match"]:
        raise SystemExit(1)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--root", type=Path, required=True)
    commands = parser.add_subparsers(dest="operation", required=True)
    item = commands.add_parser("stage", help="Copy/adapt a prepared small case; do not execute it")
    item.add_argument("--source", type=Path, required=True)
    item.add_argument("--model", type=Path, required=True)
    item.add_argument("--name", required=True)
    item.add_argument("--r-exe", type=Path, default=R_EXE)
    item.add_argument("--seed", type=int, default=20261008)
    item.add_argument("--max-copy-gib", type=float, default=2.0)
    item.set_defaults(function=stage)
    item = commands.add_parser("run", help="Execute a staged case, retaining all four R callbacks")
    item.add_argument("--name", required=True)
    item.add_argument("--engine", type=Path, default=ENGINE)
    item.add_argument("--processors", type=int, default=2, help="0=production automatic detection; local ceiling=8")
    item.add_argument("--omp-threads", type=int, default=2)
    item.add_argument("--timeout", type=float, default=0,
                      help="Observation timeout seconds; 0 waits until completion. Timeout does NOT kill the job")
    item.add_argument("--random-engine-seed", action="store_true")
    item.add_argument("--disable-native-expressions", action="store_true")
    item.add_argument("--load-note", default="Four pre-existing legacy Dinamica jobs observed; recheck load before launch")
    item.set_defaults(function=run)
    item = commands.add_parser("refresh-results", aliases=["audit"],
                               help="Refresh comparison artifacts for a completed successful case; never rerun it")
    item.add_argument("--name", required=True)
    item.add_argument("--load-note", default=None,
                      help="Correct the competing-load note while retaining its previous value in refresh history")
    item.set_defaults(function=refresh_results)
    item = commands.add_parser("compare", help="Compare successful full-callback cases")
    item.add_argument("--left", required=True)
    item.add_argument("--right", required=True)
    item.set_defaults(function=compare)
    args = parser.parse_args()
    args.function(args)


if __name__ == "__main__":
    main()
