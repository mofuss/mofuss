"""Stage and run official C++ IDW for an isolated small web-model fixture.

Requires numpy/rasterio and the separately audited OMP_specificYear build.
Does not compile/download/install anything. Stage never runs the executable.
Source demand tables and model rasters are unchanged. Private C++ CSV copies
convert tonnes to kg because upstream divides by 1000; private raster copies
normalize NoData representation, retaining exact valid values and geometry.

  python prepare_cpp_idw_fixture.py --case E:/MoFuSS_Active/TASK/case stage
  python prepare_cpp_idw_fixture.py --case E:/MoFuSS_Active/TASK/case run --start 1 --end 1
  # Inspect the first result, then record the review explicitly:
  python prepare_cpp_idw_fixture.py --case E:/MoFuSS_Active/TASK/case review --note "First-period checks reviewed"
  python prepare_cpp_idw_fixture.py --case E:/MoFuSS_Active/TASK/case run --start 2 --end 51

The explicit fixture scenario is relative friction, 12-hour exploration, IDW
exponent 1. These are not claimed to reproduce the absent server R wrapper.
The upstream C++ uses pixel width for all orthogonal movement costs, including
on rectangular cells. Full original grid metadata is retained without resampling.
"""
from __future__ import annotations

import argparse
import csv
from datetime import datetime, timezone
from decimal import Decimal
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time

import numpy as np
import rasterio

ACTIVE = Path("E:/MoFuSS_Active")
DEFAULT_ENGINE = ACTIVE / "webmofuss_performance_audit/costdistance_source/build_specific_year/CostDistance_IDW_specificYear.exe"
ENGINE_SHA = "0e91b219a3b2150ee125cf36d39b437477a7b7a056180579995c6cb2301a2fb0"
SOURCE_COMMIT = "cdb1c36453f3aa6d9906c26526a8d63f5bfd9964"
SCHEMA = "webmofuss_official_cpp_idw_fixture_v1"
NODATA = -9999.0


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def sha(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def emit(path: Path, value: object) -> None:
    path.write_text(json.dumps(value, indent=2, allow_nan=False) + "\n", encoding="utf-8")


def read(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def now() -> str:
    return datetime.now(timezone.utc).isoformat()


def inside(case: Path, relative: str) -> Path:
    raw = Path(relative)
    require(not raw.is_absolute() and ".." not in raw.parts, "Unsafe relative fixture path")
    path = case / raw
    require(case in path.resolve().parents, "Fixture path escaped case")
    for current in (path, *path.parents):
        if current == case:
            break
        require(not current.is_symlink() and not current.is_junction(), f"Linked fixture path: {current}")
    return path


def case_root(path: Path) -> Path:
    case, active = path.resolve(strict=True), ACTIVE.resolve()
    require(case.is_dir() and active in case.parents and len(case.relative_to(active).parts) >= 2,
            "Use a prepared case strictly below a named E:/MoFuSS_Active task directory")
    for current in (case, *case.parents):
        if current == active.parent:
            break
        require(not current.is_symlink() and not current.is_junction(), "Linked case directory")
    return case


def fingerprint(values: np.ndarray) -> str:
    return hashlib.sha256(np.asarray(values, dtype="<f8").tobytes()).hexdigest()


def geometry(ds) -> dict:
    return {"shape": [ds.height, ds.width], "transform": list(ds.transform), "crs": ds.crs.to_wkt()}


def same_geometry(left: dict, right: dict) -> bool:
    # Legacy Dinamica can omit descriptive CRS names/authority labels while
    # retaining the exact same projection. Compare CRS meaning, not WKT text;
    # the original WKT strings remain in the manifest. Grid checks stay exact.
    return (left["shape"] == right["shape"] and left["transform"] == right["transform"]
            and rasterio.crs.CRS.from_wkt(left["crs"]) == rasterio.crs.CRS.from_wkt(right["crs"]))


def load_map(path: Path, max_cells: int) -> tuple[np.ndarray, np.ndarray, dict]:
    with rasterio.open(path) as ds:
        require(ds.count == 1 and ds.width * ds.height <= max_cells, f"Not a bounded single-band map: {path}")
        require(ds.crs is not None and ds.crs.is_projected, f"A projected CRS is required: {path}")
        require(ds.crs.linear_units in {"metre", "meter"}, f"Relative friction requires metre coordinates: {path}")
        t = ds.transform
        require(np.isfinite(list(t)).all() and t.a > 0 and t.e < 0 and t.b == 0 and t.d == 0,
                f"An unrotated north-up grid is required: {path}")
        values, valid = ds.read(1), ds.read_masks(1) > 0
        require(valid.any() and np.isfinite(values[valid]).all(), f"Empty/nonfinite valid map values: {path}")
        require(np.isfinite(values[valid].astype(np.float32)).all(), f"Map overflows upstream float32: {path}")
        info = {**geometry(ds), "dtype": ds.dtypes[0], "nodata": str(ds.nodata),
                "cell_width_metres": t.a, "cell_height_metres": -t.e,
                "width_height_ratio": t.a / -t.e,
                "upstream_cost_scale_float32_metres": float(np.float32(t.a)),
                "valid_cells": int(valid.sum()), "invalid_cells": int((~valid).sum()),
                "valid_values_sha256": fingerprint(values[valid]),
                "valid_mask_sha256": hashlib.sha256(valid.tobytes()).hexdigest()}
    return values, valid, info


def demand_header(path: Path, channel: str) -> tuple[list[str], list[int]]:
    # Prepared rasters are small, but preparation retains the national table.
    # Read/convert those tables as streams; do not hold millions of cells.
    require(path.stat().st_size <= 512 * 1024**2, "Demand CSV exceeds bounded fixture size")
    with path.open(encoding="utf-8-sig", newline="") as stream:
        header = next(csv.reader(stream), [])
    require(2 <= len(header) <= 52 and len(set(header)) == len(header), "Expected ID plus 1..51 annual columns")
    require(all(re.fullmatch(rf"[0-9]{{4}}_fw_{channel}", col) for col in header[1:]),
            "Full demand headers must be YYYY_fw_w / YYYY_fw_v, not Key,Value")
    years = [int(col[:4]) for col in header[1:]]
    require(years == list(range(years[0], years[0] + len(years))), "Annual columns must be consecutive and ordered")
    return header, years


def convert_demand(path: Path, destination: Path, header: list[str], origin_ids: set[int]) -> dict:
    ids: set[int] = set()
    active = {str(index): [] for index in range(1, len(header))}
    float32_max = Decimal(str(float(np.finfo(np.float32).max)))
    with path.open(encoding="utf-8-sig", newline="") as source, destination.open("x", encoding="utf-8", newline="") as output:
        reader, writer = csv.reader(source), csv.writer(output, lineterminator="\n")
        require(next(reader) == header, "Demand header changed while staging")
        writer.writerow(header)
        for row in reader:
            require(len(row) == len(header), f"Ragged CSV row: {path}")
            key = Decimal(row[0])
            require(key.is_finite() and key == int(key) and 0 <= key <= 2**24,
                    "Category ID would be truncated/collide in upstream float32/int parser")
            numeric_key = int(key)
            require(numeric_key not in ids, "Duplicate demand category ID")
            ids.add(numeric_key)
            converted = [str(numeric_key)]
            for index, text in enumerate(row[1:], 1):
                value = Decimal(text)
                require(value.is_finite() and value >= 0, "Demand must be finite and nonnegative")
                kg = value * 1000
                require(kg <= float32_max, "kg input would overflow upstream float32")
                converted.append(format(kg, "f"))
                if numeric_key in origin_ids and value > 0:
                    active[str(index)].append(numeric_key)
            writer.writerow(converted)
    require(bool(ids), "Empty demand table")
    require(origin_ids.issubset(ids), "Origin categories are missing from the demand table")
    return {"demand_ids": len(ids), "mapped_origin_ids": len(origin_ids),
            "table_ids_outside_cropped_raster": len(ids - origin_ids),
            "active_origin_ids_by_period": active}


def resolve_demand(case: Path, channel: str, supplied: str | None) -> Path:
    if supplied:
        result = inside(case, supplied)
    else:
        candidates = sorted((case / "In/DemandScenarios").glob(f"*_fwch_{channel}.csv"))
        require(len(candidates) == 1, f"Supply --demand-{channel}; expected one full scenario CSV")
        result = candidates[0]
    require(result.is_file(), f"Missing full demand CSV: {result}")
    return result


def copy_map(source: Path, target: Path, values: np.ndarray, valid: np.ndarray, info: dict) -> dict:
    # Float64 stores original integer/float32/float64 values exactly; upstream
    # still requests GDT_Float32 on read, just as it would for the source map.
    data = values.astype(np.float64)
    require(np.array_equal(data[valid], values[valid]), "Valid values would change during representation conversion")
    require(not (data[valid] == NODATA).any(), "Private NoData sentinel collides with valid data")
    data[~valid] = NODATA
    with rasterio.open(source) as ds:
        profile = {"driver": "GTiff", "height": ds.height, "width": ds.width,
                   "count": 1, "dtype": "float64", "crs": ds.crs,
                   "transform": ds.transform, "nodata": NODATA, "compress": "LZW"}
    with rasterio.open(target, "w", **profile) as ds:
        ds.write(data, 1)
    copied, mask, copied_info = load_map(target, data.size)
    require(np.array_equal(mask, valid) and np.array_equal(copied[valid], values[valid]),
            "Private raster changed values or NoData mask")
    require(same_geometry(copied_info, info),
            "Private raster geometry changed")
    return {"source_sha256": sha(source), "private_sha256": sha(target),
            "source": info, "private": copied_info,
            "adaptation": "NoData normalized to -9999; exact valid values, mask, CRS and grid retained"}


def stage(args: argparse.Namespace) -> None:
    case = case_root(args.case)
    audit = inside(case, "_cpp_idw")
    require(not audit.exists(), "Refusing to overwrite an existing C++ IDW fixture")
    engine = args.engine.resolve(strict=True)
    require(args.engine_sha256 == ENGINE_SHA, "This adapter is validated only with its pinned audited executable")
    require(sha(engine) == args.engine_sha256, "C++ executable does not match its audited SHA256")
    require(args.max_cells > 0 and args.hours > 0 and args.exponent > 0, "Invalid fixture limits/settings")
    all_inputs, plan = {}, {}
    common_years = None
    for channel in ("w", "v"):
        table = resolve_demand(case, channel, getattr(args, f"demand_{channel}"))
        header, years = demand_header(table, channel)
        if common_years is None:
            common_years = years
        require(years == common_years, "Walking and vehicle annual columns differ")
        friction = inside(case, f"In/fricc_{channel}.tif")
        origins = inside(case, f"LULCC/TempRaster/locs_c_{channel}.tif")
        f, fm, fi = load_map(friction, args.max_cells)
        o, om, oi = load_map(origins, args.max_cells)
        require(same_geometry(fi, oi), "Friction/origin geometry mismatch")
        require((f[fm] > 0).all(), "Friction valid cells must be positive")
        origin_values = o[om]
        require(np.equal(origin_values, np.floor(origin_values)).all()
                and ((origin_values >= 0) & (origin_values <= 2**24)).all(),
                "Origin IDs would truncate/collide in upstream numeric types")
        origin_ids = origin_values.astype(np.int64)
        require(len(np.unique(origin_ids)) == len(origin_ids),
                "Repeated category cells would be silently reduced to one location by upstream")
        for source in (table, friction, origins):
            all_inputs[source.relative_to(case).as_posix()] = sha(source)
        for period in range(1, len(years) + 1):
            require(not inside(case, f"In/IDW_C++_fw_{channel}{period:02d}.tif").exists(),
                    "Existing annual IDW output: refuse to overwrite")
        plan[channel] = (table, header, friction, origins, f, fm, fi, o, om, oi, origin_ids)
    audit.mkdir()
    private = audit / "inputs"
    private.mkdir()
    channels = {}
    for channel, item in plan.items():
        table, header, friction, origins, f, fm, fi, o, om, oi, origin_ids = item
        destination = private / f"demand_{channel}_kg.csv"
        origin_set = set(int(x) for x in origin_ids)
        demand_summary = convert_demand(table, destination, header, origin_set)
        channels[channel] = {
            "source_demand": table.relative_to(case).as_posix(), "source_demand_sha256": sha(table),
            "private_demand": destination.relative_to(case).as_posix(), "private_demand_sha256": sha(destination),
            "source_unit": "tonnes", "private_unit": "kg", "scale": 1000,
            "upstream_divisor": 1000, "headers_preserved": header,
            "year_columns": {str(i): col for i, col in enumerate(header[1:], 1)},
            **demand_summary,
            "origin_ids_outside_friction_domain": sorted(int(x) for x in o[om & ~fm]),
            "origin_count_outside_friction_domain": int((om & ~fm).sum()),
            "origin_count_inside_friction_domain": int((om & fm).sum()),
            "outside_origin_policy": "Retain every original origin and demand; unchanged C++ starts at zero and can enter adjacent positive-friction cells while leaving negative-friction output cells NoData",
            "friction": copy_map(friction, private / f"fricc_{channel}.tif", f, fm, fi),
            "origins": copy_map(origins, private / f"locs_{channel}.tif", o, om, oi),
        }
    private_hashes = {p.relative_to(case).as_posix(): sha(p) for p in private.iterdir() if p.is_file()}
    manifest = {"schema": SCHEMA, "created_utc": now(), "case": str(case),
                "engine": str(engine), "engine_sha256": args.engine_sha256,
                "source_commit": SOURCE_COMMIT, "source_variant": "OMP_specificYear",
                "years": common_years, "max_cells": args.max_cells,
                "scenario": {"relative": 1, "hours": args.hours, "exponent": args.exponent},
                "cost_grid_convention": "Unchanged upstream C++: float32 pixel width for all orthogonal costs, sqrt(2) times width for diagonals; original rectangular grid retained without resampling",
                "scenario_scope": "Declared validation fixture, not recovered server-wrapper settings",
                "original_inputs_sha256": all_inputs, "private_inputs_sha256": private_hashes,
                "channels": channels, "original_model_inputs_modified": False}
    verify_inputs(case, manifest)
    emit(audit / "fixture_manifest.json", manifest)
    print(json.dumps({"staged": str(audit), "periods": len(common_years), "executed": False,
                      "scenario": manifest["scenario"]}, indent=2))


def verify_inputs(case: Path, manifest: dict) -> None:
    require(sha(Path(manifest["engine"])) == manifest["engine_sha256"], "Executable changed")
    for section in ("original_inputs_sha256", "private_inputs_sha256"):
        for relative, expected in manifest[section].items():
            path = inside(case, relative)
            require(path.is_file() and sha(path) == expected, f"Fixture input changed: {relative}")


def output_check(case: Path, path: Path, channel: str, period: int, manifest: dict) -> dict:
    data, valid, info = load_map(path, manifest["max_cells"])
    f, fm, fi = load_map(case / f"_cpp_idw/inputs/fricc_{channel}.tif", manifest["max_cells"])
    o, om, _ = load_map(case / f"_cpp_idw/inputs/locs_{channel}.tif", manifest["max_cells"])
    require(same_geometry(info, fi), "Output geometry/CRS changed")
    require(np.array_equal(valid, fm), "Output valid/NoData mask differs from friction domain")
    require((data[valid] >= 0).all(), "Negative IDW pressure in valid domain")
    active = manifest["channels"][channel]["active_origin_ids_by_period"][str(period)]
    active_source_mask = om & np.isin(o, active)
    source_mask = active_source_mask & fm
    outside_source_mask = active_source_mask & ~fm
    require((data[source_mask] > 0).all(),
            "Expected positive finite source-cell pressure is absent")
    require(not active or (data[valid] > 0).any(), "Active demand produced no positive pressure")
    return {"sha256": sha(path), "metadata": info, "finite_valid_cells": int(valid.sum()),
            "positive_cells": int((data[valid] > 0).sum()), "zero_cells": int((data[valid] == 0).sum()),
            "minimum": float(data[valid].min()), "maximum": float(data[valid].max()),
            "active_origins": len(active), "positive_active_origins": int((data[source_mask] > 0).sum()),
            "active_origins_inside_friction_domain": int(source_mask.sum()),
            "active_origins_outside_friction_domain": int(outside_source_mask.sum()),
            "active_outside_origins_retained_as_output_nodata": int((outside_source_mask & ~valid).sum()),
            "valid_inf_count": int(np.isinf(data[valid]).sum()), "valid_nan_count": int(np.isnan(data[valid]).sum())}


def verified_result(case: Path, period: int, manifest: dict) -> dict:
    directory = case / f"_cpp_idw/runs/period_{period:02d}"
    result = read(directory / "result.json")
    require(result.get("success") and result["engine_sha256"] == manifest["engine_sha256"], "Period was not successfully verified")
    for channel in ("w", "v"):
        name = f"IDW_C++_fw_{channel}{period:02d}.tif"
        expected = result["outputs"][channel]["sha256"]
        require(sha(directory / name) == expected and sha(inside(case, "In/" + name)) == expected,
                "Completed or installed period output changed")
    return result


def run(args: argparse.Namespace) -> None:
    case = case_root(args.case)
    audit = case / "_cpp_idw"
    manifest = read(audit / "fixture_manifest.json")
    require(manifest["schema"] == SCHEMA, "Incorrect fixture manifest")
    require(1 <= args.start <= args.end <= len(manifest["years"]), "Invalid annual index range")
    require(1 <= args.processors <= 2, "Fixture run is bounded to one or two workers")
    verify_inputs(case, manifest)
    if args.end > 1:
        review = read(audit / "first_period_review.json")
        require(review["first_result_sha256"] == sha(audit / "runs/period_01/result.json"), "First-period review is stale")
        verified_result(case, 1, manifest)
    for period in range(args.start, args.end + 1):
        verify_inputs(case, manifest)
        directory = inside(case, f"_cpp_idw/runs/period_{period:02d}")
        if directory.exists():
            verified_result(case, period, manifest)
            print(json.dumps({"skipped_verified_period": period}), flush=True)
            continue
        for channel in ("w", "v"):
            require(not inside(case, f"In/IDW_C++_fw_{channel}{period:02d}.tif").exists(), "Unverified output already exists")
        directory.mkdir(parents=True)
        private_temp = directory / "engine_temp"
        private_temp.mkdir()
        command = [manifest["engine"]]
        for index, channel in ((1, "w"), (4, "v")):
            command += [f"-{index}", str(audit / f"inputs/fricc_{channel}.tif"),
                        f"-{index+1}", str(audit / f"inputs/locs_{channel}.tif"),
                        f"-{index+2}", str(audit / f"inputs/demand_{channel}_kg.csv")]
        scenario = manifest["scenario"]
        command += ["-r", "1", "-p", str(args.processors), "-t", str(scenario["hours"]),
                    "-e", str(scenario["exponent"]), "-y", str(period)]
        env = os.environ.copy()
        env.update(TEMP=str(private_temp), TMP=str(private_temp), TMPDIR=str(private_temp), OMP_NUM_THREADS=str(args.processors),
                   OPENBLAS_NUM_THREADS="1", GDAL_DATA="C:/rtools45/x86_64-w64-mingw32.static.posix/share/gdal",
                   PROJ_DATA="C:/rtools45/x86_64-w64-mingw32.static.posix/share/proj")
        record = {"period": period, "calendar_year": manifest["years"][period-1], "started_utc": now(),
                  "engine_sha256": manifest["engine_sha256"], "source_commit": manifest["source_commit"],
                  "fixture_manifest_sha256": sha(audit / "fixture_manifest.json"),
                  "original_inputs_sha256": manifest["original_inputs_sha256"],
                  "private_inputs_sha256": manifest["private_inputs_sha256"],
                  "command": command, "processors": args.processors, "load_note": args.load_note}
        emit(directory / "started.json", record)
        start = time.perf_counter()
        with (directory / "runtime.log").open("wb") as log:
            process = subprocess.Popen(command, cwd=directory, env=env, stdout=log, stderr=subprocess.STDOUT)
            record["pid"] = process.pid
            emit(directory / "started.json", record)
            returncode = process.wait()
        record.update(returncode=returncode, elapsed_seconds=time.perf_counter()-start, ended_utc=now(), success=False)
        try:
            require(returncode == 0, "C++ process returned a failure code")
            log = (directory / "runtime.log").read_text(errors="replace")
            require("walking scenario sucessfully finished" in log and "vehicle scenario sucessfully finished" in log,
                    "Both scenario completion messages are required, not exit code alone")
            require(not re.search(r"(?im)^\s*(?:ERROR\s*[0-9]*:|error:|terminate called)", log), "C++/GDAL error in log")
            outputs = {channel: output_check(case, directory / f"IDW_C++_fw_{channel}{period:02d}.tif", channel, period, manifest)
                       for channel in ("w", "v")}
            verify_inputs(case, manifest)
            for channel in ("w", "v"):
                name = f"IDW_C++_fw_{channel}{period:02d}.tif"
                destination = inside(case, "In/" + name)
                with destination.open("xb") as stream, (directory / name).open("rb") as incoming:
                    shutil.copyfileobj(incoming, stream)
                require(sha(destination) == outputs[channel]["sha256"], "Installed raster hash differs")
            record.update(success=True, outputs=outputs)
        except Exception as error:
            record["validation_error"] = str(error)
            emit(directory / "result.json", record)
            raise
        emit(directory / "result.json", record)
        print(json.dumps({"period": period, "success": True, "elapsed_seconds": record["elapsed_seconds"],
                          "outputs": outputs}, indent=2), flush=True)


def review(args: argparse.Namespace) -> None:
    case = case_root(args.case)
    audit = case / "_cpp_idw"
    manifest = read(audit / "fixture_manifest.json")
    verify_inputs(case, manifest)
    result = verified_result(case, 1, manifest)
    require(bool(args.note.strip()), "Record the actual first-period review")
    path = audit / "first_period_review.json"
    require(not path.exists(), "First-period review already exists")
    emit(path, {"reviewed_utc": now(), "note": args.note,
                "first_result_sha256": sha(audit / "runs/period_01/result.json"),
                "outputs": result["outputs"], "permits": "Sequential continuation within the staged 1..51-period range"})
    print(json.dumps({"review_recorded": str(path)}, indent=2))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--case", type=Path, required=True)
    commands = parser.add_subparsers(dest="operation", required=True)
    item = commands.add_parser("stage")
    item.add_argument("--engine", type=Path, default=DEFAULT_ENGINE)
    item.add_argument("--engine-sha256", default=ENGINE_SHA)
    item.add_argument("--demand-w", help="Case-relative full scenario CSV; defaults to the unique *_fwch_w.csv")
    item.add_argument("--demand-v", help="Case-relative full scenario CSV; defaults to the unique *_fwch_v.csv")
    item.add_argument("--max-cells", type=int, default=10000)
    item.add_argument("--hours", type=int, default=12)
    item.add_argument("--exponent", type=float, default=1.0)
    item.set_defaults(function=stage)
    item = commands.add_parser("run")
    item.add_argument("--start", type=int, default=1)
    item.add_argument("--end", type=int, default=1)
    item.add_argument("--processors", type=int, default=2)
    item.add_argument("--load-note", default="Competing legacy jobs observed; check current host load")
    item.set_defaults(function=run)
    item = commands.add_parser("review")
    item.add_argument("--note", required=True)
    item.set_defaults(function=review)
    args = parser.parse_args()
    args.function(args)


if __name__ == "__main__":
    main()
