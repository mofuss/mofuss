"""Run exact production NRB nodes in a small native multi-year Dinamica graph.

Writes only to a new, explicitly supplied MoFuSS_Active scratch directory.
The fixture uses the real MuxMap feedback and float32 production raster type.
"""
from __future__ import annotations

import argparse
import copy
import csv
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import xml.etree.ElementTree as E

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
from build_dinamica_sourcing_v12 import calculate, filename, node, port
from dinamica_v12_transform import _producers
from fix_woodman_nrb_attribution import LEDGER_IDS, LEDGER_NULL


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--engine", type=Path, default=Path(r"C:\Program Files\Dinamica EGO\DinamicaConsole.exe"))
    args = parser.parse_args()
    scratch = args.scratch.resolve()
    if "mofuss_active" not in [p.lower() for p in scratch.parts] or scratch.name.lower() == "mofuss_active":
        raise ValueError("Use a named scratch subfolder below MoFuSS_Active")
    if scratch.exists() and any(scratch.iterdir()):
        raise FileExistsError("Refusing to overwrite runtime evidence")
    scratch.mkdir(parents=True, exist_ok=True)
    (scratch / "inputs").mkdir()
    (scratch / "debugging_1").mkdir()
    temporary = scratch / "native_temp"
    temporary.mkdir()
    import numpy as np
    import rasterio
    from rasterio.transform import from_origin

    # S, B, H, P, E, annual legacy deforestation gate, current TOF flag.
    cases = [
        ("ordinary_growth_harvest", 100, [(100, 110, 20, 90, 90, 0, 0), (90, 95, 5, 90, 90, 0, 0), (90, 110, 5, 105, 105, 0, 0)]),
        ("downward_clamp_no_harvest", 100, [(100, 60, 0, 60, 60, 0, 0), (60, 50, 0, 50, 50, 0, 0), (50, 40, 0, 40, 40, 0, 0)]),
        ("downward_clamp_with_harvest", 100, [(100, 60, 10, 50, 50, 0, 0), (50, 25, 5, 20, 20, 0, 0), (20, 20, 0, 20, 20, 0, 0)]),
        ("clear_after_harvest_then_regrow", 100, [(100, 100, 20, 80, 80, 0, 0), (0, 0, 0, 0, 0, 0, 0), (0, 10, 0, 10, 10, 0, 0)]),
        ("tof_allowance_changes", 100, [(100, 100, 50, 100, 100, 0, 1), (10, 10, 5, 10, 10, 0, 1), (200, 200, 20, 200, 200, 0, 1)]),
        ("zero_harvest_domain_gap_then_growth", 100, [(100, 100, 20, 80, 80, 0, 0), (None, None, 0, None, None, 0, None), (0, 5, 0, 5, 5, 0, 0)]),
        ("terminal_domain_gap_preserves_depletion", 100, [(100, 100, 20, 80, 80, 0, 0), (None, None, 0, None, None, None, None), (None, None, 0, None, None, None, None)]),
        ("initial_no_data_stays_no_data", None, [(100, 100, 10, 90, 90, 0, 0)] * 3),
        ("invalid_harvest_missing_state_is_visible", 100, [(100, 100, 20, 80, 80, 0, 0), (None, None, 5, None, None, 0, 0), (0, 10, 0, 10, 10, 0, 0)]),
        ("endpoint_seed_offsets", 2, [(2, 2, 2, 0, 2, 0, 0)] * 3),
        ("legacy_deforestation_exclusion", 100, [(100, 100, 20, 80, 80, 1, 0), (80, 80, 10, 70, 70, 0, 0), (70, 70, 10, 60, 60, 0, 0)]),
        ("regrowth_credit_survives_luc_reset", 100, [(100, 150, 0, 150, 150, 0, 0), (0, 0, 0, 0, 0, 0, 0), (100, 100, 40, 60, 60, 0, 0)]),
        ("fractional_float32_balance", 20.7, [(20.7, 22.4, 3.9, 18.5, 18.5, 0, 0), (18.5, 19.67, 2.76, 16.91, 16.91, 0, 0), (16.91, 17.66, 1.95, 15.71, 15.71, 0, 0)]),
        ("missing_endpoint_zero_harvest_carries", 100, [(100, 100, 20, 80, 80, 0, 0), (80, 90, 0, 90, None, 0, 0), (0, 5, 0, 5, 5, 0, 0)]),
        ("legacy_deforestation_gate_survives_gap", 100, [(100, 100, 20, 80, 80, 1, 0), (None, None, 0, None, None, None, None), (0, 5, 0, 5, 5, 0, 0)]),
        ("numeric_zero_initial_is_supported", 0, [(0, 5, 0, 5, 5, 0, 0), (5, 5, 2, 3, 3, 0, 0), (3, 3, 1, 2, 2, 0, 0)]),
        ("missing_start_does_not_credit_seed", 100, [(100, 100, 20, 80, 80, 0, 0), (None, 0, 0, 0, 2, None, 0), (2, 3, 0, 3, 3, 0, 0)]),
        ("signed_balance_crosses_exact_legacy_nodata", 100, [(100, 10099, 0, 10099, 10099, 0, 0), (10099, 10119, 5, 10114, 10114, 0, 0), (10114, 10134, 5, 10129, 10129, 0, 0)]),
        ("signed_balance_near_legacy_nodata_below", 100, [(100, 10099.00390625, 0, 10099.00390625, 10099.00390625, 0, 0), (10099.00390625, 10119, 5, 10114, 10114, 0, 0), (10114, 10134, 5, 10129, 10129, 0, 0)]),
        ("signed_balance_near_legacy_nodata_above", 100, [(100, 10098.9970703125, 0, 10098.9970703125, 10098.9970703125, 0, 0), (10098.9970703125, 10119, 5, 10114, 10114, 0, 0), (10114, 10134, 5, 10129, 10129, 0, 0)]),
        ("end_balance_seed_crosses_exact_legacy_nodata", 2, [(2, 9999, 0, 9999, 9999, 0, 0), (0, 0, 0, 0, 2, 0, 0), (2, 7, 0, 7, 7, 0, 0)]),
    ]
    values = np.array([[[(np.nan if value is None else value) for value in step]
                        for step in steps] for _, _, steps in cases], dtype=np.float32)
    initial = np.array([np.nan if value is None else value for _, value, _ in cases], dtype=np.float32)

    def write_map(path, array):
        with rasterio.open(path, "w", driver="GTiff", width=len(array), height=1, count=1,
                           dtype="float32", nodata=-9999, crs="EPSG:4326",
                           transform=from_origin(0, 1, 1, 1)) as target:
            target.write(np.where(np.isnan(array), -9999, array).reshape(1, -1), 1)

    def read_map(path):
        with rasterio.open(path) as source:
            return source.read(1, masked=True).filled(np.nan).reshape(-1)

    write_map(scratch / "inputs" / "initial.tif", initial)
    columns = ("S", "B", "H", "P", "E", "gate", "tof")
    for year in range(3):
        for column, label in enumerate(columns):
            write_map(scratch / "inputs" / f"{label}{year + 1}.tif", values[:, year, column])

    model_path = Path(__file__).resolve().parents[1] / "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml"
    model_bytes = model_path.read_bytes()
    (scratch / "source_v14.egoml").write_bytes(model_bytes)
    production = E.fromstring(model_bytes)
    producers = _producers(production)
    fixture = E.Element("script")
    E.SubElement(fixture, "property", key="dff.version", value="2.4.1.20140602")

    def load(parent, output, path=None, peer=None):
        entry = node(parent, "LoadMap", "Fixture input " + output)
        port(entry, "filename", '"' + path + '"' if path else None, peer=peer)
        for key, value in (("nullValue", ".none"), ("loadAsSparse", ".no"),
                           ("suffixDigits", "0"), ("step", ".none"), ("workdir", ".none")):
            port(entry, key, value)
        E.SubElement(entry, "outputport", name="map", id=output)

    def save(parent, source, stem, annual=False):
        entry = node(parent, "SaveMap", "Fixture output " + stem)
        port(entry, "map", peer=source)
        port(entry, "filename", '"' + stem + '.tif"')
        port(entry, "suffixDigits", "2" if annual else "0")
        port(entry, "step", None if annual else ".none", peer="v39" if annual else None)
        port(entry, "useCompression", ".yes")
        port(entry, "workdir", ".none")

    load(fixture, "v200", path="inputs/initial.tif")
    fixture.append(copy.deepcopy(producers["v93000"]))
    calculate(fixture, "Value", "One MC", "1", "v38")
    annual = node(fixture, "Repeat", "Three production balance steps", container=True)
    port(annual, "iterations", "3")
    E.SubElement(annual, "internaloutputport", name="step", id="v39")
    for index, (label, output) in enumerate(zip(columns, ("v90008", "v171", "v130", "v131", "v98", "v112", "v90005"))):
        file_id = f"v960{index}"
        filename(annual, f"inputs/{label}<v1>.tif", ("v39",), file_id)
        load(annual, output, peer=file_id)
    for ident in ("v93001", "v93002", "v93003", "v97", "v96", "v93004", "v57", "v107", "v77", "v117"):
        cloned = copy.deepcopy(producers[ident])
        for entry in cloned.iter("inputport"):
            if entry.get("peerid") == "v90016":
                entry.set("peerid", "v93000")
        annual.append(cloned)
    # Reuse the exact production balance writer, including the annual filename.
    writer = next(n for n in production.iter("functor") if n.get("name") == "SaveMap"
                  and n.find("inputport[@name='filename']") is not None
                  and n.find("inputport[@name='filename']").get("peerid") == "v93004")
    annual.append(copy.deepcopy(writer))
    for source, stem in (("v97", "annual_nrb"), ("v96", "annual_fnrb"), ("v93003", "balance_end")):
        save(annual, source, stem, annual=True)
    # Explicit physical-state outputs demonstrate that a ledger-only NoData
    # change cannot modify these inputs in the native observer graph.
    physical = {"S": "v90008", "B": "v171", "H": "v130", "P": "v131", "E": "v98"}
    for label, source in physical.items():
        save(annual, source, "physical_" + label, annual=True)
    for ident in ("v193", "v194"):
        fixture.append(copy.deepcopy(producers[ident]))
    save(fixture, "v193", "terminal_nrb")
    save(fixture, "v194", "terminal_fnrb")
    E.indent(fixture, space="    ")
    fixture_path = scratch / "fixture.egoml"
    E.ElementTree(fixture).write(fixture_path, encoding="utf-8", xml_declaration=True)
    environment = os.environ.copy()
    environment.update(TEMP=str(temporary), TMP=str(temporary))
    result = subprocess.run([str(args.engine), "-processors", "1", "-predefined-seed",
                             "-log-level", "3", str(fixture_path)], cwd=scratch,
                            env=environment, text=True, capture_output=True, timeout=180)
    (scratch / "engine.log").write_text(result.stdout + result.stderr, encoding="utf-8")
    if result.returncode:
        raise RuntimeError(result.stdout + result.stderr)

    # Run the same observer with its historical sentinel to establish that
    # the new crossing cases exercise the actual defect, not just a formula.
    legacy_dir = scratch / "legacy_sentinel"
    legacy_dir.mkdir()
    shutil.copytree(scratch / "inputs", legacy_dir / "inputs")
    (legacy_dir / "debugging_1").mkdir()
    legacy = copy.deepcopy(fixture)
    legacy_producers = _producers(legacy)
    for ident in LEDGER_IDS:
        legacy_producers[ident].find("inputport[@name='nullValue']").text = ".default"
    legacy_path = legacy_dir / "fixture.egoml"
    E.ElementTree(legacy).write(legacy_path, encoding="utf-8", xml_declaration=True)
    old_result = subprocess.run([str(args.engine), "-processors", "1", "-predefined-seed",
                                 "-log-level", "3", str(legacy_path)], cwd=legacy_dir,
                                env=environment, text=True, capture_output=True, timeout=180)
    (legacy_dir / "engine.log").write_text(old_result.stdout + old_result.stderr, encoding="utf-8")
    if old_result.returncode:
        raise RuntimeError(old_result.stdout + old_result.stderr)
    legacy_collisions = []
    for year in range(1, 4):
        for label in physical:
            relative = f"physical_{label}{year:02d}.tif"
            np.testing.assert_equal(read_map(scratch / relative), values[:, year - 1, columns.index(label)])
            if (scratch / relative).read_bytes() != (legacy_dir / relative).read_bytes():
                raise AssertionError("Physical raster changed after sentinel migration: " + relative)
        for relative in (f"debugging_1/Woodfuel_balance{year:02d}.tif", f"balance_end{year:02d}.tif"):
            with rasterio.open(scratch / relative) as dataset:
                np.testing.assert_equal(dataset.nodata, float(np.float32(float(LEDGER_NULL))))
            safe_values = read_map(scratch / relative)
            old_values = read_map(legacy_dir / relative)
            for index, (label, _, _) in enumerate(cases):
                if np.isfinite(safe_values[index]) and not np.isfinite(old_values[index]):
                    legacy_collisions.append({"case": label, "year": year, "output": relative})
    expected_collisions = {name for name, _, _ in cases if "legacy_nodata" in name}
    if {entry["case"] for entry in legacy_collisions} != expected_collisions:
        raise AssertionError("Native legacy control did not expose all sentinel collision cases")

    expected = {label: [] for label in ("annual_nrb", "annual_fnrb", "post", "balance_end")}
    cprev = np.where(np.isnan(initial), np.nan, 0.0)
    cumulative_harvest = cprev.copy()
    cumulative_gate = cprev.copy()
    records = []
    for year in range(3):
        nrbs, fractions, posts, ends = [], [], [], []
        for index, (label, _, _) in enumerate(cases):
            start, before, harvest, post, end, gate, tof = map(float, values[index, year])
            support = not np.isnan(initial[index])
            missing_state = any(np.isnan(v) for v in (start, before, post, end))
            nrb = (np.nan if not support or np.isnan(harvest) else 0 if harvest <= 0 else
                   np.nan if any(np.isnan(v) for v in (start, before, post)) else
                   0 if tof == 1 or gate != 0 else max(0, min(harvest, min(start, before) - post)))
            # Exact production storage boundaries, with double intermediate
            # arithmetic as in CalculateMap and float32 output rounding.
            cp = (np.nan if not support or np.isnan(cprev[index]) or np.isnan(harvest) else
                  cprev[index] if missing_state and harvest <= 0 else np.nan if missing_state else
                  cprev[index] + min(start, before) - post)
            cp = float(np.float32(cp))
            ce = (np.nan if np.isnan(cp) else cprev[index] if missing_state and harvest <= 0 else
                  np.nan if missing_state else cp - (end - post))
            ce = float(np.float32(ce))
            nrb = float(np.float32(nrb))
            fraction = float(np.float32(np.nan if not support else 0 if harvest <= 0 else nrb / harvest))
            nrbs.append(nrb); fractions.append(fraction); posts.append(cp); ends.append(ce)
        for name, items in (("annual_nrb", nrbs), ("annual_fnrb", fractions), ("post", posts), ("balance_end", ends)):
            expected[name].append(np.array(items))
        cprev = np.array(ends)
        cumulative_harvest = (cumulative_harvest + values[:, year, 2]).astype(np.float32)
        cumulative_gate = (cumulative_gate + np.nan_to_num(values[:, year, 5], nan=0)).astype(np.float32)
        for name in expected:
            path = (scratch / "debugging_1" / f"Woodfuel_balance{year + 1:02d}.tif" if name == "post" else
                    scratch / f"{name}{year + 1:02d}.tif")
            observed = read_map(path)
            want = expected[name][-1]
            np.testing.assert_allclose(observed, want, rtol=2e-6, atol=2e-6, equal_nan=True,
                                       err_msg=f"{name} year {year + 1}")
            for index, (label, _, _) in enumerate(cases):
                records.append({"case": label, "year": year + 1, "output": name,
                                "expected": want[index], "observed": observed[index]})
    terminal_nrb = np.where(np.isnan(initial), np.nan,
                           np.where(cumulative_harvest <= 0, 0,
                                    np.where(cumulative_gate > 0, 0,
                                             np.maximum(0, np.minimum(cumulative_harvest, cprev))))).astype(np.float32)
    with np.errstate(divide="ignore", invalid="ignore"):
        terminal_fnrb = np.where(np.isnan(initial), np.nan,
                                 np.where(cumulative_harvest <= 0, 0, terminal_nrb / cumulative_harvest)).astype(np.float32)
    for name, want in (("terminal_nrb", terminal_nrb), ("terminal_fnrb", terminal_fnrb)):
        observed = read_map(scratch / f"{name}.tif")
        np.testing.assert_allclose(observed, want, rtol=2e-6, atol=2e-6, equal_nan=True, err_msg=name)
        for index, (label, _, _) in enumerate(cases):
            records.append({"case": label, "year": 3, "output": name,
                            "expected": want[index], "observed": observed[index]})
    with (scratch / "checks.csv").open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(records[0]))
        writer.writeheader(); writer.writerows(records)
    error = max(abs(float(record["expected"]) - float(record["observed"]))
                for record in records if np.isfinite(record["expected"]))
    report = {"passed": True, "cases": len(cases), "years": 3, "checks": len(records),
              "maximum_absolute_error": error, "source_sha256": hashlib.sha256(model_bytes).hexdigest(),
              "case_names": [case[0] for case in cases], "engine": str(args.engine),
              "balance_null_value": LEDGER_NULL,
              "native_legacy_collision_cases": sorted(expected_collisions),
              "legacy_collision_observations": legacy_collisions,
              "physical_rasters_byte_identical_to_legacy": len(physical) * 3}
    (scratch / "report.json").write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
