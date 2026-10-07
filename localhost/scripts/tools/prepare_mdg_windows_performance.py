"""Prepare the eight existing MDG Windows runs without running simulations.

Default: inspect and print the proposed code/configuration changes. --install
requires the completed legacy exact-output regression suite. EGO 8.13 also
requires its five-case engine evidence and a passing regional raster/scalar
comparison before installation. Only model/support code,
a small code backup and a preparation manifest are written in each run folder.
Scientific inputs, Monte Carlo batches and existing results are never changed.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import json
from pathlib import Path
import shutil
import xml.etree.ElementTree as ET

from windows_report_option import assert_presentation_only
from windows_engine_compatibility import explicit_annual_filename_steps

SCRIPTS = Path(__file__).resolve().parents[1]
SUPPORT = ("rnorm_v8.R", "bypassMC_v8.R")
# These helpers are outside the frozen engine regression. They were separately
# reviewed against all four F copies: LUC1 draw order/distributions are unchanged;
# Sourcing directories and deterministic table exports are added. Reject later
# edits until that review has been repeated.
REVIEWED_SUPPORT_SHA256 = {
    "rnorm_v8.R": "fa47d442f430f79843308452ae418236b4b1bc13475c118bed1078db244a9d59",
    "bypassMC_v8.R": "de0af2fa8f7b4137217b9ea76159cf4a4035b3b81d1c6f686e8c43a078e08128",
}
EXPECTED_CASES = {"v13_fixed_capped", "v13_fixed_uncapped", "v14_dynamic_capped",
                  "v14_dynamic_uncapped", "v14_dynamic_capped_patcher"}
ENGINES = {
    "legacy": Path("C:/Program Files/Dinamica EGO/DinamicaConsole.exe"),
    "8.13": Path("C:/Users/UNAM/AppData/Local/Programs/DinamicaEGO-8.13/DinamicaConsole8.exe"),
}
ENGINE8_FLAGS = ("-disable-parallel-steps", "-disable-parallel-functors")


def digest(data):
    return hashlib.sha256(data).hexdigest()


def rows(path):
    with path.open(encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def constant(root, identifier):
    node = next(n for n in root.iter() if any(p.get("id") == identifier for p in n.findall("outputport")))
    return node.find("inputport[@name='constant']")


def validate_release_gate(verification, tested_models, engine):
    gate = json.loads(verification.read_text())
    comparisons = gate.get("comparisons", [])
    if (not gate.get("all_selected_checks_passed")
            or set(gate.get("cases", [])) != EXPECTED_CASES
            or len(gate.get("cases", [])) != len(EXPECTED_CASES)
            or len(comparisons) != len(EXPECTED_CASES)
            or {c.get("case") for c in comparisons} != EXPECTED_CASES
            or not all(c.get("retained_scientific_content_identical") for c in comparisons)):
        raise ValueError("Five-case exact scientific-output release gate not satisfied")
    for case in comparisons:
        folder = verification.parent / case["candidate"]
        manifest = json.loads((folder / "performance_manifest.json").read_text())
        version = 13 if case["case"].startswith("v13_") else 14
        source = tested_models / f"10_dyn_Sc17_webmofuss_ctrees_g_v{version}.egoml"
        if (manifest.get("case") != case["case"] or manifest.get("version") != version
                or manifest.get("source_model_sha256") != digest(source.read_bytes())):
            raise ValueError(f"Tested model does not match successful evidence: {source}")
        if Path(case["candidate_command"][0]).resolve() != engine.resolve():
            raise ValueError("Selected engine does not match the exact-output release gate")
    return digest(verification.read_bytes())


def validate_engine_gate(verification, tested_models, engine):
    """Bind EGO 8 parity evidence to the two reviewed filename-carrier edits.

    Report rounding may pass the dedicated comparison tool, but raster and
    decoded scalar equality remain mandatory. This five-case gate does not
    assert regional performance or replace any separate benchmark decision.
    """
    gate = json.loads(verification.read_text())
    comparisons = gate.get("comparisons", [])
    required = ("engine_core_parity", "frozen_inputs_identical",
                "all_tiff_values_masks_geometry_type_exact",
                "decoded_binary64_all_exact", "csv_comparison_passed")
    if (gate.get("engine_core_parity") is not True
            or set(gate.get("cases", [])) != EXPECTED_CASES
            or len(gate.get("cases", [])) != len(EXPECTED_CASES)
            or len(comparisons) != len(EXPECTED_CASES)
            or {c.get("case") for c in comparisons} != EXPECTED_CASES
            or not all(all(c.get(key) is True for key in required) for c in comparisons)):
        raise ValueError("Five-case EGO 8 exact raster/scalar and reviewed CSV gate not satisfied")
    for case in comparisons:
        expected_tiffs = 239 if case["case"].endswith("_uncapped") else 230
        if (case.get("missing_outputs") != [] or case.get("additional_outputs") != []
                or case.get("tiff_count") != expected_tiffs or case.get("csv_count") != 63
                or case.get("decoded_binary64_count") != 252
                or case.get("csv_numeric_changed_fields") != case.get("csv_tolerated_changed_fields")):
            raise ValueError(f"Incomplete or unapproved EGO 8 output comparison: {case['case']}")
        folder = verification.parent / case["candidate"]
        manifest = json.loads((folder / "performance_manifest.json").read_text())
        runtime = json.loads((folder / "runtime_result.json").read_text())
        version = 13 if case["case"].startswith("v13_") else 14
        source = Path(manifest["source_model"])
        model_name = f"10_dyn_Sc17_webmofuss_ctrees_g_v{version}.egoml"
        if (manifest.get("case") != case["case"] or manifest.get("version") != version
                or manifest.get("source_model_sha256") != digest(source.read_bytes())
                or manifest.get("fixture_model_sha256") != digest((folder / "regression_model.egoml").read_bytes())):
            raise ValueError(f"EGO 8 tested model bytes changed or mismatch the evidence: {source}")
        expected = explicit_annual_filename_steps((tested_models / model_name).read_text(encoding="utf-8"))
        assert_presentation_only(expected, source.read_text(encoding="utf-8"))
        command = case.get("candidate_command", [])
        if (not command or Path(command[0]).resolve() != engine.resolve()
                or command != runtime.get("command") or runtime.get("returncode") != 0
                or runtime.get("extra_engine_flags") != list(ENGINE8_FLAGS)
                or any(command.count(flag) != 1 for flag in ENGINE8_FLAGS)
                or "-disable-native-expressions" in command
                or Path(command[-1]).resolve() != (folder / "regression_model.egoml").resolve()):
            raise ValueError(f"EGO 8 execution options mismatch the verified serial configuration: {case['case']}")
        # Bind the compared old side to the same reviewed legacy source too.
        baseline = verification.parent / case["baseline"]
        old_manifest = json.loads((baseline / "performance_manifest.json").read_text())
        if (old_manifest.get("case") != case["case"]
                or old_manifest.get("source_model_sha256") != digest((tested_models / model_name).read_bytes())
                or Path(case["baseline_command"][0]).resolve() != ENGINES["legacy"].resolve()):
            raise ValueError(f"EGO 8 comparison used a different legacy baseline: {case['case']}")
    return digest(verification.read_bytes())


def validate_regional_gate(verification, tested_models, engine):
    """Never let a small-fixture pass waive a failed regional scalar check."""
    gate = json.loads(verification.read_text())
    required = ("engine_core_parity", "frozen_inputs_identical",
                "all_tiff_values_masks_geometry_type_exact",
                "decoded_binary64_all_exact", "csv_comparison_passed")
    if (not all(gate.get(key) is True for key in required)
            or gate.get("missing_outputs") != [] or gate.get("additional_outputs") != []):
        raise ValueError("Regional EGO 8 exact raster/scalar and reviewed CSV gate not satisfied")
    command = gate.get("candidate_command", [])
    if (not command or Path(command[0]).resolve() != engine.resolve()
            or any(command.count(flag) != 1 for flag in ENGINE8_FLAGS)):
        raise ValueError("Regional EGO 8 engine/serial flags differ from deployment")
    for side in ("baseline", "candidate"):
        folder = verification.parent / gate[side]
        manifest_path = folder / "fixture_manifest.json"
        # Existing regional evidence provides these byte hashes. Validate
        # source and staged model whenever that provenance is available.
        if manifest_path.exists():
            manifest = json.loads(manifest_path.read_text())
            if "model_source_sha256" in manifest:
                source = Path(manifest["model_source"])
                if digest(source.read_bytes()) != manifest["model_source_sha256"]:
                    raise ValueError(f"Regional source model changed after verification: {source}")
                expected = (tested_models / source.name).read_text(encoding="utf-8")
                if side == "candidate":
                    expected = explicit_annual_filename_steps(expected)
                assert_presentation_only(expected, source.read_text(encoding="utf-8"))
            if ("staged_model_sha256" in manifest and
                    digest((folder / "regression_model.egoml").read_bytes()) != manifest["staged_model_sha256"]):
                raise ValueError(f"Regional staged model changed after verification: {folder}")
    return digest(verification.read_bytes())


def console_launcher(engine, model_name, run, paired_bau, engine_flags=()):
    """Use the explicitly validated engine, without changing its RNG setting."""
    tag = run.drive.rstrip(":") + "_" + run.name
    notice = ("Start only after this BAU has generated its NEW Monte Carlo batch: "
              + str(paired_bau)) if paired_bau else "This BAU run generates a new Monte Carlo batch."
    options = " ".join(("-processors", "2", "-log-level", "4", *engine_flags))
    lines = ["@echo off", "setlocal EnableExtensions", "pushd \"%~dp0\"",
             "if errorlevel 1 exit /b 1", "echo " + notice,
             "echo Use at most four simultaneous models, each with two processors.",
             f'set "TEMP=E:\\MoFuSS_Active\\mdg_windows_reruns\\{tag}\\%RANDOM%_%RANDOM%"',
             'mkdir "%TEMP%"', "if errorlevel 1 goto temp_error",
             'set "TMP=%TEMP%"', 'set "TMPDIR=%TEMP%"',
             f'"{engine}" {options} "{model_name}"',
             'set "MOFUSS_EXIT=%ERRORLEVEL%"',
             "echo.", "echo Dinamica finished with exit code %MOFUSS_EXIT%.",
             "popd", "pause", "endlocal & exit /b %MOFUSS_EXIT%",
             ":temp_error", "echo Could not create the temporary folder on E:.",
             "popd", "pause", "endlocal & exit /b 1"]
    return ("\r\n".join(lines) + "\r\n").encode("utf-8")


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--models", type=Path, required=True)
    p.add_argument("--tested-models", type=Path, required=True)
    p.add_argument("--verification", type=Path, required=True)
    p.add_argument("--engine", choices=tuple(ENGINES), default="legacy")
    p.add_argument("--engine-verification", type=Path,
                   help="Required for 8.13: completed five-case engine comparison JSON")
    p.add_argument("--regional-verification", type=Path,
                   help="Required for 8.13 installation: passing full-regional exact raster/scalar comparison JSON")
    p.add_argument("--report", type=Path, required=True)
    p.add_argument("--install", action="store_true")
    a = p.parse_args()
    if a.engine == "8.13" and a.engine_verification is None:
        p.error("--engine-verification is required with --engine 8.13")
    if a.engine == "8.13" and a.install and a.regional_verification is None:
        p.error("--regional-verification is required to install --engine 8.13")
    if a.engine == "legacy" and (a.engine_verification is not None or a.regional_verification is not None):
        p.error("Engine/regional verification arguments apply only to --engine 8.13")
    release = "windows_performance_2026-10-06"
    engine = ENGINES[a.engine]
    engine_flags = list(ENGINE8_FLAGS) if a.engine == "8.13" else []
    if not engine.is_file():
        raise FileNotFoundError(engine)
    for filename in SUPPORT:
        if digest((SCRIPTS / filename).read_bytes()) != REVIEWED_SUPPORT_SHA256[filename]:
            raise ValueError(f"Support script changed since its compatibility review: {filename}")
    # Inspect mode verifies provenance too, so its reviewable plan records the
    # same evidence hashes that installation will require.
    gate_hash = validate_release_gate(a.verification, a.tested_models, ENGINES["legacy"])
    engine_gate_hash = (validate_engine_gate(a.engine_verification, a.tested_models, engine)
                        if a.engine == "8.13" else None)
    regional_gate_hash = (validate_regional_gate(a.regional_verification, a.tested_models, engine)
                          if a.regional_verification is not None else None)
    result = {"release": release, "created_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
              "engine": str(engine), "engine_choice": a.engine, "engine_flags": engine_flags,
              "installed": a.install, "verification_sha256": gate_hash,
              "engine_verification_sha256": engine_gate_hash,
              "regional_verification_sha256": regional_gate_hash, "runs": []}
    planned = []
    for drive, version, luc in (("F", 13, 1), ("D", 14, 3)):
        name = f"10_dyn_Sc17_webmofuss_ctrees_g_v{version}.egoml"
        source = (a.models / name).read_text(encoding="utf-8")
        # The final models may add the report editor, but their executable graph
        # must be precisely the one tested with frozen inputs.
        expected = (a.tested_models / name).read_text(encoding="utf-8")
        if a.engine == "8.13":
            expected = explicit_annual_filename_steps(expected)
        assert_presentation_only(expected, source)
        for mode, uncapped in (("capped", "0"), ("uncapped", "1")):
            for scenario in ("bau1", "ics3"):
                run = Path(f"{drive}:/MDG_1000m_{scenario}_2050_mc3_{mode}")
                if not run.is_dir():
                    raise FileNotFoundError(run)
                pars = {r["Var"]: r["ParCHR"] for r in rows(run / "LULCC/TempTables/parameters_dinamica.csv")}
                for key, expected in (("start_year", "2000"), ("end_year", "2050"),
                                      ("monte_carlo_runs", "3"), ("uncapped_regrowth", uncapped)):
                    if pars.get(key) != expected:
                        raise ValueError(f"Unexpected {key} in {run}: {pars.get(key)}")
                needed = [f"LULCC/TempTables/growth_parameters{luc}.csv",
                          f"LULCC/TempRaster/LULCt{luc}_c.tif", "LULCC/TempRaster/agb3_c.tif",
                          "In/DemandScenarios/W_origin_component_index.csv",
                          "In/DemandScenarios/V_origin_component_index.csv"]
                if luc == 3:
                    needed += [f"LULCC/TempRaster/{stem}{year}.tif" for year in range(2000, 2051)
                               for stem in ("LULCt3_c_", "TOFvsFOR_mask3_", "LULCt3_transition_")]
                missing = [x for x in needed if not (run / x).is_file()]
                if missing:
                    raise FileNotFoundError(f"Missing inputs in {run}: {missing}")
                model = ET.fromstring(source)
                if constant(model, "v302").text != str(luc):
                    raise ValueError("Unexpected model LUC default")
                rerun = scenario == "bau1"
                constant(model, "v256").text = ".yes" if rerun else ".no"
                payload = ET.tostring(model, encoding="utf-8", xml_declaration=True)
                files = {name: payload, **{f: (SCRIPTS / f).read_bytes() for f in SUPPORT}}
                partner = Path(f"{drive}:/MDG_1000m_bau1_2050_mc3_{mode}")
                files["RUN_MDG_optimized.cmd"] = console_launcher(
                    engine, name, run, None if rerun else partner, engine_flags)
                item = {"run": str(run), "model": name, "luc": luc, "mc_reruns": rerun,
                        "mc": 3, "years": [2000, 2050], "uncapped": int(uncapped),
                        "paired_bau": None if rerun else str(partner),
                        "engine": str(engine), "engine_choice": a.engine, "engine_flags": engine_flags,
                        "verification_sha256": gate_hash, "engine_verification_sha256": engine_gate_hash,
                        "regional_verification_sha256": regional_gate_hash,
                        "patcher_bypassed": constant(model, "v313").text == ".yes",
                        "render_reports": constant(model, "v257").text == ".yes",
                        "files": {f: {"new_sha256": digest(data),
                                       "previous_sha256": digest((run/f).read_bytes()) if (run/f).exists() else None}
                                  for f, data in files.items()}}
                if not item["patcher_bypassed"]:
                    raise ValueError("MDG preparation expects the agreed Patcher bypass")
                result["runs"].append(item)
                planned.append((run, files, item))
    # All eight preflights pass before the first runtime code file changes.
    if a.install:
        for run, files, _ in planned:
            for filename, data in files.items():
                target = run / filename
                old = run / "_code_backups" / release / filename
                if (target.exists() and target.read_bytes() != data
                        and old.exists() and old.read_bytes() != target.read_bytes()):
                    raise FileExistsError(f"Refusing to replace an earlier backup: {old}")
        for run, files, item in planned:
            backup = run / "_code_backups" / release
            for filename, data in files.items():
                target = run / filename
                if target.exists() and target.read_bytes() != data:
                    backup.mkdir(parents=True, exist_ok=True)
                    old = backup / filename
                    if old.exists() and old.read_bytes() != target.read_bytes():
                        raise FileExistsError(f"Refusing to replace an earlier backup: {old}")
                    if not old.exists():
                        shutil.copy2(target, old)
                target.write_bytes(data)
                if digest(target.read_bytes()) != item["files"][filename]["new_sha256"]:
                    raise IOError(f"Installed file hash mismatch: {target}")
            (run / "windows_performance_preparation.json").write_text(
                json.dumps({**item, "engine": str(engine), "release": release,
                            "verification_sha256": gate_hash,
                            "engine_verification_sha256": engine_gate_hash,
                            "regional_verification_sha256": regional_gate_hash}, indent=2) + "\n", encoding="utf-8")
    a.report.parent.mkdir(parents=True, exist_ok=True)
    a.report.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({"installed": a.install, "runs": len(planned), "report": str(a.report)}, indent=2))


if __name__ == "__main__":
    main()
