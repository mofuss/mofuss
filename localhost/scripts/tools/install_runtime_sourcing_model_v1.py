"""Install a new model beside v11, without changing any run inputs or results.

Default is read-only inspection. Explicit --install copies the new version and
small provenance snapshots; original v11 files remain in place. This tool does
not launch Dinamica, R, the IDW calculator, or any simulation.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import json
import shutil
from pathlib import Path


def sha(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def rows(path):
    with path.open(encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream))


def inspect_run(run, model, baseline):
    run = run.resolve(strict=True)
    if run == run.parent or not run.is_dir():
        raise ValueError("A specific existing run folder is required")
    if model.name not in ("10_dyn_Sc17_webmofuss_ctrees_g_v12.egoml", "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml"):
        raise ValueError("Expected a reviewed v12/v13 model basename")
    original = run / baseline.name
    if not original.is_file() or sha(original) != sha(baseline):
        raise ValueError(f"Original installed model differs from reviewed v11: {run}")
    target = run / model.name
    if target.exists() and sha(target) != sha(model):
        raise FileExistsError(f"Refusing to overwrite a different installed model: {target}")
    pars_path = run / "LULCC/TempTables/parameters_dinamica.csv"
    pars = {r["Var"]: r["ParCHR"] for r in rows(pars_path)}
    demand = run / "In/DemandScenarios"
    indices = {channel: rows(demand / f"{channel}_origin_component_index.csv") for channel in ("W", "V")}
    countries = [r["DemandISO3"] for r in indices["W"]]
    if len(set(countries)) != len(countries) or not countries:
        raise ValueError(f"W requires unique country origins: {run}")
    if set(countries) != {r["DemandISO3"] for r in indices["V"]}:
        raise ValueError(f"W/V origins differ: {run}")
    if model.name.endswith("v13.egoml"):
        for row in indices["W"]:
            if row["AllowedSourceISO3"].strip() != row["DemandISO3"].strip():
                raise ValueError("v13 requires strictly domestic W components; run preprocessing first: " + str(run))
    ready = demand / "HC_jobs/HC_IDW_install_manifest.csv"
    if len(countries) > 1 and not ready.is_file():
        raise ValueError("Regional IDW installation manifest is required: " + str(run))
    provenance = [pars_path, run / "LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv",
                  demand / "W_origin_component_index.csv", demand / "V_origin_component_index.csv"]
    for relative in ("LULCC/TempVector/userarea.gpkg", "LULCC/TempRaster/admin_c.tif"):
        path = run / relative
        if not path.is_file():
            raise FileNotFoundError("Country sourcing provenance input is missing: " + str(path))
        provenance.append(path)
    if ready.is_file():
        provenance.append(ready)
    direction = demand / "HC_jobs/country_direction_rules.csv"
    if direction.is_file():
        provenance.append(direction)
    provenance += sorted(demand.glob("W_origin_demand[0-9][0-9].csv"))
    provenance += sorted(demand.glob("V_origin_demand[0-9][0-9].csv"))
    dependencies = [run / name for name in (
        "rnorm_v8.R", "NRB_graphs_datasets_v8.R", "maps_animations_v8.R",
        "finalogs_v8.R", "bypassMC_v8.R", "bypass_maps_animations_v8.R")]
    for path in dependencies:
        if not path.is_file():
            raise FileNotFoundError(path)
    report = {
        "run": str(run), "model": model.name, "model_sha256": sha(model),
        "previous_model": original.name, "previous_sha256": sha(original),
        "previous_model_retained": True, "simulation_launched": False,
        "monte_carlo_runs": int(pars["monte_carlo_runs"]),
        "start_year": int(pars["start_year"]), "end_year": int(pars["end_year"]),
        "uncapped_regrowth": int(pars["uncapped_regrowth"]), "countries": countries,
        "w_tof_mechanism": "domestic_origin_preserving" if model.name.endswith("v13.egoml") else "legacy_regional_pool",
        "provenance": [{"relative_path": p.relative_to(run).as_posix(), "sha256": sha(p)} for p in provenance + dependencies],
        "utc": dt.datetime.now(dt.timezone.utc).isoformat(),
    }
    return run, provenance, report


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--run-dir", type=Path, action="append", required=True)
    ap.add_argument("--model", type=Path, required=True)
    ap.add_argument("--baseline", type=Path, required=True)
    ap.add_argument("--install", action="store_true")
    ap.add_argument("--report", type=Path)
    args = ap.parse_args()
    if args.report and args.report.exists():
        raise FileExistsError("Report exists; choose another path")
    model, baseline = args.model.resolve(strict=True), args.baseline.resolve(strict=True)
    inspected = [inspect_run(run, model, baseline) for run in args.run_dir]
    if args.install:
        for run, provenance, report in inspected:
            capture = run / "Sourcing"
            if capture.exists() and any(capture.glob("MC*/mask_*.tif")):
                raise FileExistsError(f"Existing sourcing results present; cannot install into {run}")
            metadata = capture / "metadata"
            receipt = metadata / "model_installation.json"
            if receipt.exists():
                prior = json.loads(receipt.read_text(encoding="utf-8"))
                if prior.get("model_sha256") != report["model_sha256"]:
                    raise FileExistsError(f"A different model was prepared here: {receipt}")
            for source in provenance:
                destination = metadata / "input_snapshot" / source.relative_to(run)
                if destination.exists() and sha(destination) != sha(source):
                    raise FileExistsError(f"Different provenance snapshot already exists: {destination}")
        for run, provenance, report in inspected:
            target = run / model.name
            if not target.exists():
                shutil.copy2(model, target)
            if sha(target) != report["model_sha256"]:
                raise IOError("Model SHA-256 mismatch after copying")
            metadata = run / "Sourcing/metadata"
            metadata.mkdir(parents=True, exist_ok=True)
            (run / "Sourcing/static").mkdir(exist_ok=True)
            for mc in range(1, report["monte_carlo_runs"] + 1):
                (run / "Sourcing" / f"MC{mc:03d}").mkdir(exist_ok=True)
            for source in provenance:
                destination = metadata / "input_snapshot" / source.relative_to(run)
                destination.parent.mkdir(parents=True, exist_ok=True)
                if destination.exists() and sha(destination) != sha(source):
                    raise FileExistsError(f"Different provenance snapshot already exists: {destination}")
                if not destination.exists():
                    shutil.copy2(source, destination)
                if sha(destination) != sha(source):
                    raise IOError("Provenance SHA-256 mismatch after copying")
            receipt = metadata / "model_installation.json"
            if not receipt.exists():
                receipt.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
            for item in report["provenance"]:
                if sha(run / item["relative_path"]) != item["sha256"]:
                    raise IOError("A canonical input/dependency changed during installation")
    result = {"installed": args.install, "runs": [x[2] for x in inspected]}
    if args.report:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({"installed": args.install, "run_count": len(inspected), "model": model.name,
                      "sha256": sha(model), "previous_v11_retained": True}))


if __name__ == "__main__":
    main()
