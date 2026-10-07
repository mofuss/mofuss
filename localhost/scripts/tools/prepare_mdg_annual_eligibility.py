"""Install the approved v14 annual-eligibility correction in the four D runs.

F remains on the verified v13 release. Default is a read-only preflight apart
from the requested report. --install writes only the D model, code backups and
preparation manifests. It never starts simulations or changes scientific data.
The release requires independent full-graph and sourcing-reader evidence.
"""
from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
from pathlib import Path
import shutil
import xml.etree.ElementTree as ET

from prepare_mdg_windows_performance import constant, rows, ENGINES

SCRIPTS = Path(__file__).resolve().parents[1]
V13 = "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml"
V14 = "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml"
READER = SCRIPTS / "postprocessing_sourcing/2post_runtime_sourcing_v1.R"
CONTRACT = "annual_domain_after_static_npa_cache_v1"
CASES = {"fixed_capped", "fixed_uncapped", "dynamic_capped",
         "dynamic_uncapped", "dynamic_capped_patcher"}
RELEASE = "windows_annual_eligibility_2026-10-06"


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load(path):
    return json.loads(path.read_text(encoding="utf-8-sig"))


def verify_evidence(integration_path, reader_path, candidate, baseline):
    gate, reader = load(integration_path), load(reader_path)
    cases = gate.get("cases", [])
    if (gate.get("all_checks_passed") is not True
            or set(gate.get("required_cases", [])) != CASES
            or len(cases) != len(CASES) or {c.get("case") for c in cases} != CASES
            or not all(c.get("passed") is True for c in cases)
            or gate.get("years") != 3 or gate.get("mc") != 3):
        raise ValueError("The five-case annual-eligibility integration gate is incomplete")
    expected_hashes = {"source_v13_sha256": sha(SCRIPTS / V13),
                       "source_v14_sha256": sha(baseline),
                       "candidate_sha256": sha(candidate)}
    for key, expected in expected_hashes.items():
        if gate.get(key) != expected:
            raise ValueError(f"Integration evidence does not match {key}")
    if sha(Path(gate["candidate_source"])) != expected_hashes["candidate_sha256"]:
        raise ValueError("The integrated candidate source changed after verification")
    for case in cases:
        runtimes = case.get("runtime_results", [])
        if len(runtimes) != 2 or len(set(runtimes)) != 2:
            raise ValueError("Each integration case needs reference and candidate runtimes")
        roles = []
        for value in runtimes:
            path = Path(value)
            runtime = load(path)
            manifest = load(path.parent / "eligibility_manifest.json")
            roles.append(manifest.get("role"))
            command = runtime.get("command", [])
            if (runtime.get("returncode") != 0 or not command
                    or Path(command[0]).resolve() != ENGINES["legacy"].resolve()
                    or "-disable-native-expressions" in command
                    or Path(command[-1]).resolve() != (path.parent / "regression_model.egoml").resolve()
                    or manifest.get("case") != case["case"]
                    or manifest.get("fixture_model_sha256") != sha(path.parent / "regression_model.egoml")):
                raise ValueError(f"Runtime evidence does not match the verified native graph: {path}")
            expected = (expected_hashes["candidate_sha256"] if manifest.get("role") == "candidate"
                        else expected_hashes["source_v13_sha256"] if case["case"].startswith("fixed_")
                        else expected_hashes["source_v14_sha256"])
            if manifest.get("source_sha256") != expected:
                raise ValueError(f"Runtime source hash differs from the release: {path}")
        if set(roles) != {"reference", "candidate"}:
            raise ValueError("Each case requires one reference and one candidate execution")
    if (reader.get("all_checks_passed") is not True
            or reader.get("source_sha256") != sha(READER)
            or reader.get("full_integration_replay_passed") is not True
            or reader.get("integration_candidate_sha256") != expected_hashes["candidate_sha256"]):
        raise ValueError("Sourcing-reader compatibility and integrated exact-replay gate failed")
    return {**expected_hashes, "integration_evidence_sha256": sha(integration_path),
            "reader_evidence_sha256": sha(reader_path), "reader_sha256": sha(READER)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--candidate", type=Path, default=SCRIPTS / V14)
    parser.add_argument("--baseline-v14", type=Path, required=True)
    parser.add_argument("--integration-verification", type=Path, required=True)
    parser.add_argument("--reader-verification", type=Path, required=True)
    parser.add_argument("--previous-release", type=Path, required=True)
    parser.add_argument("--report", type=Path, required=True)
    parser.add_argument("--install", action="store_true")
    args = parser.parse_args()
    evidence = verify_evidence(args.integration_verification, args.reader_verification,
                               args.candidate, args.baseline_v14)
    old = load(args.previous_release)
    if old.get("installed") is not True or len(old.get("runs", [])) != 8:
        raise ValueError("Expected the completed eight-run performance preparation record")
    expected_runs = {Path(f"{drive}:/MDG_1000m_{scenario}_2050_mc3_{mode}").resolve()
                     for drive in ("F", "D") for scenario in ("bau1", "ics3")
                     for mode in ("capped", "uncapped")}
    if {Path(r["run"]).resolve() for r in old["runs"]} != expected_runs:
        raise ValueError("Previous preparation record does not describe the eight MDG runs")
    source = args.candidate.read_text(encoding="utf-8")
    model = ET.fromstring(source)
    marker = model.find("property[@key='mofuss.sourcing.capture.contract']")
    if marker is None or marker.get("value") != CONTRACT:
        raise ValueError("Corrected capture contract is missing from the candidate")
    result = {"release": RELEASE, "installed": args.install, "evidence": evidence,
              "created_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
              "engine": str(ENGINES["legacy"]), "runs": []}
    planned = []
    for prior in old["runs"]:
        run = Path(prior["run"])
        # Fail before any write if the user or another process changed runtime
        # code since the reviewed installation. Scientific outputs are unread.
        for name, record in prior["files"].items():
            if sha(run / name) != record["new_sha256"]:
                raise ValueError(f"Runtime code changed since the previous release: {run / name}")
        manifest_path = run / "windows_performance_preparation.json"
        current = load(manifest_path)
        if current.get("engine_choice") != "legacy" or current.get("engine_flags") != []:
            raise ValueError(f"Unexpected selected engine: {run}")
        params = {r["Var"]: r["ParCHR"] for r in rows(run / "LULCC/TempTables/parameters_dinamica.csv")}
        for key, value in (("start_year", "2000"), ("end_year", "2050"),
                           ("monte_carlo_runs", "3"), ("uncapped_regrowth", str(prior["uncapped"]))):
            if params.get(key) != value:
                raise ValueError(f"Unexpected {key} in {run}")
        if run.drive.upper() == "F:":
            result["runs"].append({"run": str(run), "action": "verified_unchanged_v13",
                                    "luc": 1, "mc_reruns": prior["mc_reruns"]})
            continue
        for year in range(2000, 2051):
            for stem in ("LULCt3_c_", "TOFvsFOR_mask3_", "LULCt3_transition_"):
                path = run / f"LULCC/TempRaster/{stem}{year}.tif"
                if not path.is_file():
                    raise FileNotFoundError(path)
        configured = ET.fromstring(source)
        constant(configured, "v302").text = "3"
        constant(configured, "v256").text = ".yes" if prior["mc_reruns"] else ".no"
        for key in ("v313", "v257"):
            if constant(configured, key).text != ".yes":
                raise ValueError("Expected Patcher bypass and standard report generation")
        payload = ET.tostring(configured, encoding="utf-8", xml_declaration=True)
        payload_hash = hashlib.sha256(payload).hexdigest()
        record = {"run": str(run), "action": "install_corrected_v14", "luc": 3,
                  "mc_reruns": prior["mc_reruns"], "paired_bau": prior["paired_bau"],
                  "model": V14, "previous_sha256": sha(run / V14), "new_sha256": payload_hash}
        updated = {**current, "release": RELEASE, "previous_release": current["release"],
                   "capture_contract": CONTRACT, "annual_eligibility_evidence": evidence,
                   "files": {**current["files"], V14: {"new_sha256": payload_hash,
                                                       "previous_sha256": record["previous_sha256"]}}}
        manifest_bytes = (json.dumps(updated, indent=2) + "\n").encode("utf-8")
        planned.append((run, {V14: payload, manifest_path.name: manifest_bytes}))
        result["runs"].append(record)
    # Check every backup before making the first change.
    for run, files in planned:
        for name in files:
            backup = run / "_code_backups" / RELEASE / name
            if backup.exists() and backup.read_bytes() != (run / name).read_bytes():
                raise FileExistsError(f"Conflicting previous backup: {backup}")
    if args.install:
        for run, files in planned:
            backup_dir = run / "_code_backups" / RELEASE
            backup_dir.mkdir(parents=True, exist_ok=True)
            for name, data in files.items():
                target, backup = run / name, backup_dir / name
                if not backup.exists():
                    shutil.copy2(target, backup)
                target.write_bytes(data)
                if target.read_bytes() != data:
                    raise IOError(f"Write verification failed: {target}")
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({"installed": args.install, "D_models": len(planned),
                      "F_models_verified_unchanged": 4, "report": str(args.report)}, indent=2))


if __name__ == "__main__":
    main()
