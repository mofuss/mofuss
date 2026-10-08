"""Replay completed bounded v14 fixtures with ONLY the NRB observer correction.

The reference directory must contain the five completed annual-eligibility
candidate fixtures. Frozen MC inputs and every reference output are hash checked.
All non-NRB scientific outputs must remain byte-identical. No source run is
modified and no fixture is overwritten.
"""
from __future__ import annotations
import argparse
import json
import re
from pathlib import Path
import shutil
import sys
import xml.etree.ElementTree as ET

sys.dont_write_bytecode = True
import dinamica_runtime_regression_v1 as harness
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
from fix_woodman_nrb_attribution import correct_nrb_attribution, CONTRACT

CASES = ("fixed_capped", "fixed_uncapped", "dynamic_capped", "dynamic_uncapped", "dynamic_capped_patcher")


def check_output_invariants(target: Path, *, years: int = 3, mc: int = 3) -> dict:
    """Audit every corrected output cell without rerunning a scratch fixture.

    Ordinary zero-harvest stock-domain gaps must retain finite ledger history.
    Impossible positive-harvest/missing-stock states may make the ledger NoData;
    those invalid states must remain visible rather than silently becoming zero.
    """
    import numpy as np
    import rasterio

    target = Path(target).resolve()
    if "MoFuSS_Active" not in target.parts or not (target / "fixture_manifest.json").is_file():
        raise ValueError("Invariant evidence must remain in a staged MoFuSS_Active fixture")
    if json.loads((target / "runtime_result.json").read_text())["returncode"] != 0:
        raise ValueError("Invariant checks require a successful completed run")
    model = ET.parse(target / "regression_model.egoml").getroot()
    marker = model.find("property[@key='mofuss.nrb.attribution.contract']")
    if marker is None or marker.get("value") != CONTRACT:
        raise ValueError("Fixture lacks corrected NRB contract")
    parents = {child: parent for parent in model.iter() for child in parent}
    nodes = {p.get("id"): n for n in model.iter() for p in n.findall("outputport")}
    grid = None
    raster_count = checked_values = 0

    def read(relative, *, ledger=False):
        nonlocal grid, raster_count
        with rasterio.open(target / relative) as source:
            current = (source.width, source.height, source.transform, source.crs)
            if grid is None:
                grid = current
            if current != grid:
                raise AssertionError("Output grid differs: " + relative)
            if ledger and source.dtypes != ("float32",):
                raise AssertionError("Ledger must use production float32: " + relative)
            result = source.read(1, masked=True).astype("float64").filled(np.nan)
        raster_count += 1
        return result

    def require(condition, label):
        nonlocal checked_values
        condition = np.asarray(condition)
        checked_values += condition.size
        if not np.all(condition):
            raise AssertionError(f"{label}: {np.count_nonzero(~condition)} cells violate the contract")

    def check_fraction(nrb, fraction, harvest, support, invalid, label):
        require(~np.isfinite(nrb[~support]), label + " NRB outside support")
        require(~np.isfinite(fraction[~support]), label + " fNRB outside support")
        require(np.isfinite(harvest[support]) & (harvest[support] >= 0), label + " harvest")
        require(np.isfinite(nrb[support & ~invalid]), label + " finite supported NRB")
        finite = support & np.isfinite(nrb)
        require(np.isfinite(fraction[finite]), label + " finite ratio")
        require(~np.isfinite(fraction[support & ~np.isfinite(nrb)]), label + " invalid ratio")
        tolerance = 4 * np.finfo(np.float32).eps * np.maximum(1, harvest[finite])
        require((nrb[finite] >= -tolerance) & (nrb[finite] <= harvest[finite] + tolerance),
                label + " NRB in [0,H]")
        require((fraction[finite] >= -4e-7) & (fraction[finite] <= 1 + 4e-7), label + " fNRB in [0,1]")
        zero = support & (harvest <= 0)
        require(nrb[zero] == 0, label + " H=0 implies NRB=0")
        require(fraction[zero] == 0, label + " H=0 retains legacy zero fraction")
        positive = finite & (harvest > 0)
        expected = (nrb[positive] / harvest[positive]).astype(np.float32).astype(float)
        require(np.isclose(fraction[positive], expected, rtol=2e-6, atol=2e-7), label + " fNRB=NRB/H")
        return float(np.max(np.abs(fraction[positive] - expected), initial=0))

    # Verify both the source node and final-MC condition before interpreting
    # shared annual diagnostics as a particular realization.
    selector = nodes.get("v92050")
    values = {} if selector is None else {
        n.findtext("inputport[@name='valueNumber']"): n.find("inputport[@name='value']").get("peerid")
        for n in selector.findall("functor") if n.get("name") == "NumberValue"}
    annual_verified = (selector is not None and
                       " ".join(selector.findtext("inputport[@name='expression']").split()) == "[v1 = v2]" and
                       values == {"1": "v8", "2": "v282"})
    for filename, peer in (('"Debugging/nrb.tif"', "v97"), ('"Debugging/fnrb.tif"', "v96")):
        writers = [n for n in model.iter("functor") if n.get("name") == "SaveMap"
                   and n.findtext("inputport[@name='filename']") == filename]
        annual_verified = annual_verified and len(writers) == 1
        if len(writers) == 1:
            writer = writers[0]
            condition = parents[writer].find("inputport[@name='condition']")
            annual_verified = annual_verified and (
                writer.find("inputport[@name='map']").get("peerid") == peer and
                writer.find("inputport[@name='step']").get("peerid") == "v39" and
                writer.findtext("inputport[@name='suffixDigits']") == "2" and
                parents[writer].get("name") == "IfThen" and condition is not None and
                condition.get("peerid") == "v92050")
    realizations = []
    ratio_error = 0.0
    for realization in range(1, mc + 1):
        initial = read(f"Temp/2_IniSt{realization:02d}.tif")
        support = np.isfinite(initial)
        invalid_history = np.zeros(initial.shape, dtype=bool)
        for step in range(1, years + 1):
            directory = f"debugging_{realization}"
            harvest = read(f"{directory}/Harvest_tot{step:02d}.tif")
            growth = read(f"{directory}/Growth{step:02d}.tif")
            post = read(f"{directory}/Growth_less_harv{step:02d}.tif")
            invalid = support & (harvest > 0) & (~np.isfinite(growth) | ~np.isfinite(post))
            invalid_history |= invalid
            balance = read(f"{directory}/Woodfuel_balance{step:02d}.tif", ledger=True)
            require(~np.isfinite(balance[~support]), f"MC{realization} year{step} ledger outside support")
            require(np.isfinite(balance[support & ~invalid_history]),
                    f"MC{realization} year{step} ledger retains support and zero-harvest gaps")
            require(~np.isfinite(balance[invalid_history]),
                    f"MC{realization} year{step} positive-harvest missing state remains visible")
            if realization == mc and annual_verified:
                ratio_error = max(ratio_error, check_fraction(
                    read(f"Debugging/nrb{step:02d}.tif"), read(f"Debugging/fnrb{step:02d}.tif"),
                    harvest, support, invalid, f"MC{realization} year{step} annual"))
        harvest = read(f"Temp/2_CON_TOT{realization:02d}.tif")
        ratio_error = max(ratio_error, check_fraction(
            read(f"Temp/2_NRB{realization:02d}.tif"), read(f"Temp/2_fNRB{realization:02d}.tif"),
            harvest, support, invalid_history, f"MC{realization} terminal"))
        realizations.append({"mc": realization, "initial_support_cells": int(support.sum()),
                             "outside_support_cells": int((~support).sum()),
                             "invalid_positive_harvest_history_cells": int(invalid_history.sum()),
                             "zero_cumulative_harvest_cells": int(np.count_nonzero(support & (harvest <= 0)))})
    report = {"passed": True, "contract": CONTRACT, "case": target.name,
              "mc": mc, "years": years, "rasters_read": raster_count,
              "cell_invariant_checks": checked_values, "maximum_ratio_error": ratio_error,
              "annual_debug_final_mc_verified": bool(annual_verified), "realizations": realizations,
              "model_sha256": harness.sha256(target / "regression_model.egoml")}
    harness.emit_json(target / "attribution_output_invariants.json", report)
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reference-root", type=Path)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--engine", type=Path, default=harness.DEFAULT_ENGINE)
    parser.add_argument("--cases", default=",".join(CASES))
    parser.add_argument("--resume", action="store_true",
                        help="Recheck completed identical fixtures and run only cases not yet staged")
    parser.add_argument("--check-invariants-only", action="store_true",
                        help="Read completed scratch outputs and save cellwise invariant evidence; no simulations")
    args = parser.parse_args()
    args.root = args.root.resolve()
    if "MoFuSS_Active" not in args.root.parts or args.root == args.root.parent:
        parser.error("Use a named scratch folder under MoFuSS_Active")
    if args.check_invariants_only:
        for case in args.cases.split(","):
            if case not in CASES:
                parser.error("Unknown case: " + case)
            print(json.dumps(check_output_invariants(args.root / case), indent=2), flush=True)
        return
    if args.reference_root is None:
        parser.error("--reference-root is required when replaying simulations")
    args.root.mkdir(parents=True, exist_ok=True)
    results = []
    for case in args.cases.split(","):
        if case not in CASES:
            parser.error("Unknown case: " + case)
        reference = args.reference_root / ("candidate__" + case)
        runtime = json.loads((reference / "runtime_result.json").read_text())
        if runtime["returncode"] != 0 or "-predefined-seed" not in runtime["command"]:
            raise ValueError("Reference did not complete with frozen engine seed")
        if Path(runtime["command"][0]).resolve() != args.engine.resolve():
            raise ValueError("Reference uses a different engine")
        frozen = json.loads((reference / "frozen_input_hashes.json").read_text())
        old_outputs = json.loads((reference / "scientific_output_sha256.json").read_text())
        for name, digest in {**frozen, **old_outputs}.items():
            if harness.sha256(reference / name) != digest:
                raise ValueError("Reference input/output changed: " + name)
        corrected, report = correct_nrb_attribution((reference / "regression_model.egoml").read_text(encoding="utf-8"))
        if report["already_applied"]:
            raise ValueError("An uncorrected reference is required")
        target = harness.checked_fixture(args.root, case, must_exist=False)
        if target.exists():
            if not args.resume:
                raise FileExistsError("Use a new scratch root: " + str(target))
            actual = (target / "regression_model.egoml").read_text(encoding="utf-8")
            if ET.canonicalize(actual, strip_text=True) != ET.canonicalize(corrected, strip_text=True):
                raise ValueError("Completed fixture uses a different corrected graph")
            completed = json.loads((target / "runtime_result.json").read_text())
            if completed["returncode"] != 0 or Path(completed["command"][0]).resolve() != args.engine.resolve():
                raise ValueError("Cannot resume failed or different-engine evidence")
            if json.loads((target / "frozen_input_hashes.json").read_text()) != frozen:
                raise ValueError("Completed fixture input manifest differs")
            for name, digest in frozen.items():
                if harness.sha256(target / name) != digest:
                    raise ValueError("Completed fixture input changed: " + name)
        else:
            target.mkdir()
            for name in frozen:
                destination = target / name
                destination.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(reference / name, destination)
            for name in ("Sourcing/static", "Sourcing/MC001", "Sourcing/MC002", "Sourcing/MC003"):
                (target / name).mkdir(parents=True, exist_ok=True)
            # Normally rnorm creates these; external R initialization is absent.
            for name in old_outputs:
                (target / name).parent.mkdir(parents=True, exist_ok=True)
            (target / "regression_model.egoml").write_bytes(corrected.encode())
            harness.emit_json(target / "fixture_manifest.json", {
                "reference": str(reference), "contract": CONTRACT,
                "reference_model_sha256": harness.sha256(reference / "regression_model.egoml"),
                "corrected_model_sha256": harness.sha256(target / "regression_model.egoml"),
                "years": 3, "mc": 3, "observer_correction": report})
            harness.emit_json(target / "frozen_input_hashes.json", frozen)
            print("RUNNING", case, flush=True)
            harness.run(argparse.Namespace(root=args.root, name=case, engine=args.engine,
                                          processors=1, timeout=1800, verify_only=False,
                                          disable_native_expressions=False))
        new_outputs = harness.science_hashes(target)
        additions = set(new_outputs) - set(old_outputs)
        expected = {f"debugging_{mc}/Woodfuel_balance{step:02d}.tif" for mc in range(1, 4) for step in range(1, 4)}
        if additions != expected or set(old_outputs) - set(new_outputs):
            raise AssertionError("Unexpected output inventory change")
        # v117 is the legacy deforestation exclusion ledger, not a physical
        # harvest flow. Its carry over missing domains is corrected as well.
        compared = [p for p in old_outputs if "nrb" not in p.lower()
                    and not re.fullmatch(r"(?:Debugging/Cum_Fw_def|Temp/2_FW_DEF)[0-9]+[.]tif", p, re.I)]
        changed = [p for p in compared if old_outputs[p] != new_outputs[p]]
        if changed:
            raise AssertionError("Dynamics or sourcing changed: " + repr(changed))
        invariants = check_output_invariants(target)
        result = {"case": case, "passed": True, "identical_non_nrb_outputs": len(compared),
                  "nrb_output_changes": [p for p in old_outputs if old_outputs[p] != new_outputs[p]],
                  "new_ledger_maps": len(additions), "frozen_inputs_identical": True,
                  "output_invariants_passed": invariants["passed"]}
        results.append(result)
        harness.emit_json(target / "attribution_comparison.json", result)
        harness.emit_json(args.root / "attribution_integration.json", {
            "contract": CONTRACT, "cases": results, "all_selected_passed": True,
            "complete_five_case_suite": len(results) == len(CASES),
            "scope": "3 years, 3 frozen MC draws, 73437 cells; full graph, external R calls excluded"})
        print("PASS", case, "unchanged outputs:", len(compared), flush=True)


if __name__ == "__main__":
    main()
