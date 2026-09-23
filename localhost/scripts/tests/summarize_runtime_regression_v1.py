"""Compile the explicit ECSA-upgrade regression gates, without rerunning models."""
from __future__ import annotations

import json
from pathlib import Path

ROOT = Path("E:/MoFuSS_Active/ecsa_sourcing_and_speed_v1/runtime_regression")
PAIRS = [
    ("lso_v11_capped_3y", "lso_v11_capped_3y_repeat", "baseline_repeatability"),
    ("lso_v11_capped_3y", "lso_v12_prod_capped_3y", "v12_compatibility"),
    ("lso_v11_uncapped_3y", "lso_v12_prod_uncapped_3y", "v12_compatibility"),
    ("lso_v11_capped_21y", "lso_v12_prod_capped_21y", "v12_compatibility"),
    ("synthetic_v11_finite_3y", "synthetic_v12_prod_3y", "v12_compatibility"),
    ("lso_v11_capped_3y", "lso_v13_prod_capped_3y", "v13_singleton_compatibility"),
    ("synthetic_v11_finite_3y", "synthetic_v13_prod_3y", "v13_intentional_bug_correction"),
    ("synthetic_zero_v11_3y", "synthetic_zero_v12_3y", "v12_zero_demand_endpoint"),
    ("synthetic_zero_v11_3y", "synthetic_zero_v13_3y", "v13_zero_demand_endpoint"),
]


def read(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def main() -> None:
    gates = []
    for left, right, purpose in PAIRS:
        path = ROOT / f"comparison_{left}_vs_{right}.json"
        result = read(path)
        sides = {}
        for name in (left, right):
            m = read(ROOT / name / "fixture_manifest.json")
            r = read(ROOT / name / "runtime_result.json")
            sides[name] = {
                "model_source_sha256": m["model_source_sha256"],
                "staged_model_sha256": m["staged_model_sha256"],
                "years": m["years"], "mc": m["mc"], "uncapped": m["uncapped"],
                "returncode": r["returncode"], "elapsed_seconds": r["elapsed_seconds"],
                "scientific_outputs": r["scientific_outputs"],
            }
        gates.append({
            "purpose": purpose, "left": left, "right": right,
            "compared": result["compared"], "identical": result["identical"],
            "different_count": len(result["different"]),
            "missing_count": len(result["missing"]),
            "additional_count": len(result["additional"]),
            "all_baseline_sha256_identical": result["all_baseline_scientific_outputs_sha256_identical"],
            "comparison_report": str(path), "fixtures": sides,
        })
    compatible = [g for g in gates if g["purpose"].startswith("v12_")]
    report = {
        "scope": {
            "engine": "Dinamica EGO 2.4.1.20140602 native Windows runtime",
            "fixture_grid": "269 rows x 273 columns, original completed LSO grid",
            "regional_fixture": "Arbitrary west/east AAA/BBB split; asymmetric domestic W and overlapping V origins",
            "draws": "Seven frozen MC input tables copied from completed LSO; 1..3 MC prefix selected; same input SHA hashes required",
            "allocation": "Bypass Patchers=Yes; deterministic pixel selection, stochastic biological MC parameters preserved",
            "idw": "Existing IDW inputs copied only; no IDW computations performed",
            "staging": "Four original external R initialization/reporting calls removed identically, source model unchanged; only duration/MC/mode parameters adjusted",
            "execution": "One engine processor, predefined engine seed; child TEMP/TMP isolated in final regional and endpoint tests",
            "not_covered": ["Full ECSA 51-year runtime", "Stochastic Patcher branch", "External R reporting lifecycle", "Concurrency/speed extrapolation to full ECSA"],
        },
        "v12_legacy_outputs_compared": sum(g["compared"] for g in compatible),
        "v12_legacy_outputs_sha256_identical": sum(g["identical"] for g in compatible),
        "v12_all_compatibility_gates_pass": all(g["all_baseline_sha256_identical"] for g in compatible),
        "exceptions": [
            "v13 intentionally fixes regional pooling of W TOF deficits; regional legacy outputs need not be identical.",
            "LSO v13 has 9 Ex_agr_harv diagnostic TIFF hash differences: 915 null-versus-zero cells each; all common finite cells and sums identical; all other 216 baseline outputs byte-identical.",
            "Measured elapsed times on small fixtures show observer overhead, not a proven full-region speed gain. Parallel test jobs and compilation/file caching affect timings.",
        ],
        "gates": gates,
        "domestic_tof_native_bugproof_report": str(ROOT.parent / "domestic_tof_bugproof/bugproof_report.json"),
        "singleton_pixel_diagnostics": str(ROOT / "pixel_comparison_lso_v11_capped_3y_vs_lso_v13_prod_capped_3y.csv"),
    }
    destination = ROOT.parent / "regression_release_summary.json"
    destination.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({"report": str(destination),
        "v12_compared": report["v12_legacy_outputs_compared"],
        "v12_identical": report["v12_legacy_outputs_sha256_identical"],
        "v12_all_compatibility_gates_pass": report["v12_all_compatibility_gates_pass"]}, indent=2))


if __name__ == "__main__":
    main()
