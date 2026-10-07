"""Build optimized Windows v13/v14 candidates; never run a model.

The validated Windows v13 is the source. v14 retains the existing annual LUC,
initial-stock support and K-on-return rules. v14 additionally includes the
approved annual sourcing-cache correction and a real fixed-input LUC=1 branch.
The scientific correction intentionally changes affected dynamic-LUC results;
the performance transforms themselves retain their exact-output contract.
"""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

from optimize_windows_io import optimize_io
from optimize_dinamica_windows import optimize_table_lookups
from build_woodman_dinamica_v14 import build_model
from windows_report_option import expose_report_option, assert_presentation_only
from windows_engine_compatibility import explicit_annual_filename_steps


def build(source: str, engine8: bool = False) -> tuple[dict[str, str], dict]:
    v13, io = optimize_io(source)
    v13, lookup = optimize_table_lookups(v13)
    rendered = expose_report_option(v13)
    assert_presentation_only(v13, rendered)
    v13 = rendered
    if engine8:
        v13 = explicit_annual_filename_steps(v13)
    v14 = build_model(v13)
    models = {f"10_dyn_Sc17_webmofuss_ctrees_g_v{v}.egoml": s
              for v, s in ((13, v13), (14, v14))}
    return models, {
        "policy": "exact_performance_transforms_plus_approved_v14_annual_eligibility_correction",
        "performance_policy": ("exact_rasters_and_decoded_scalars_with_reviewed_report_rounding"
                               if engine8 else "exact_retained_scientific_outputs_required"),
        "v14_correction_policy": {
            "dynamic_luc": "Exact agreement with independently recomputed annual sourcing domains",
            "fixed_luc": "Preserve v13 scientific mechanics; initial-stock support guards retain their documented exclusions",
            "capture_observers": "Use explicit annual-domain capture contract; filenames and annual accumulator outputs intentionally differ",
        },
        "engine8_explicit_annual_filename_steps": engine8,
        "source_text_sha256": hashlib.sha256(source.encode()).hexdigest(),
        "models": {name: hashlib.sha256(s.encode()).hexdigest() for name, s in models.items()},
        "io": io, "lookups": lookup,
        "annual_sourcing_cache_correction_included": True,
        "annual_sourcing_capture_contract": "annual_domain_after_static_npa_cache_v1",
        "fixed_luc_reads_existing_static_inputs": True,
        "v14_scientific_change": "Apply current landscape eligibility after cached static NPA adjustment each year",
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--v13-source", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--engine-8", action="store_true",
                        help="Use the verified explicit Step carrier for EGO 8 sourcing filenames")
    args = parser.parse_args()
    models, report = build(args.v13_source.read_text(encoding="utf-8"), args.engine_8)
    report["source_file"] = str(args.v13_source.resolve())
    report["source_file_sha256"] = hashlib.sha256(args.v13_source.read_bytes()).hexdigest()
    destinations = {args.output_dir / name: text for name, text in models.items()}
    report_path = args.output_dir / "windows_performance_build.json"
    if any(path.exists() for path in (*destinations, report_path)):
        raise FileExistsError("Use a new output folder; existing models/reports are preserved")
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for path, text in destinations.items():
        # Explicit bytes keep reported SHA-256 values identical to files on
        # Windows as well as other hosts (write_text can translate newlines).
        path.write_bytes(text.encode("utf-8"))
    report_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report["models"], indent=2))


if __name__ == "__main__":
    main()
