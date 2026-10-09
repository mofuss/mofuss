# Legacy WebMoFuSS performance audit

Status: **standalone candidate passed local full-workflow validation**.
The guarded candidate preserves all 4,074 scientific raster/table files in a
fresh two-MC, 51-year case, both geographic databases, all output names, every
rendered PDF page and the animation. Its larger frozen-input test observed a
49.4% reduction in dynamics-only elapsed time. Neither test establishes a
universal speed gain or compatibility with every production option.
The supplied server backup has been inspected. The Windows launcher supplied
on 2026-10-08 confirms the local legacy Dinamica executable, independently
measured as **2.4.1.20140602**, with `-processors 0 -log-level 4`.
The user confirms this computer runs the Windows simulation and has a maximum
of **eight logical processors**; the earlier 70-core assumption is superseded.
Linux runs preparation and hands the simulation to Windows. No server files
or canonical simulation directories were changed.
See [SERVER_BACKUP_AUDIT.md](SERVER_BACKUP_AUDIT.md) for the 2026-10-08 findings
and the launch evidence. The small real-data case was prepared and completed
locally from the supplied global inputs. See
[SMALL_CASE_VALIDATION.md](SMALL_CASE_VALIDATION.md) for its full evidence and
the explicit test-environment adaptations.

Later partial additions under `F:\webmofuss_speedup_sandbox\webmofuss_files`
contain three case folders (`6840ecfcad627`, `68e88410b267d`, `68e895bd7eca3`).
They currently supply source/preparation data, not runnable dynamics inputs or
completed outputs. All three configure Windows R 4.5.0 in `Rpath.csv`; their
companion TXT discovery files identify Linux R. This is consistent with the
Linux-preparation/Windows-simulation handoff. It does not identify the Dinamica
build by itself. The local preparation fixture uses the backup's global Kenya
inputs, retaining national demand normalization before selecting a small
Nakuru area; it is separate from those three partial case folders.

**Historical compatibility counterexample:** appending an unused text column to the
numeric MC tables succeeds with the original but failed in the unguarded lookup candidate
with `Variant type is not real`. That version must not be deployed as an
unconditionally compatible replacement. The small probe's original
produced all 12 expected TIFFs; its candidate produced none. Evidence is in
`E:\MoFuSS_Active\webmofuss_performance_audit\lookup_contract_probe\results.json`.

On 2026-10-08 the builder and candidate were revised to guard both numeric
column types and MC-row presence, falling back to the original expressions
when caching is unsuitable. Local native microprobes cover numeric tables,
unused text columns, a bypassed missing initial-stock row, and missing rows on
an all-NoData map. These targeted checks supplement the completed full case.
See [LOOKUP_GUARD_AUDIT.md](LOOKUP_GUARD_AUDIT.md).

**The three-row historical timing table later in this document belongs to the
earlier unguarded candidate** (SHA-256
`a9a661abb802a66f4639ddbf7903bd660251b97f4fc0c8b1627e8255862ec54e`).
They must not be represented as measurements of the newly guarded artifact.
The guarded candidate's separate measurements follow.

The first guarded **full-graph correctness** comparison now passes: the same
two-MC, 21-year capped frozen fixture produced all **740 scientific files**
byte-identical to its original baseline (719 TIFFs), with no missing or added
files. All 614 frozen-input hashes matched. Evidence:
`E:\MoFuSS_Active\webmofuss_performance_audit\comparison_baseline_v3_long_2mc_21y_vs_guarded_v3_long_2mc_21y.json`.
The guarded run took 162.48 seconds while four other Dinamica jobs and new-case
R preprocessing were active. That is not a comparable speed measurement
against the earlier 144.21-second baseline. Both sides of this correctness
test omit the same four R callbacks. The separate fresh case below retains them.

A fresh sequential pair using the guarded candidate also passes the uncapped
three-MC, three-year fixture: all **186 files** (159 TIFFs, 27 CSVs) are
byte-identical, with no missing or added scientific outputs. The original took
125.71 seconds and the guarded candidate 63.61 seconds, a **49.4% observed
reduction**. Both used two processors while unrelated simulations were active;
small-area R preparation was paused for both runs. This is one dynamics-only
timing pair on a busy host, not an end-to-end or idle-server benchmark.
Evidence: `comparison_baseline_guard_pair_uncapped_vs_guarded_pair_uncapped.json`
and each fixture's `runtime_result.json` under the temporary audit root.

The fresh **full-workflow** case also passes: two MCs, 2000–2050, a 33 by 33
Nakuru grid, and capped BaU. All four R callbacks completed on both sides.
All **4,074 scientific files** (1,752 TIFFs, 2,322 CSVs) are byte-identical;
two GeoPackages have identical schemas, attributes, stored CRS and exact
normalized geometries. The same 5,804 produced filenames exist. All ten PDF
page renders and the MP4 animation match exactly. The original took 546.30
seconds and the candidate 708.43 seconds, but four other simulations started
before the candidate: this pair establishes compatibility, **not** a speed
comparison. R callbacks consumed about 83% of the original tiny-case elapsed
time, limiting the benefit available from EGOML-only changes for that case.
Detailed timing, provenance and limits are in `SMALL_CASE_VALIDATION.md`.

## Deliverable

`7_dyn_Sc17_webmofuss_ctrees_g_v3_fast.egoml` is a standalone EGOML candidate.
It requires no Python helper at runtime. The original
`7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml` is unchanged.

The candidate preserves the old scientific model and single-scenario workflow.
It does not introduce paired scenario runs, change the number of realizations,
or substitute the newer localhost model.

Two conservative changes are included:

1. For numeric tables containing the requested MC row, select each
   realization's parameter row once, then use numeric
   lookup tables in four raster expressions. The original arithmetic order,
   double-valued parameters, float32 output stages, class indices, and MC row
   identity are retained. These four maps are initialized once per MC, not once
   per simulated year. The local legacy engine cannot compile the original
   general-table raster accesses into native expressions.
2. Disable eight attribute flags whose results are not consumed: unnecessary
   statistical summaries on six maps, plus dynamic/statistical attributes on
   one map used only for cell width. Required sums and cell counts stay enabled.

All original loaders, output writers and their scopes, external R calls,
wizard metadata, constants, paths, precision settings, and Patcher settings
are retained. The reproducible builder checks the exact reviewed source hash
and rejects other model revisions.

The builder also offers `--last-mc-debugging`, disabled by default. That option
writes 25 shared diagnostic raster families only on the last realization.
For 30 MCs, it would remove 29/30 of those particular writes, not 29/30 of total
runtime. Intermediate file availability changes, so this option is not included
in the shipped candidate and needs a check for live web readers before use.

## Validation and limitations

Source repository: `C:\Users\UNAM\Documents\mofuss`.
Read-only fixture input source: `F:\LSO_1000m_bau1_2050_mc3_capped`.
Disposable tests and logs:
`E:\MoFuSS_Active\webmofuss_performance_audit`.

The local executable is Dinamica `2.4.1.20140602`, using two processors and
`-predefined-seed`. The frozen-input tests run each variant sequentially on separate
copies with identical frozen inputs. They remove the same four external R calls
from each test copy so initialization cannot regenerate MC draws and reporting
cannot write outside the fixture. The deliverable retains those calls.

The newer local fixture provides decennial IDW files. The first unadapted test
correctly failed on missing `In/IDW_C++_fw_v02.tif`. Subsequent fixtures copy each
decade's IDW to missing annual filenames solely in disposable test storage.
This permits a controlled old-model comparison, but does not make that fixture
a substitute for the actual web case. Every adaptation is recorded and hashed.

All three successful original/candidate pairs had byte-identical scientific
files and no missing or added files:

| Frozen fixture | MCs | Annual steps | Identical files | Original | Candidate | Elapsed time reduction |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Capped growth | 3 | 3 | 177 (150 TIFF, 27 CSV) | 111.07 s | 65.33 s | 41.2% |
| Uncapped/CTrees growth | 3 | 3 | 186 (159 TIFF, 27 CSV) | 112.58 s | 62.85 s | 44.2% |
| Capped growth, longer run | 2 | 21 | 740 (719 TIFF, 21 CSV) | 144.21 s | 97.73 s | 32.2% |

These are single whole-process timing observations for each variant. The
different growth branches are separate regression fixtures, not a change to
the web's one-scenario-per-job workflow. There were 1,103 byte-identical file
comparisons across the three pairs. This is not a benchmark of the server or
the full R/web workflow, and it does not negate the mixed-table counterexample.

The current eleven static tests passed. The process-MC prototype also ran
three realizations with at most two concurrent one-processor workers. It
preserved all 186 scientific outputs and proved at least 52.66 seconds of
native-process overlap. Its worker phase took 105.83 seconds versus the
earlier guarded serial run's 63.61 seconds, on a busy host. This demonstrates
feasibility, not a speed benefit; small jobs can be dominated by repeated
startup/compilation. It remains an experiment, not a production launcher.

Static checks cover unchanged I/O and external calls, expression arithmetic,
map precision, MC helper scope, live normalizers, unique/resolved IDs, exact
shipped-artifact reproducibility, and rejection of unreviewed source revisions.

The parameter-row optimization assumes the numeric matrices produced by the
legacy R preprocessing. The supplied backup's `rnorm_v3.R`, lines 643–678,
generates numeric Key/LULC columns, drops helper columns, and replaces NA/NaN
with 0.11111 before writing all three matrices. The fresh full case confirms
one numeric key, 760 numeric parameter columns, rows 1 and 2, and finite
values throughout all three generated matrices. They match across both runs.
Eagerly reading a full row differs from conditionally
reading individual columns for the confirmed mixed-type counterexample, and
also touches absent map categories and initial-stock cells that tree-cover mode
would otherwise bypass.

## Real multicore execution

See [MC_PARALLELISM.md](MC_PARALLELISM.md) for the exact graph and file audit and
[RUNTIME_COMPATIBILITY.md](RUNTIME_COMPATIBILITY.md) for runtime evidence.

MC realizations are independent after input generation; the nine cross-MC
feedback tables only collect results. Annual state within each realization is
dependent and must remain sequential. This is a good candidate for coarse
parallel execution, provided global MC indices and output collection are handled
correctly.

The local old engine did not overlap independent loop iterations or sibling
groups when given four processors. Identical scheduler probes did overlap under
the separately installed 8.13 engine. Consequently, setting the processor count
or copying graph groups is not sufficient evidence of parallel MC on the server.

Thirty processes with `-processors 1`, or thirty with `-processors 2`, are a
plausible architecture for 30 or 60 logical processor capacity, respectively.
They need private working/temp directories, distinct global MC identities,
appropriate random streams, and one ordered gather/report stage. They must also
fit memory and I/O capacity and the web job's allocated CPU budget. This is an
orchestration change beyond the current single-file candidate.

Worker count must be calculated per job. The sample backup allocates eight
Linux CPUs and passes only a session ID to the Windows launcher. The supplied
launcher uses `-processors 0` for automatic Windows processor detection, and
the user confirms that this Windows host has at most eight logical processors.
The earlier 30/60-worker example describes the architecture only; it is not
appropriate for this host. No existing handoff communicates a separate
per-job Windows CPU allocation, so competing jobs also matter.

Running thirty unmodified scripts with MC count set to one is incorrect: each
would select the first parameter draw, rerun preprocessing/reporting, and
overwrite common files. The prototype under `tests/` is a frozen-input mechanism
experiment only; it is not a production worker launcher or a validated RNG policy.

## Reproduce and continue

Run source checks from the repository:

```powershell
python -B -m unittest discover -s webmofuss/tests -p test_webmofuss_fast.py -v
```

Build to a new, unused path (the builder refuses overwrites):

```powershell
python -B webmofuss/tools/build_webmofuss_fast.py --output C:/path/to/new_candidate.egoml
```

`tests/run_webmofuss_regression.py` provides `stage`, `run`, and `compare`
subcommands with an explicit temporary `--root`. It delegates the frozen-input
copying and engine launch to the existing localhost regression harness. Its
comparison additionally rejects candidate-only scientific files.

The complete local workflow, scientific outputs and R reports have now been
checked on the launcher-selected engine. Remaining coverage limits include
the production wrapper's exact IDW settings, options beyond the tested capped
BaU full case, stochastic patchers, the reporting branch at 30 or more MCs,
and the launcher's automatic processor setting (tests bounded it to two).
An idle-host comparison and peak-memory measurement would strengthen any
production performance claim; current timings must retain the qualifications
above. No source or server launcher has been replaced.

For a deployment using the existing launcher, the candidate must be installed
under `7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml`, its current expected filename.
Keep the original bytes available for rollback and make the swap between jobs.
The `_fast` filename distinguishes the delivered artifact in this repository;
no new inputs, helper process, R installation or output schema are required
by the optimized EGOML itself.

Original SHA-256:
`e4ce6ab47a12bbc7ed1f290a95ad6c1477182d21e95fba1428d816ecc85bca2d`.
Current guarded candidate SHA-256:
`15d9e2ff6f5aa9b166ab8a9ddc18f3f21cbd0960a18ef4c25e426fb5f5db5932`.
