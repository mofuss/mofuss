# Small real-data WebMoFuSS validation case

Status on 2026-10-08: preparation, all 51 annual IDW pairs, and both complete
original/guarded-candidate workflows passed. All 4,074 scientific table/raster
files are byte-identical; both geographic databases match semantically. This
establishes local compatibility for the declared fixture, not every production
configuration or an end-to-end speed benefit.

## Locations and scope

- Canonical model source: `C:\Users\UNAM\Documents\mofuss\webmofuss`.
- Read-only supplied backup: `F:\webmofuss_speedup_sandbox`.
- Disposable work: `E:\MoFuSS_Active\webmofuss_performance_audit\small_area_case`.
- Prepared case: the `case` subdirectory; provenance and preparation logs are
  in its sibling `preparation_audit` directory.

The fixture uses a Nakuru, Kenya rectangle (35.9–36.2 E, 0.45–0.15 S),
2000–2050, two Monte Carlo realizations and the default capped BaU scenario.
The original harmonizer produces a 33 × 33 EPSG:3395 grid. This is a single
scenario, matching the web job contract. It is a computational validation
case, not a regional estimate for publication.

Whole national population and demand normalization were retained before
selection of local demand locations. Cropping population before normalization
would incorrectly concentrate national demand into this small rectangle.
Sixteen raw spatial inputs were cropped on their native grids with a two-cell
halo; their source paths, storage types, extents and valid counts are recorded.

## Preparation actually exercised

`tests/prepare_small_web_case.R` sources the backup's original stages 0, 2,
2c, 3, 4, 5, 6a and 6d. The missing precomputed BaU table was generated using
the supplied original `2b_oneschema_fix_v5.R`, WHO workbook and demand
parameters. All fifteen original legacy Dinamica land-change calibration
models completed successfully. Original numerical source statements were not
rewritten. The following fixture/environment adapters were necessary:

| Adapter | Reason and boundary |
| --- | --- |
| Population VRT band named `pop_2020` | Original code selects that name; supplied TIFF uses `GlobalWorldPop`. The VRT reads unchanged TIFF band 1, with no resampling or value scaling. |
| KML contains the original rectangle and `Name` | The harmonizer supplies `ID` and `NAME_0`; duplicating those fields in the KML causes collisions. |
| Explicit local R/engine paths | Avoids machine discovery and production network-launch commands. |
| Private R startup temp and `MOFUSS_LOCAL_SCRATCH` | Keeps transient writes under the authorized E: fixture, including vector-write helpers. |
| Two processors for calibration | Bounds this test while other user jobs are active. |

An initial harmonizer attempt was stopped by the directory guard when its
helper selected the host's shared `D:/dinTemp` directory. The completed demand
and growth files were retained. `--resume-after-demand` verifies source
hashes, completion markers and persisted products, then reconstructs the
harmonizer context from disk. It does not restore serialized terra pointers.
The resumed preparation completed on 2026-10-08 at 08:55 local time.

## IDW provenance and limits

The production `idwW.R` wrapper is absent. The declared fixture instead uses
the official `mofuss/CostDistance_IDW` `OMP_specificYear` source at commit
`cdb1c36453f3aa6d9906c26526a8d63f5bfd9964`, built locally with its algorithm
unchanged. Settings are relative friction, 12 hours, exponent 1 and two workers.
They are recorded test settings, not a claim to have recovered the missing
wrapper's exact configuration. See `RUNTIME_COMPATIBILITY.md` for build and
small synthetic probes.

`tests/prepare_cpp_idw_fixture.py` copies the full annual demand CSVs with
their original headers and IDs. Only those private copies convert tonnes to
kilograms, compensating for the existing C++ division by 1,000. It also makes
private raster copies with a consistent -9999 NoData sentinel; every valid
value, mask and exact grid is verified. Model inputs remain unchanged.
Equivalent CRS definitions are compared semantically, since legacy Dinamica
omits descriptive names and authority labels from some WKT strings.

The original rectangular grid is retained: width 1011.9953708479633 m and
height 1005.235817016008 m. The C++ implementation uses float32 pixel width
for all orthogonal travel costs and preserves the full output geotransform.
No resampling or correction to this existing algorithm is applied.

The first actual annual pair passed validation: 991 walking and 1026 vehicle
valid cells were finite and positive, with exact grid/NoData preservation.
All 738 walking and 810 vehicle active origins inside the friction masks had
positive pressure. Another 78 and 48 origins are outside those masks; the
official code permits them to start travel into valid neighboring cells and
keeps their own output cells NoData. All those demands and origins are retained.
Very small positive values in unreachable cells follow the original C++
float-maximum cost initialization, not a newly imposed floor.

All 51 periods subsequently completed with the same grid, finite-value,
source-cell and NoData checks. Per-period records include input/output hashes,
command, source commit, executable hash and elapsed time in `case/_cpp_idw`.

## Full-callback comparison procedure

`tests/run_small_web_case.py` stages independent copies and selects either
original or guarded EGOML under the same deployed filename, preserving its
bytes and all four R calls. Both copies receive the same R seed, local R 4.6
path, omission of three unused retired package imports from `finalogs.R`,
and disabled automatic MiKTeX installation. These changes belong to the test
environment, not the standalone EGOML candidate.

Success requires an engine success result and fresh completion logs for all
four callbacks, followed by scientific file-name/hash comparison and report
review. A successful engine exit alone is insufficient. Two MC realizations
exercise the reporting branch for fewer than thirty draws; they do not
exercise the uncertainty-display branch used at thirty or more.

The first full launch (`cases/baseline`) exposed a test-harness serialization
mistake: quoting the numeric key in `Rpath.csv` made the legacy parser infer
a string key. It stopped before R initialization. The harness now asserts the
one-row numeric-key schema and preserves the key as unquoted `1`; fresh
`baseline_v2` and `candidate_v2` copies were staged. The failed case and logs
remain as evidence, and no EGOML change was made to address this harness error.

## Completed comparison

The successful independent cases are `cases/baseline_v2` and
`cases/candidate_v2`. Both used the launcher-selected Dinamica
`2.4.1.20140602`, two processors, `-predefined-seed`, two OpenMP threads and
R 4.6.0. The staged inputs and callback adaptations match exactly, excluding
only the selected EGOML. All four R callbacks produced fresh completion logs
without errors, and both native processes reported success.

| Check | Observed result |
| --- | --- |
| Scientific rasters | All 1,752 TIFFs byte-identical |
| Scientific tables | All 2,322 CSVs byte-identical, including six changed `LULCC/TempTables` files |
| Geographic databases | Two GeoPackages, five features total: exact schemas, typed attributes, stored CRS and normalized 2D coordinates agree |
| Complete produced-file names | Same 5,804 names; none added or missing |
| Summary PDF | Ten pages each; all ten rendered PNGs byte-identical at 90 dpi; extracted text identical |
| Animation | MP4 byte-identical, 5,329,839 bytes; complete baseline decode exited zero |

All ten original PDF pages were visually inspected: maps, plots, tables and
the animation link are present and legible. Its template credit line was
retained; this test report is not a publication-ready regional result.
The only produced-file byte differences are the two GeoPackages, PDF, four R
logs and two native logs. GeoPackage timestamps/storage layout and PDF
metadata can differ; the geographic contents and rendered report agree.

The generated MC matrices `i_st_all.csv`, `k_all.csv` and `rmax_all.csv` each
have one numeric key, two rows keyed 1 and 2, and 760 numeric parameter
columns. All 1,520 parameter values in each matrix are finite; all raster
classes are covered. All three matrices are byte-identical across runs.
This checks the actual generated table contract assumed by the row-cache
guard. The candidate log contains six additional `CalculateLookupTable`
evaluations and eight additional `MapJunction` evaluations: three selected
rows and four guarded map joins per MC, with no per-year cache regeneration.

Comparison evidence under the disposable root:

- `cases/comparison_baseline_v2_vs_candidate_v2.json`.
- Each case's `_audit/runtime_result.json`, input/output hash manifests,
  `geopackage_semantics.json` and callback logs.
- `report_review/full_artifact_comparison.json`, text extraction and all
  twenty rendered pages.
- `report_review/candidate_competing_load_observation.json`.

The `refresh-results` command expanded the completed inventories to include
`LULCC/TempTables` and GeoPackages, without rerunning either simulation. It
preserved prior runtime records in `_audit/refresh_history`. The candidate's
initial idle-load note was incorrect and was corrected there after process
inspection; the previous note remains in that history.

## Timing and practical interpretation

| Timed component | Original | Guarded candidate |
| --- | ---: | ---: |
| Whole engine invocation, including R callbacks | 546.30 s | 708.43 s |
| R parameter generation | 299.15 s | 353.48 s |
| R NRB tables/graphs | 12.70 s | 16.37 s |
| R maps/animation/report | 132.64 s | 180.20 s |
| R final logs | 9.17 s | 11.26 s |

These are **not comparable speed measurements**. No other simulations were
observed at original launch; four other Dinamica jobs began around 09:33,
before the candidate launched at 09:35, with additional R work. They were
left running. The candidate's longer elapsed time cannot be assigned to the
optimization from this pair.

The original's R callback durations sum to 453.66 seconds, approximately
83% of its full invocation. Even eliminating every other part of that tiny
case would save only about 17% at that observed split. The supplied larger
frozen fixture has 273 by 269 cells (67.44 times this grid's cell count), so
reducing interpreted raster table access matters more there. Its separate
guarded uncapped comparison observed 125.71 to 63.61 seconds, with identical
outputs, but remains a single dynamics-only pair on a busy host. See
`PERFORMANCE_AUDIT.md` for that evidence and its limits.

The full case covers capped BaU with two MCs. It does not exercise the
30-or-more-MC uncertainty-report branch or stochastic patcher configurations.
The fresh annual IDW inputs use declared official C++ fixture settings because
the production `idwW.R` wrapper was absent. Identical local R adaptations were
needed on both sides. No claim is made that these replace production inputs
or that all server/R options have been certified. The standalone candidate
contains none of these test adaptations and needs no Python helper at runtime.

Final source verification passed all eleven contract tests, including exact
candidate/builder reproduction, unchanged I/O and callback nodes, arithmetic
and precision, guards, scope and graph references. Python syntax checks
passed for all seven WebMoFuSS builder/test modules, and the R preparation
script parsed successfully. The original and candidate hashes below were
rechecked after those tests. The EGOML was not changed after native validation.

Original model SHA-256:
`e4ce6ab47a12bbc7ed1f290a95ad6c1477182d21e95fba1428d816ecc85bca2d`.
Guarded candidate SHA-256:
`15d9e2ff6f5aa9b166ab8a9ddc18f3f21cbd0960a18ef4c25e426fb5f5db5932`.
