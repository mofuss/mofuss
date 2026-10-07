# Windows performance regression

## Completed comparison, 2026-10-06

`run_windows_performance_regression.py` compared frozen baseline v13/v14
models with the combined table-lookup and I/O optimizations using the installed
Windows Dinamica EGO **2.4.1.20140602** engine. **All 1,483 retained output files
were byte-identical: 1,168 TIFFs and 315 CSVs.** Each pair also had identical
frozen scientific input hashes.

The candidate prepares table rows once per Monte Carlo draw, writes the 17
shared `Debugging` map families only on the last draw, and loads the three
static Chapman–Richards maps once per draw when uncapped growth is enabled.
The separately identified annual sourcing-cache correction is outside this
performance-only comparison.

### Coverage

Each case used a copied **269 × 273** raster grid, years **2000–2002**, and
**three different frozen Monte Carlo draws**. Runs used native expressions,
one engine processor, a predefined engine seed, and a separate child-process
`TEMP` directory. Four external R preparation/reporting calls were removed
identically from both members of each pair; the scientific dynamics graph
remained active. No canonical D: or F: run was modified.

| Case | TIFFs | CSVs | Exact retained files | Baseline seconds | Candidate seconds |
| --- | ---: | ---: | ---: | ---: | ---: |
| v13, fixed LUC, capped | 230 | 63 | 293 / 293 | 180.48 | 90.26 |
| v13, fixed LUC, uncapped | 239 | 63 | 302 / 302 | 223.58 | 93.38 |
| v14, changing LUC, capped | 230 | 63 | 293 / 293 | 314.88 | 98.97 |
| v14, changing LUC, uncapped | 239 | 63 | 302 / 302 | 253.45 | 93.70 |
| v14, changing LUC, capped, active Patcher | 230 | 63 | 293 / 293 | 315.78 | 156.29 |

**These times are indicative correctness-run measurements.** Some runs
overlapped other native probes or engine checks, and all omit the four external
R stages. They do not establish a production speedup or a full-region runtime
projection. Engine startup and compilation are included.

All 14 injected cohorts passed the existing stock/harvest/support assertions
for each of the three MC draws in each case. Cohorts include zero, missing,
and positive initial AGB in forest and TOF; initially missing non-TOF stock
with TOF gain, disappearance/reentry, and unchanged cover; forest-class return;
TOF loss/return; and missing growth parameters. Dynamic fixtures supply annual
LUC/TOF changes through the MODIS-compatible small-fixture input channel.
The active-Patcher case logged **36 Patcher executions**, compared with **18**
transition-related executions in the bypassed cases.

Whole-file equality includes annual stocks, growth, harvest, sourcing fields,
the retained shared debug maps, per-MC debug maps, and generated scientific
CSVs. The driver also supports a zero-tolerance raster content comparison if
TIFF headers differ; that fallback was unnecessary for these five pairs.
This is bounded three-year regression coverage, not a complete 51-year
BAU/ICS or all-input certification.

### Verified operation reductions

- Shared `Debugging` SaveMap calls fell from **153 to 51 per case**
  (`17 families × 3 years × 3 draws` to `17 × 3 × 1`). All retained maps were
  byte-identical to the baseline's final-draw files. The draws differ; for
  example, the fixture's forest-class K values are 2700, 8467.41741589318,
  and 3306.19184450553.
- In both uncapped cases, each of `A_c.tif`, `k_c.tif`, and `m_c.tif` was loaded
  **9 times before and 3 times after**: 27 to 9 total map loads over the fixture.
- Baseline native-compilation warnings numbered four for v13 and six for v14.
  Each candidate logged three warnings for the small once-per-MC table-row
  helpers. The raster-wide table lookups no longer require the old engine's
  unsupported general-table native-compilation path. This warning is not
  evidence that the installed compiler itself is absent or broken.

SaveMap and CalculateMap elapsed messages use coarse integer seconds, so
zero-second log entries must not be interpreted as zero computational or I/O
cost. The retained outputs establish equivalence independently of timing.

### Native compiler failure and preserved retry

The first candidate dynamic-uncapped attempt failed during startup, before any
scientific output was produced. The old native compiler/linker could not open
`engine_temp/EGOCF8A.dll` (`Permission denied`), followed by a
`boost::filesystem::remove` access-denied error. Its complete scratch folder
was preserved as
`combined_native__v14_dynamic_uncapped__failed_native_attempt1`.
A fresh native attempt in a newly staged fixture succeeded; its 302 outputs
were byte-identical. No interpreter fallback was used for the completed gate.

### Source identities and temporary evidence

| Source | SHA-256 |
| --- | --- |
| Baseline v13 | `a1d1f4d32cab12fa3ec9682db752180317c81e511114ce5a9ccf3c5e979ed28c` |
| Baseline v14, with initial-model-stock support mask | `89adc11bfc650ba9b46d74eff6b8b4978697f739423689618d2ab74dda3a79c8` |
| Combined candidate v13 | `26f14ecba7b2a64b36ebe51f03055392d57ab177b68ce191d7ff42aa98557601` |
| Combined candidate v14 | `cb47225f27563121d3a33747be9af06661a34f8a852e0cab9982084efc563709` |

Temporary evidence root:
`E:/MoFuSS_Active/mdg_windows_performance_2026-10-06/regression`.
The root contains `suite_combined_native.json`, five
`performance_comparison_combined_native__*.json` files, frozen input/output
hash inventories, source/configured/staged model hashes, engine commands and
logs, cohort results, and the preserved failed attempt. These are disposable
computational evidence; this repository report preserves the conclusions.

### Reproduction

Run the driver from the source repository, with a named scratch root, a
prepared small cohort source, frozen model paths, and a fresh label:

```powershell
python localhost/scripts/tests/run_windows_performance_regression.py `
  --root E:/MoFuSS_Active/<named-task>/regression `
  --source E:/MoFuSS_Active/<prepared-cohort-source> `
  --label baseline_native --v13 <baseline-v13.egoml> --v14 <baseline-v14.egoml>

python localhost/scripts/tests/run_windows_performance_regression.py `
  --root E:/MoFuSS_Active/<named-task>/regression `
  --source E:/MoFuSS_Active/<prepared-cohort-source> `
  --label combined_native --compare-to baseline_native `
  --v13 <candidate-v13.egoml> --v14 <candidate-v14.egoml>
```

Use `--cases` for a subset, `--stage-only` to prepare inputs without running,
and `--engine`/`--processors` for explicit engine settings. Repeat
`--engine-arg=-flag` to forward additional engine flags; every flag is recorded
in the runtime evidence. Existing successful evidence is reused only with the
same source model and engine options. Preserve failed attempts before retrying
with a fresh fixture. Run performance pairs serially on an otherwise idle
machine when qualifying elapsed-time claims.

## Full-MDG engine decision

**Production remains on Dinamica EGO 2.4.1, with the optimized models.**
Windows EGO 8.13 and its enhancement plugin were installed separately under
`C:/Users/UNAM/AppData/Local/Programs/DinamicaEGO-8.13`. The original engine was
retained. The official download source was
[Dinamica EGO 8](https://csr.ufmg.br/dinamica/dinamica-8/).

On the five small fixtures, optimized 2.4 versus optimized 8.13 preserved all
1,168 raster arrays/masks/grids/types and all 1,260 decoded sourcing binary64
values. Thirty-four reporting fields differed, by at most 1e-7, within the
reviewed rounding exceptions. Small-fixture wall times generally favored 2.4;
the active-Patcher case favored 8.13. Those timings were not isolated.

A separate controlled comparison used full MDG inputs copied from
`D:/MDG_1000m_bau1_2050_mc3_uncapped`: **1,601 × 813 = 1,301,613 cells**, LUC 3,
2000–2002, one frozen MC draw, two processors, native expressions. Engines ran
serially without other heavy agent work. The 8.13 run additionally disabled
parallel steps and parallel functors. The same four external R calls were
excluded from both runs.

| Measurement | Optimized 2.4 | Optimized 8.13 | Elapsed reduction |
| --- | ---: | ---: | ---: |
| Entire bounded run | 196.797 s | 148.422 s | 24.58% |
| Three annual iterations | 116 s | 90.621 s | 21.88% |

The old engine's inner timers round to whole seconds. These are single,
three-year measurements, not a 51-year runtime prediction. Both runs produced
the same 136 output paths. All **115 TIFFs** had exactly equal values, NoData,
grid, CRS and datatype.

However, **7 of 42 decoded sourcing scalars** differed by 1–2 binary64 ULPs,
with maximum absolute difference about 5.82e-11 and relative differences about
2–4e-16. They are harvesting normalization denominators. The pattern is
consistent with aggregate summation roundoff, not a proven change in model
equations. Nineteen CSV numeric fields differed overall; only two passed the
predeclared reporting exceptions. Stock-total reporting differences reached
4e-5 at totals around 2.7–2.9e9, despite identical stock rasters.

The user accepted rounded reporting values when speed gains are substantial.
This release still requires exact internal sourcing scalars: they feed
normalization, and the three-year result cannot certify exact future pixels
through 2050. Consequently, **the full-MDG strict parity gate failed and 8.13
was not selected for production**. Its installation and compatibility candidate
remain available for a separately qualified migration. The code optimizations
do not depend on that migration.

Temporary detailed evidence is in
`E:/MoFuSS_Active/mdg_windows_performance_2026-10-06/mdg_engine_benchmark`,
including `mdg_engine_benchmark_result.json`. Its failed parity result must not
be replaced with a pass merely because every tested raster matched.

## Production preparation

The models installed by this performance release included the validated
performance graph and the report option described below. Their SHA-256 values
at that installation were:

- v13: `6097bcfa7593dcd73896eb807320429680de2636767ce5efaadff6f15290ed1a`
- v14: `990bf8d6f0381826c765a10d5bb4fcb4a42f14ac09a599797812cf7cb6046d33`

The original `tools/build_windows_performance_models.py` reproduced those
models. The current builder additionally includes the separately approved
v14 annual eligibility correction described in the linked release below.
Its optional `--engine-8` applies the explicit Step carrier required by the
two nested sourcing filenames; it does not authorize an engine migration.

`tools/prepare_mdg_windows_performance.py` checks all eight MDG folders before
writing model/support code. It binds the successful five-case gate to the
tested model hashes and engine, verifies the final executable graph, checks
reviewed helper hashes, and preflights backup conflicts. Existing changed code
is backed up under `_code_backups/windows_performance_2026-10-06` in each run.
It records file hashes and configuration in `windows_performance_preparation.json`.
It neither runs simulations nor changes scientific inputs, Monte Carlo draws,
or existing scientific outputs.

Installation completed in all eight folders. A separate readback verified all
32 installed model/helper/launcher files, eight preparation manifests and eight
configured executable graphs. The final source passed 11 v14 static checks,
two I/O contract tests, table-transform idempotence/incremental/negative-input
checks and exact builder reproduction. The installation record is
`E:/MoFuSS_Active/mdg_windows_performance_2026-10-06/deployment_installed.json`;
each run also retains its own preparation manifest and relevant code backups.

| Drive | Model | LUC | BAU MC reruns | ICS MC reruns |
| --- | --- | ---: | --- | --- |
| F | Windows v13 | 1, fixed reference cover | Yes | No |
| D | Windows v14 | 3, annual cover | Yes | No |

Each of the capped/uncapped BAU and ICS folders receives
`RUN_MDG_optimized.cmd`, which explicitly selects the validated engine, uses
two processors, and creates its own temporary folder below
`E:/MoFuSS_Active/mdg_windows_reruns`. It does not force a new engine seed.
Use at most four concurrent simulations on this four-core machine. Start each
ICS only after its matching BAU has generated the **new** complete MC batch;
the BAU dynamics may still be running. A ready file left from an earlier run
does not establish that the new batch has been generated.

The reviewed `rnorm_v8.R` and `bypassMC_v8.R` updates add sourcing directories
and deterministic table exports to the older F copies. For LUC 1, sampling
distributions and draw order are unchanged. `MOFUSS_SEED` was unset at process,
user and machine scope. Unseeded BAU reruns intentionally draw a fresh batch;
equality to historical BAU draws is not claimed.

Report generation defaults to **Yes**, retaining the full output bundle.
The optional wizard control states that **No also skips period-level fNRB
summary tables/vectors**, as well as maps, animation and report rendering.
It retains annual Dinamica rasters, engine Temp tables and sourcing captures.
Keep Yes when the standard summary products are needed.

For a 51-year, three-MC run, the shared diagnostic writes fall from 2,601 to
867. Static uncapped-growth map loads fall from 459 to 9. All final shared
diagnostics, per-MC outputs and sourcing captures remain present.

The separate [annual sourcing-cache correction](woodman_annual_sourcing_cache_candidate.md)
was subsequently approved by the user. It changes affected D results and is
validated separately against annual recomputation. See the
[annual eligibility release](woodman_v14_annual_eligibility_release.md) for its
current source identity, checks and deployment status. The exact-output and
timing evidence in this document belongs to the earlier performance release.
