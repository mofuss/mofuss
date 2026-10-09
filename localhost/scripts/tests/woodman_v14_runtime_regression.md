# Windows v13/v14 mechanics regression

These tests read the canonical Windows EGOML files and write only to a new,
explicitly named folder under `E:/MoFuSS_Active`. They do not launch R preparation
in a production run or modify existing D:/F: inputs or results.

## Freeze-year container dependency repair, 2026-10-09

The initial freeze-year reader introduced a dependency cycle between root
Groups 1610 and 2457. Moving `v94000`, `v94001`, and `v94002` beside their
parameter table `v267` in Group 2457 removes the cycle without changing any
equations or scenario settings. The patcher also repairs already-patched graphs,
and its static validator now rejects cycles between root containers.

`test_woodman_freeze_full_graph_startup.py` preserves the complete production
graph and checks native scheduling and R startup with isolated two-cell inputs
and harmless R stubs. All eight combinations of freeze year 2026/2050,
BAU/ICS, and capped/uncapped passed with native compilation enabled. An additional
broken-graph negative control reproduced the reported loop. These are startup
checks, not full simulations; the stubs stop before MC preparation or annual
outputs. Native-expression fallback warnings also occur in successful startup
checks and are separate from the dependency-cycle error.

The repaired source and all eight MDG copies were verified; all 54,016 existing
scientific output files retained their paths, sizes, and modification times.
Permanent evidence is in
`D:/mofuss_postprocessing/MDG_Woodman_freeze_setup_2026-10-09/loop_repair/`.

## Signed-ledger NoData correction and recovery, 2026-10-08

The default float32 NoData value, `-9999`, is a valid value of the signed
woodfuel balance. A completed Madagascar uncapped MC1 cell reached exactly
`-9999` in 2048; its observer remained NoData thereafter while its physical
biomass continued growing. Values close to `-9999` can also be hidden by a
raster reader's approximate NoData comparison. This is an observer-storage
defect, not a loss of simulated biomass.

The canonical v14 correction changes **only three `nullValue` literals** from
`.default` to `-1e30`: `v93000` (initial signed balance), `v93002` (postharvest
balance), and `v93003` (balance after the endpoint seed). The native engine
stores this as float32 `-1.0000000150474662e30`, far outside plausible biomass
balances. Equations, cell type, initialization, growth, harvest, sourcing and
physical stock feedback are unchanged. The accounting contract remains
`woodfuel_attributed_signed_balance_v1`; this is an encoding repair within
that contract. Safe canonical v14 SHA256:
`c7f86f4746f2f394d036d349d0f7ffb19de89d68cd592789e21980162229ff3d`.

`tools/fix_woodman_nrb_attribution.py` validates the complete existing observer
graph before migrating those known legacy literals. It rejects unreviewed
NoData values and altered equations. Applying it again to the safe graph is
byte-preserving. Newly generated v14 models use the safe sentinel immediately.

```powershell
python -B localhost/scripts/tests/test_nrb_attribution_graph.py
python -B localhost/scripts/tests/test_nrb_attribution_runtime.py --scratch E:/MoFuSS_Active/my_v14_check/sentinel
```

Verification on the installed native Windows Dinamica engine:

- Nine structural tests passed, including generation equality, migration,
  idempotence, rejection of altered graphs, and absence of observer feedback
  into physical mechanics. Migrating the existing canonical model changes
  exactly the three literals identified above.
- The native fixture passed **21 cases over three years, 294 numeric checks,
  maximum absolute error zero**. It includes exact `-9999`, both nearby
  float32 values, an endpoint seed that makes Cend exactly `-9999`, subsequent
  feedback, true initial NoData, invalid-state gaps and ordinary NRB cases.
- An otherwise identical legacy-sentinel control reproduces failures in all
  four collision cases. All **15 physical S/B/H/P/E raster outputs** remain
  byte-identical between the safe and legacy controls. This is a bounded
  observer fixture plus full-graph structural isolation, not a new full-country
  simulation or a claim that previously damaged observer outputs were correct.

Native evidence is temporary under
`E:/MoFuSS_Active/MDG_LUC_attribution_postprocessing_2026-10-08/tests_sentinel/`:
`report.json`, `checks.csv`, the exact source snapshot, both native graphs and
their engine logs. Test code and the conclusions above are retained here.

The eight completed D:/F: runs and their executed model copies remain intact.
Their postprocessing reconstructs **period increments** from saved physical
states, using `min(post-LUC start, preharvest stock) - postharvest stock` and
the preceding valid endpoint-seed correction. It does not substitute a raw
AGB loss for NRB, interpolate damaged cumulative balances, or rewrite the
completed rasters. True positive-harvest gaps remain un-attributable; known
zero-harvest domain gaps retain the observer's carry rule. A seed is credited
only when the preceding complete observer state was valid.

Independent Python scalar calculations, without the R recovery helper or
saved cumulative balance as the period estimator, verified four real uncapped
MC1 collision cells over 2026-2050. Every cell had constant forest class, no
land-cover transition, positive stock and nondecreasing preharvest growth,
so the independent increment telescopes to previous minus current stock.
The R helper matched **48 scalar checks exactly**, including both reporting
baselines, BAU/ICS signed depletion, harvest and net savings. In particular,
the F: BAU cell at zero-based row 901, column 508 recovers C2050
`-10269.412109375`, while the old exported ledger is stuck at `-9999`.
Both scenarios' clipped period NRB is zero at these regrowing cells.
Evidence: `independent_collision_scalars.json` and
`independent_collision_helper_comparison.json` in the same named task folder.

Full-grid MC1 postprocessing pilots for D: capped and F: uncapped then passed
with complete process support (660,846 and 619,748 cells respectively).
Readable exported increments supplied 33,042,300 and 30,987,397 independent
scenario-pixel-year comparisons. Maximum discrepancies were respectively
`0.0009765625` and `0.0001220703125` Mg, consistent with float32 storage;
reconstructed per-pixel and aggregate accounting closure errors were zero.
The F: pilot recovered the exact-sentinel cell; nearby sentinel values were
readable in that R/terra build. Reader-dependent masking is why actual storage
encoding and physical reconstruction matter, not a fixed count of raster NAs.

The readable-increment gate uses
`8 * 2^-23 * max(1, abs(Ccurrent), abs(Cprevious), abs(Pcurrent), abs(Pprevious)) + 1e-6`
Mg, ignoring missing scale terms while retaining every finite ledger comparison.
Reconstruction works in double precision and preserves signed regrowth until
the requested period is selected and NRB is bounded by its harvest total.
These pilots are checks on the 2026-2050 MC1 period, not a substitute for
validating every selected MC draw and reporting period in the production run.

Production postprocessing subsequently passed all 12 BAU/ICS pair/draw
accounts (F/D, capped/uncapped, MC1-3) for 2026-2050. Every account has complete
process and reconstructed-ledger support on its reporting endpoints and zero
aggregate component-closure error. Stages 3 and 4 reconcile with unchanged
Stage 2 totals; Stage 5 was checked separately for each LUC product with
Madagascar-only partial coverage, without combining the two experiments.
Three draws are retained for diagnostics, not strong uncertainty inference.

A computer restart interrupted postprocessing only. All 4,896 expected annual
simulation rasters remained present, and all 96 final-step rasters were fully
readable. Ten finished process accounts were reused only after complete raster,
sum, annual-total, support, auxiliary-hash and source-version validation; the
two unfinished uncapped MC3 accounts were recalculated. The dated analysis
roots on D: and F: preserve interrupted outputs, executed source snapshots,
cache/readback evidence and the recovery scripts under `_restart_provenance`.

Final engineering fixtures also verify the nested lazy helper under both
`source()` and `sys.source()` from an arbitrary working directory, positive-
harvest gaps excluded from NRB-support counts, and the distinction between
initial model-stock support and the original-AGB reporting mask. These last
two diagnostic corrections change no Madagascar production totals or counts.
The final Stage 1 readback passed all eight scenarios and all 96 report rasters:
MC1-3, the 2026-2050 window, 2025/2050 snapshots, recovery provenance, original-
AGB masking and `0 <= mean NRB <= mean harvest` all passed. Executed helper
versions are preserved alongside the analysis outputs; the diagnostic-only
source revision is distinguishable by its recorded hash.

## Pixel equations

```powershell
python localhost/scripts/tests/test_woodman_dinamica_v14_runtime.py --scratch E:/MoFuSS_Active/my_v14_check/pixels
```

This executes exact expression nodes extracted from both production graphs in
the installed Windows Dinamica engine. It checks:

- Three years of equal numerical values and NULL masks under unchanged LUC:
  zero, positive and NULL model stock; forest and TOF; zero and positive demand.
- The actual capped logistic and uncapped Chapman–Richards branches separately.
- Equivalent sold-fuelwood eligibility and Patcher domains.
- Zero growth availability and harvest in transition years 1, 2 and 4 for cells
  with a valid initial model stock.
- Return to the original class restores its baseline calibrated K, including
  numerical zero. Initially missing stock and K remain missing in all classes.
- Initially missing model stock remains NULL through every transition code,
  including TOF gain and a deliberately supplied finite previous stock.
- Initially numeric zero remains eligible. Initially valid stock with missing
  CR parameters still becomes NULL in the uncapped branch; capped growth retains
  its existing behavior.

The stock inputs here are model states. Category lookup and initial raster
preparation are supplied as controlled inputs. The full-graph test below covers
those dependencies. `end` in the pixel CSV is the seeded `v98` feedback state;
it is distinct from the saved pre-seed `Growth_less_harv` stock.

## Full graph with explicit raw-AGB cohorts

The command below documents the earlier model contract. For v14 with the
approved annual sourcing-cache correction, use
`run_windows_annual_eligibility_regression.py`; the older full-model driver
rejects the corrected graph before staging. See the
[current release report](woodman_v14_annual_eligibility_release.md) for its
fixed-input branch and capture contract.

```powershell
python localhost/scripts/tests/run_woodman_dinamica_v14_full_regression.py --source E:/path/to/completed_small_fixture --scratch E:/MoFuSS_Active/my_v14_check/full --mode both --dynamic
```

Use a completed small fixture containing frozen MC tables, demand inputs and
both logistic and CTrees growth inputs. The test stages independent copies,
injects raw AGB zero/NULL/positive cells under forest and TOF, verifies initial
stock, and compares all common scientific TIFF/CSV hashes for three years and
one MC iteration. It selects the same LUC channel in both models and makes exact
annual copies of that baseline LUC/TOF pair, with zero transitions. Both copy
success and hashes are checked. Four legacy external R initialization/report
calls are removed identically by `dinamica_runtime_regression_v1.py`.

`--dynamic` additionally executes v14 with all four transition codes and domain
disappearance/reentry. Fourteen injected cohorts distinguish numeric zero,
positive stock, initially missing non-TOF stock, raw-AGB-missing TOF with a valid
sampled allowance, and initially valid stock with missing CR parameters. It
checks that initially missing non-TOF cells never recover, while initially valid
cells retain the established conversion and reentry rules. Frozen MC inputs, sourcing,
initialization and scientific graph execution remain active. The established
small fixture uses deterministic Patcher bypass; this is not a stochastic
Patcher-placement or full-country production rerun.

`--dynamic-only` runs only the transition/domain tests and makes no static-parity
claim. `--mode capped` or `--mode uncapped` narrows the configuration.

If this old Dinamica installation encounters compiler DLL file locks, use
`--disable-native-expressions`. This uses the documented **Windows engine
interpreter for both models**, with unchanged model expressions. The command is
recorded in each runtime result; compiled and interpreted results must be
reported separately. Source snapshots/hashes, input hashes, engine logs, cohort
coordinates and output comparisons are retained under the supplied scratch path.

## Initial model stock support rule

Annual eligibility is fixed by the finite support of model initialization
`v200`. This is not a raw-AGB mask: a TOF cell whose raw AGB is missing remains
eligible when the legacy sampled allowance supplies finite initial model stock.
NoData initial model stock stays excluded even after a later LUC or TOF change.
The rule does not exclude numeric zero or add a growth-parameter coverage mask.

The compiled pixel fixture explicitly retains zero harvest on excluded cells,
matching the existing harvest accounting, while biomass availability and stock
remain NULL. Full-graph byte equality under fixed LUC is reported separately.
The support-aware gate requires byte equality for growth, realized harvest,
stocks and all CSV files. Only enumerated auxiliary raster differences are
allowed, exclusively at initially missing cells: positive sourcing bases and
probabilities become zero, forest-state one becomes NULL, and zero cumulative,
projected-harvest and domain fields become NULL. Any change at an initially
eligible cell, any other value conversion or any unlisted file fails the gate.

## Fixed initial support verification on 2026-10-06

Windows source SHA256: unchanged v13
`a1d1f4d32cab12fa3ec9682db752180317c81e511114ce5a9ccf3c5e979ed28c`;
v14 `89adc11bfc650ba9b46d74eff6b8b4978697f739423689618d2ab74dda3a79c8`.

- Eleven static tests passed, including immutable support wiring and unchanged
  v13 initialization nodes.
- Compiled Windows pixel probes passed 38 cases over three years and both
  growth modes. Eighteen fixed-cover cases have exact v13/v14 values and NULL
  masks for all four checked states. Initially missing stock remains NULL through
  every transition; zero stays eligible; original calibrated K returns; missing
  CR inputs keep their previous uncapped behavior.
- All six bounded Windows interpreter graphs completed: paired v13/v14 fixed
  LUC in both modes and dynamic v14 in both modes. Fourteen injected cohorts
  passed initialization, stock, growth and realized-harvest checks, including
  missing non-TOF stock through loss/return, TOF gain/loss and disappearance/
  reentry. Raw-AGB-missing TOF retained its valid sampled initial allowance.
- Strict byte comparison reports **109/141 capped** and **110/144 uncapped**
  files identical. The 32 and 34 changed auxiliary rasters pass the explicit
  support-aware gate: every difference is confined to 21 initially missing
  model-stock cells; no eligible-cell value or NULL mask changes. Actual growth,
  realized harvest, stock outputs and every CSV remain byte-identical.

Strict whole-output byte parity is therefore **false** for this version; the
fixed-LUC scientific outcomes and the enumerated support contract pass. The
first full-harness invocation correctly returned a strict-parity failure; the
retained comparison JSONs and a subsequent support audit document its exact
cause. Strengthened cohort assertions were rerun against all six completed
graphs. The production graph was unchanged between those checks.

Evidence is temporary under
`E:/MoFuSS_Active/mdg_v14_initial_stock_mask_2026-10-06/`, in
`native_pixel_probes` and `full_interpreted`. The full fixtures remain 269 by
273 cells, three years, one MC draw, with deterministic Patcher bypass. These
checks do not constitute a production-country rerun or stochastic Patcher test.

## Earlier verification on 2026-10-06, before fixed initial support

The earlier tested Windows source hashes were v13
`a1d1f4d32cab12fa3ec9682db752180317c81e511114ce5a9ccf3c5e979ed28c`
and rebuilt v14
`03c4fa6a8dfb7ea829160bdde76325128976da0ff825c1806d192b1cb54a8006`.

- Compiled native pixel probes passed all fixed-cover comparisons, NULL masks,
  conversion checks, and baseline-K return checks.
- Paired full-graph Windows interpreter runs matched **141/141 capped** and
  **144/144 uncapped** scientific files byte for byte: 285 total, including
  231 TIFFs. Inputs explicitly contained raw AGB zero, NULL and positive cohorts
  under valid forest and TOF. Both runs used the same frozen inputs.
- Full-graph dynamic checks passed in both modes: conversion-year availability,
  harvest and post-harvest stock were zero for forest loss/return, including NULL
  baseline AGB/K; disappearance produced NULL stock and reentry produced finite
  stock as specified by the model's domain-entry rule.

That version allowed a missing initial stock to become finite after a recorded
conversion. Its transition results below are historical evidence, superseded by
the fixed initial support rule above.

The earlier full runs used the Windows interpreter after two native compilation attempts
encountered temporary DLL file locks before scientific output. The miniature
pixel probes completed with native expression compilation enabled. The full
fixture had 269 by 273 cells, three years, one MC iteration and deterministic
Patcher bypass. This verifies the stated bounded cases, not a corrected full
Madagascar rerun or stochastic Patcher placement.

Temporary detailed evidence is under
`E:/MoFuSS_Active/mdg_v13_v14_mechanics_audit_2026-10-06/runtime_probes/`, in
`final_native_pixel_probes` and `final_full_interpreted`. These are disposable
diagnostic outputs; the test code and conclusions above remain in this source
repository.
