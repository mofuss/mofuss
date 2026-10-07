# Windows v13/v14 mechanics regression

These tests read the canonical Windows EGOML files and write only to a new,
explicitly named folder under `E:/MoFuSS_Active`. They do not launch R preparation
in a production run or modify existing D:/F: inputs or results.

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
