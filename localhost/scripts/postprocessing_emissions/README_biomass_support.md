# Biomass reporting support

## Woodman freeze year, 9 October 2026

V14 accepts `woodman_luc_freeze_year`, an integer from 2000 through 2050,
defaulting to 2050 when the parameter is absent. It affects LUC mode 3 only.
The annual Woodman history, including transitions, applies through that year
inclusive. Later steps reuse the freeze year's land-cover and TOF maps and
apply zero new transition codes. The freeze-year clearing/new-forest event
must not be replayed every subsequent year. Model time, demand, growth,
harvest, biomass stocks and signed woodfuel accounting continue normally.

For the matched Madagascar experiment, F uses Woodman LUC3 frozen at 2026 and
D uses Woodman LUC3 through 2050. Both paths retain the same prescribed history
through 2026. This differs from the earlier fixed-MODIS versus annual-Woodman
comparison; retain its dated outputs as historical evidence.

Each MC realization writes `debugging_<MC>/woodman_luc_execution.csv` from
the actual native model inputs. It is a `Key*, Value` lookup table: 1 is the
active LUC selector, 2 the freeze year, 3 the freeze-contract version (1), and
4 the model start year. The model declares
`mofuss.woodman.freeze.contract=woodman_freeze_year_v1`. Updating a model or
parameter CSV does not establish how old physical outputs were generated:
the shared NRB and process readers require this executed evidence for the new
contract, reject conflicting ready-batch metadata, and reject different
freeze years within a Woodman BAU/ICS pair. Legacy models without the freeze
contract retain their historical annual-input interpretation through 2050.

The MC readiness and bypass manifests also record the configured freeze year;
old manifests lacking it imply the historical 2050 default. Changing the
freeze year does not consume random draws. The ICS bypass validates matching
BAU/ICS freeze settings for LUC3 before reusing BAU tables.
Both preparation scripts compare the source parameter, prepared runtime
parameter and actual Dinamica `WoodmanFreezeYear` argument before removing
old outputs; inconsistent LUC3 settings fail at that preflight.

Physical signed-ledger reconstruction and process attribution use the same
effective cover year and transition suppression as the model. Output stock
and ledger filenames still follow actual calendar steps. Stage 1 provenance
and Stage 3 summary/annual/JSON products record the freeze setting and its
evidence. Annual process tables distinguish actual `year` from
`land_cover_year` and record whether Woodman transitions are enabled.

Reporting windows and checkpoints are unchanged. In particular, explicit
2026–2050 accounts use end-2025 through end-2050; opening saved biomass carries
into each period. LUC losses remain separate from woodfuel NRB/fNRB and are
already included in the net retained-stock benefit, so no second deduction is
made. `test_woodfuel_luc_decomposition.R` covers the freeze-year reset, later
regrowth/harvest, absent unused annual maps, period closure, NRB reconstruction,
and missing/conflicting execution evidence. `test_bypassMC_v2.R` covers default
compatibility and freeze-year pairing.

## Signed-ledger storage and recovery, 8 October 2026

The first corrected v14 exports used `-9999` as NoData in a signed balance.
A legitimate negative balance can equal that value, and GDAL can also mask
nearby float32 values. An exact collision can propagate missing accounting
values into later years. The observer does not feed the physical simulation:
saved growth, postharvest stock and harvest remain usable.

The canonical model now uses `-1e30` for the initial, postharvest and end-step
signed-ledger nodes. This changes storage only, with the attribution equations
and physical model unchanged. Completed simulations are not rewritten.

For existing exports, the shared NRB reader reconstructs signed **period
increments** from saved annual physical states. It includes the previous
step's seed correction, preserves the observer's zero-harvest carry rule,
and rejects unexplained disagreements with readable ledger increments.
The reconstruction is also used by the Stage 3 process account. This avoids
interpolating across missing cumulative values or silently dropping cells
whose balances collided with NoData. It does not recover genuinely missing
physical states by substituting zero.

The reader checks both model provenance and the actual TIFF NoData encoding;
replacing a model file does not repair earlier exports. Stage 1 records the
read policy, helper hash and auxiliary input hashes in its manifest, and
preflights reconstruction inputs before clearing its exact output directory.
Process outputs report recovered ledger coverage, validation error, annual
support gaps and numerical residuals separately. The long-form
`Temp/mc_k_NN.csv` table is indexed directly by its land-cover `Key`; the
engine's extra column offset applies only to its wide table with a leading
Monte Carlo ID column.

The reporting mask remains the finite original AGB reference. Shared model
reports reconstruct on the wider finite initial model-stock domain first,
and Stage 1 applies its reporting mask afterward. Tests and native engine
evidence are described in `tests/woodman_v14_runtime_regression.md`.

## Woodfuel savings and land-cover reversal, 8 October 2026

For corrected v14 runs, Stage 3 adds a process account beside the existing
reference-relative stock-loss/stock-gain decomposition. It reads the saved
annual model states and signed woodfuel ledger through
`helpers/woodfuel_luc_decomposition.R`; it does not rerun the simulation.
The new products are `process_attribution_per_run_*.csv`,
`process_attribution_annual_*.csv`, and per-pair/run diagnostics under
`process_attribution/`. Stage 4 publishes an MC1 process table and figure,
with a separate `process_attribution_policy.csv` describing the account.

The period identity is:

```
net retained stock benefit = signed woodfuel effect
                          + direct LUC reset effect
                          + TOF allowance effect
                          + capacity adjustment effect
                          + unclassified support adjustment
                          + numerical closure residual
```

The net must reconcile to the unchanged Stage 2 result. Its LUC losses have
already reduced the final BAU/CCTS stock difference; **do not subtract them
again**. The signed woodfuel term retains biological growth offsets.
The companion `net_before_direct_luc_mg` adds back the signed direct LUC
reset effect to the net result. It describes an accounting exclusion under
the realized pathway, not a simulated landscape without land-cover change.
Separately reported clipped NRB saving is BAU period NRB minus CCTS period
NRB. Because NRB is bounded cellwise, it is not interchangeable with the
signed term in the stock identity and is not an additional emissions benefit.

Direct reset effects, annual TOF allowances, and downward capacity adjustments
are distinct components. The helper further diagnoses capacity changes and
domain coverage. Where annual states or ledgers cannot identify a process,
the unresolved stock contribution remains visible as a support diagnostic;
it is not silently assigned to LUC or woodfuel. The fixed original-reference
footprint and complete Stage 2 endpoint result remain the reporting baseline.

The primary reversal percentage is **event conditional**: positive CCTS-minus-
BAU stock removed at direct LUC reset events divided by positive saved stock
exposed immediately before those same events. The denominator is summed over
events, so repeated exposures count separately. Zero exposure produces an
undefined value, not zero percent. This is not a claim that individual tonnes
have been tracked since creation, nor a percentage of the signed NRB saving.
Period opening stock, signed process gains and losses, and the denominator
are supplied explicitly for interpretation. Carbon-equivalent conversion uses
the existing `0.47 * 44/12` factor; end-use accounting remains separate.

Legacy runs without corrected ledger provenance keep their stock-based
products and report process attribution as unavailable. Corrected runs with
missing required annual files fail process preflight before Stage 3 clears
its output directory. Annual pixel-level coverage gaps are reported explicitly.

`test_process_attribution_reporting.R` verifies the Stage 3/4 handoff,
conservation and no double subtraction, the separate NRB metric, legacy
availability, and the event denominator. Existing Stage 3/4 stock-estimand
tests remain applicable.

## NRB attribution and the emissions estimand, 7 October 2026

Stage 1 now uses `helpers/woodfuel_nrb_attribution.R`, the same NRB reader as
the model's maps and administrative reports. For corrected runs,
`debugging_<MC>/Woodfuel_balanceNN.tif` stores the signed cumulative depletion
at the postharvest checkpoint. External stock changes caused by LUC and
preharvest carrying-capacity reductions never enter that balance. Positive
growth, including restoration at the end of a model step, offsets depletion;
the balance stays signed until the requested reporting period is selected.

For the historical default windows, the baseline is the START-year preharvest
stock. The signed period change is
`Cpost[END] - Cpost[START] + Growth[START] - Growth_less_harv[START]`.
For explicit `--period=START:END` windows, the baseline is the previous year's
postharvest checkpoint, and the change is `Cpost[END] - Cpost[START-1]`.
The former explicit-window implementation incorrectly read `Growth[START-1]`
while describing it as an end-of-year baseline. The corrected implementation
uses the documented checkpoint. NRB is bounded to zero through period harvest;
fNRB is NRB divided by that same harvest, and is undefined where harvest is zero.
Period NRB is not a sum of annual positive NRB, because later growth can restore
previous depletion.

An annual-LUC run without a complete ledger and the corrected model attribution
contract is rejected before Stage 1 outputs are cleaned or written. A partial
ledger is also rejected. Verified legacy static-LUC runs can still use the
matching stock endpoints, with method `legacy_fixed_luc_stock_difference`
recorded explicitly in the Stage 1 manifest. Unknown or conflicting LUC
provenance is rejected rather than assumed static. This compatibility path does
not retrofit the corrected model's process ledger into historical simulations.

Stages 2–5 do **not** use Stage 1 NRB or fNRB to calculate biomass mitigation.
They retain their existing numerical estimand: the period change in the ICS
minus BAU stock gap, converted by `0.47 * 44/12`. This is net retained biomass
carbon under the prescribed LUC pathway. The same prescribed clearing can
remove different amounts of biomass from BAU and ICS, so these interactions
are part of the net stock benefit. It is not a harvest-only NRB flux. End-use
emissions remain demand based.

Stages 2–5 record and validate
`biomass_estimand=net_retained_stock_under_prescribed_luc_v1`. Historical
`harvest` field names and raster paths remain for compatibility; manuscript
labels identify them as **Net biomass carbon**. Stage 3's split relative to
initial stock is labeled **Stock-loss component** and **Stock-gain component**
in manuscript products. That algebraic split is not a process attribution to
harvest or biological regrowth. Do not mix an NRB-derived emissions series into
these tables under the existing estimand. Rebuild Stages 2–5 to obtain the
explicit provenance and updated labels; valid stock-based numerical results
are preserved.

Regression coverage includes `test_stage1_woodfuel_attribution.R` (clearing
without harvest, clearing plus harvest, signed growth recovery, both period
baselines, and missing-ledger rejection), `test_stage1_initial_biomass_support.R`,
`test_postprocessing_biomass_support.R`, and
`test_postprocessing_global_incremental.R`. Fixtures stay outside the source
repository and canonical run/output directories.

## Agreed rule, 6 October 2026

Stages 1–5 use policy `finite_initial_agb_reference_v1`. Biomass reporting is
restricted to finite values in the original country reference
`LULCC/TempRaster/agb3_c.tif`. Numeric zero is valid. NoData is excluded, even
when a later land-cover transition produces finite simulated biomass there.
Existing endpoint validity requirements also remain in force. Missing growth
parameters are not filled, and excluded biomass is not replaced with zero.

This policy applies to both the F fixed-LUC analyses and D annual-LUC analyses.
Their original reference rasters were verified byte-identical. It is a common
eligibility rule; it does not imply that their different LUC products or later
growth-parameter coverage have identical valid domains.

The model and reporting domains serve different purposes:

| Domain | Definition |
|---|---|
| v14 simulation | Finite initial **model stock**, initialized exactly as in v13, including TOF allowances independent of raw CTrees coverage |
| Biomass reporting | Finite original **AGB reference**, intersected with the valid stocks needed for each calculation |

An initially valid TOF cell therefore remains available to the simulation even
if its original CTrees reference is missing. Its biomass contribution is omitted
from reporting under this policy. End-use demand emissions retain their
existing demand-based calculation. The full-model harvest-versus-demand
diagnostic remains unmasked and explicitly labels that different scope.

## Implementation

- Stage 1 applies the reference mask before computing MC summaries of NRB,
  source harvest and AGB snapshots.
- Stage 2 requires matching BAU/ICS references for every accounting period,
  masks biomass stock differences before country aggregation and projection,
  and retains signed excluded differences as diagnostics.
- Stage 3 uses exactly the same reporting support. Avoided loss plus regrowth
  must still equal Stage 2 biomass benefits, nationally and by country.
- Stages 3–5 reject missing or incompatible support-policy metadata. Reference
  hashes must match within an analysis. Different analyses can have different
  country reference rasters.
- Stage 4 writes `biomass_support_policy.csv`; Stage 5 verifies it before using
  manuscript rasters alongside Stage 3 country tables.

Excluded benefits are recorded as diagnostics, not as a third reported
mitigation component. They can be signed; neither positive nor negative
excluded differences are added to the supported totals.

## Updating existing results

The initial-support correction described below was implemented on 6 October.
The newly completed D/F Madagascar comparison already includes that correction;
it does not by itself require those completed runs to be repeated.

The current **NRB attribution** correction adds an observer ledger to v14 and
requires new simulations to create it. For a paired Madagascar comparison with
the same corrected NRB mechanics, run both static LUC1 and annual LUC3 using the
corrected v14 in new run directories. The legacy fixed-LUC fallback is available
for historical inspection, but is not a replacement for that paired rerun.

Then rebuild Stages 1–5 for the desired enabled batches using
`0post_emissions_pipeline_v2.R`. Resuming at Stage 3 with old Stage 2 metadata is
rejected deliberately. Existing run folders and reports are not rewritten by
changing the source files or running regression fixtures.

## Verification

- `tests/test_stage1_initial_biomass_support.R`: native terra integration of
  the Stage 1 execution path, MC summaries, zero retention, NA/Inf exclusion,
  geometry and changed-reference rejection.
- `tests/test_postprocessing_biomass_support.R`: Stage 2 support calculation
  and actual Stage 3 processing with two countries, signed exclusions,
  retained zeros, later CR gaps, component/country reconciliation, unchanged
  end use, and policy/reference rejection.
- `tests/test_postprocessing_global_incremental.R`: global accounting and
  rejection of missing, old or inconsistent reporting provenance.
- `tests/test_stage4_attribution_contract.R`: executes the current Stage 3/4
  table handoff and checks that edited values, incompatible estimands and
  missing country/regional attribution metadata are rejected.
- `tests/test_stage1_helper_loading.R`: both `source()` and `sys.source()` can
  load configuration first and resolve the helper later from an arbitrary
  working directory, after source frames close and without `--file` metadata.
- `tests/test_postprocessing_manuscript_inventory_v3.R`: current Stage 4 output
  inventory. The older `test_postprocessing_manuscript_uncertainty_threshold.R`
  still targets the removed Stage 4 v1 and its pre-country-output fixture. It is
  a pre-existing obsolete test, not current Stage 4 validation, and was not
  migrated as part of this NRB correction.
- Windows model tests and their precise scope are documented in
  `tests/woodman_v14_runtime_regression.md` (paths relative to `localhost/scripts`).

The original support tests passed on 6 October 2026; the NRB and estimand
extensions were verified on 7 October. The earlier read-only checks of all eight
D/F run folders confirmed exact preservation of supported input values. No
production simulation, emissions batch or manuscript rendering was run as part
of these postprocessing source changes. Temporary evidence is under named
`E:/MoFuSS_Active` folders; the rules and regression code above are preserved in
the repository.
