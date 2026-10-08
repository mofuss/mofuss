# Biomass reporting support

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
