# Biomass reporting support

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

The v14 source now prevents initially missing **model stock** from becoming
finite after a LUC transition. Existing D outputs predate that change and must
be regenerated with the updated v14 before producing the final comparison.
Preserving v13 initialization means the F simulations need no rerun for this
change. Their biomass reports do need rebuilding.

After the D simulation rerun, rebuild Stages 1–5 for the desired enabled
batches using `0post_emissions_pipeline_v2.R`. Resuming only at Stage 3 with old
Stage 2 results is rejected deliberately. Existing run folders and reports are
not rewritten by changing the source files or running regression fixtures.

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
- Windows model tests and their precise scope are documented in
  `tests/woodman_v14_runtime_regression.md` (paths relative to `localhost/scripts`).

These tests passed on 6 October 2026. Read-only checks of all eight D/F run
folders also confirmed exact preservation of supported input values. No
production simulation, emissions batch or manuscript rendering was run as part
of this source change. Temporary evidence is under named `E:/MoFuSS_Active`
folders; the rules and regression code above are preserved in the repository.
