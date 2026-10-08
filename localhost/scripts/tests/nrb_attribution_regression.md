# Woodfuel attribution of NRB and fNRB

The corrected v14 model uses the contract
`woodfuel_attributed_signed_balance_v1`. Prescribed land-cover changes still
change the simulated landscape, carrying capacity, regrowth and harvest
availability. Their direct stock resets and downward preharvest stock clamps
are excluded from woodfuel NRB. Effects on subsequent woodfuel supply remain
part of the simulation.

## Engine accounting

For a cell and year, let S be stock after the prescribed LUC reset, B stock
after growth/carrying-capacity adjustment but before harvest, H realized
harvest, P stock after harvest and E the end-of-step stock including the
existing forest seed. The annual diagnostic is:

```
annual_NRB = max(0, min(H, min(S, B) - P))
annual_fNRB = annual_NRB / H
```

The production implementation also preserves initial model support, the
nondegradable TOF rule and the legacy deforestation exclusion. It evaluates
zero harvest before division. Engine rasters retain the historical zero
fraction for zero harvest; presentation tables use NA for an undefined ratio.

A signed balance preserves regrowth offsets across years:

```
Cpost[t] = Cend[t-1] + min(S[t], B[t]) - P[t]
Cend[t]  = Cpost[t] - (E[t] - P[t])
Cend[0]  = 0
```

Only `Cpost` is exported, as `debugging_<MC>/Woodfuel_balanceNN.tif`.
Temporary missing-state years with zero harvest carry the previous balance.
Missing states with positive harvest remain invalid. The legacy deforestation
exclusion accumulator also carries its history across such domain gaps.
Terminal NRB clips Cend to [0, cumulative H], with the legacy exclusion gate.
The ledger uses float32, matching the production Windows 2.4.1 engine and
stock rasters; R readers perform their period arithmetic in double precision.

The supported production graph hard-wires `DEF_FW=0`. Positive legacy
deforestation gates are retained for historical compatibility and tested in
isolation; the retired `DEF_FW=1` branch is not a supported configuration.
The shared period reader does not reproduce that retired branch's exclusion
semantics. Conversion codes 1, 2 and 4 force preharvest stock and harvest to
zero in both growth modes; their postharvest and endpoint stocks are therefore
both zero, so endpoint conversion cannot enter the signed balance.

This is an accounting observer: it does not feed growth, stocks, harvest
allocation, sourcing or random draws. Summing clipped annual NRB is **not**
equivalent to period NRB, because later regrowth can offset earlier depletion.

## Period boundaries and consumers

`helpers/woodfuel_nrb_attribution.R` is shared by the map/table readers and
emissions Stage 1. For a preharvest first-year baseline it computes:

```
NRB[a:b] = clip(Cpost[b] - Cpost[a] + B[a] - P[a], 0, sum(H[a:b]))
```

For an explicit emissions window with an end-of-previous-year baseline:

```
NRB[a:b] = clip(Cpost[b] - Cpost[a-1], 0, sum(H[a:b]))
```

The latter uses zero initial balance if a=1. Reporting metadata records the
actual included years. Internal decade bins retain their half-open convention;
the terminal bin includes the final simulated year, including 2050. Numerator
and denominator use the same years. Undefined ratios and wholly unsupported
zones stay missing, while supported zero NRB stays zero.

Readers verify the active LUC selector using runtime/preparation provenance,
never the mere presence of annual files. Annual-LUC output without the
corrected model contract and complete ledger is rejected. Verified static
legacy output has an explicitly named stock-difference fallback. Use corrected
v14 for both LUC1 and LUC3 in new Windows comparisons to give both scenarios
the same accounting mechanics. Retained v13 files support historical replay.
The corrected Windows v14 workflow supports LUC1 and LUC3. Copernicus LUC2
and Linux engine replay still select legacy v13; updated NRB reporting rejects
LUC2, and corrected v14 Linux execution has not been validated.

## Emissions meaning

Stages 2–5 use the change in the ICS-minus-BAU stock gap, plus their separate
end-use emissions calculation. This measures net retained biomass carbon
under the prescribed LUC history, not harvest-attributed NRB. A common LUC
trajectory can remove different amounts of carbon from two different stocks.
Those stock interactions are therefore retained in the net carbon result.
The explicit contract is `net_retained_stock_under_prescribed_luc_v1`.

The reference-relative decomposition is labeled stock-loss and stock-gain
components. It is not a process attribution of every difference to harvest or
regrowth. Historical machine-readable field names remain compatibility aliases.
Downstream stages reject incompatible or absent attribution metadata.

## Regression coverage

- `test_nrb_attribution_graph.py`: generator identity, idempotence, fail-on-drift
  checks and whole-graph proof that only accounting observers change.
- `test_nrb_attribution_runtime.py`: exact production nodes and native feedback
  on 17 three-year cases, including pure LUC clearing, K reduction, harvesting,
  signed regrowth recovery, seed offsets, TOF, zero/NoData support and domain
  reentry. Windows 2.4.1 passed 238 numeric/mask checks with zero difference
  against the independent reference at float32 storage boundaries.
- `run_nrb_attribution_full_regression.py`: five complete frozen-input fixtures
  (fixed and annual LUC, capped and uncapped, plus annual active Patcher), three
  years and three MC draws each. Every non-NRB physical/sourcing output must
  remain byte-identical; new annual ledger files are required. Its separately
  callable output invariant checker examines every grid cell for all three
  MCs: immutable support and domain-gap preservation, terminal NRB bounds,
  zero-harvest behavior and fNRB ratios. Shared annual NRB/fNRB rasters are
  checked only after proving that their writers export the final MC's direct
  accounting nodes. `--check-invariants-only` runs these checks without a
  simulation and writes its evidence only in the supplied scratch fixture.
- `test_reporting_woodfuel_attribution.R`: actual reporting partition block,
  administrative/ecoregion CSV and GeoPackage products, single-MC figures,
  signed period accounting, zero/NA handling and final-year inclusion.
- `test_stage1_woodfuel_attribution.R`: Stage 1 execution, both temporal
  baselines, pure clearing, clearing plus harvest, regrowth, model-bundle
  provenance and missing-ledger rejection.
- The biomass-support, initial-support, harvest-projection, relocation,
  manuscript-inventory and global-aggregation tests cover downstream arithmetic
  and provenance. The old `test_postprocessing_manuscript_uncertainty_threshold.R`
  targets a removed Stage 4 v1 script; it is not a current pipeline acceptance
  test and is not claimed as passing.

Temporary evidence belongs under
`E:/MoFuSS_Active/MDG_NRB_attribution_fix_2026-10-07`, outside this repository.
The tests do not constitute a new 51-year Madagascar production run. Updating
source does not repair already-written D:/F: outputs: generate fresh corrected
runs and rebuild reporting before treating their NRB comparison as final.

## Completed verification, 7 October 2026

The five-case replay in `integration_03` passed on Windows Dinamica 2.4.1.
All **1,563 physical/sourcing output files were byte-identical** to their
frozen pre-correction counterparts: 309 fixed capped, 318 fixed uncapped,
309 annual capped, 318 annual uncapped and 309 annual capped with Patcher.
The only excluded comparisons were NRB-dependent diagnostics and the two
output names of the repaired legacy exclusion-history accumulator. Each case
also produced all nine new annual ledger maps.

Full-grid accounting checks passed for **270 rasters and 12,992,793 cell
invariants**, with zero fNRB ratio discrepancy. Each of the three realizations
in each fixture contained 40,300 initially supported and 33,137 excluded
cells. No positive-harvest/missing-stock invalid state occurred in these
fixtures. The graph tests passed six attribution checks and thirteen existing
v14 structure/mechanics checks; the independent native 17-case test passed all
238 numeric/mask comparisons.

Reporting validation passed through CSV, GeoPackage, TIFF and native PDF
generation on the synthetic dynamic fixture. The actual completed F: Madagascar
MC1 inputs also passed all administrative/ecoregion partition exports with
every generated output confined to E: scratch. Stage 1 dynamic and static
support, Stage 2/3 accounting, Stage 3/4 table handoff and attribution guards,
Stage 4 inventory, Stage 5 aggregation, relocated manifests and delayed helper
loading all passed. The historical v1 uncertainty-test limitation above remains.

The first scratch replay attempt was interrupted during review; the next
attempt failed because the new fixture driver had not created debug output
directories. Those attempts are retained separately. The successful evidence
uses the corrected fixture setup and native expressions throughout.
