# Windows v14 annual harvesting eligibility

**Status, 2026-10-06:** approved correction installed in the four D MDG folders.
All four F folders remain on the verified v13 release. Final readback passed
for 32 runtime files, eight configured graphs and preparation manifests, and
the four D code backups. No production simulations were started.

## Approved change

The user approved applying the current year's land-cover eligibility after
the cached distance/NPA calculation. This corrects the previous TOF-to-forest
exclusion lasting until a decadal cache refresh. Current land cover now masks
the cached float32 NPA base every year. The same correction handles landscape
exit and reentry. It can change allocation and subsequent biomass in D runs.

Initial model-stock eligibility, numeric zero versus NoData, the calibrated K
on return to the original class, conversion-year restrictions, growth equations,
harvest thresholds, demand, Patcher and MC sampling are retained. The accepted
uncapped growth-parameter coverage limitation remains unchanged.

The v14 LUC=1 branch also uses the existing static LUC/TOF inputs directly and
constructs zero transitions. It requires no year-labelled rasters. LUC=3 retains
annual input selection. F production remains on v13 as requested; the common
v14 fixed-input branch provides a verified option for future configurations.

## Source and runtime

- Canonical source: `C:/Users/UNAM/Documents/mofuss`.
- Engine: Windows Dinamica EGO 2.4.1; no EGO 8 migration.
- v13 SHA256 remains
  `6097bcfa7593dcd73896eb807320429680de2636767ce5efaadff6f15290ed1a`.
- Pre-correction v14 SHA256:
  `990bf8d6f0381826c765a10d5bb4fcb4a42f14ac09a599797812cf7cb6046d33`.
- Corrected canonical v14 SHA256:
  `8368bc719dc979753b91507816a1c3fcc66589f210eac4f8d8b2ef7133c3565b`.
- Temporary tests:
  `E:/MoFuSS_Active/mdg_v14_annual_eligibility_release_2026-10-06`.

Builders `build_woodman_dinamica_v14.py` and
`build_windows_performance_models.py` include the approved correction and true
fixed-input branch. `fix_woodman_annual_sourcing_cache.py` validates its explicit
contract marker and rejects incomplete or altered corrected graphs. It is
idempotent for a complete corrected graph.

## Sourcing format

Corrected runs write
`Sourcing/static/annual_domain_after_static_npa_cache_v1.csv` and unmasked
`W_npa_baseCCC_SS.tif` / `V_npa_baseCCC_SS.tif`. Annual eligibility captures
encode that year's domain, and each MC/year gets `accumulator_domainYY.tif`.
The older static accumulator writer is replaced by the explicit marker.

`2post_runtime_sourcing_v1.R` retains the older capture route for existing
v13/v14 runs. Corrected captures require the marker, complete new bases,
annual accumulator and the v13 origin-preserving W scalar schema. Mixed,
partial or incompatible captures fail rather than silently falling back.
Normalization and attribution arithmetic are unchanged. Fresh BAU/ICS
initialization recreates the Sourcing tree; this release does not move or erase
existing scientific outputs during code preparation.

## Verification procedure

`run_windows_annual_eligibility_regression.py` stages independent copies of the
73,437-cell cohort fixture, three years and three distinct frozen MC draws.
It removes the same four external R initialization/report calls from each
pair and uses native Windows execution with isolated temporary folders.
Temporary per-origin output observers never feed the scientific graph.

- Fixed capped/uncapped: compare corrected v14 with retained v13. Annual input
  files are absent. Core scientific outputs and all CSV/scalar files must match
  exactly. Only the previously approved diagnostic changes at initially missing
  model-stock cells are allowed, using an explicit file/value allowlist.
- Dynamic capped/uncapped and active-Patcher: compare corrected v14 with an
  independent reference made from the previous v14. That reference executes
  its original NPA and current landscape-mask stages on every year, bypassing
  cache reuse. All comparable scientific outputs must match exactly. Controlled
  channel-1 rasters/category tables are copied to channel-3 filenames in both
  fixtures so the selected class semantics are identical.
- Changed static capture filenames and annual accumulator files are checked by
  the updated reader. Its reconstructed per-origin W/V pressure, W TOF
  redistribution and accumulated W/V pressure must equal directly recorded
  engine observer rasters, including NoData masks.

These are bounded integration tests, not a full MDG 2000–2050 rerun. The earlier
[cache-fragment tests](woodman_annual_sourcing_cache_candidate.md) additionally
cover steps 1, 2, 3, 11 and 12 across cold/warm caches and two IDW snapshots.

The older performance/full-model drivers explicitly reject corrected v14
inputs when their staging or capture assumptions no longer apply; their helper
functions and historical-model validation remain available. Use the new
annual-eligibility driver for this release.

## Completed results

All five native Windows comparison pairs passed: fixed capped/uncapped,
changing-cover capped/uncapped, and changing-cover capped with active Patcher.
The three changing-cover cases matched annual recomputation in **1,035/1,035
comparable files byte for byte**. Fixed-cover core outputs and every CSV/scalar
were exact; 120 auxiliary rasters had only the explicitly allowed differences
at initially excluded cells (57 capped, 63 uncapped). No initially eligible
cell differences were accepted. This is not an all-file identity claim between
v13 and v14.

The sourcing reader reproduced **360 full-grid vectors exactly**, including
NoData masks: 180 per-origin pressure, 90 W TOF redistribution and 90 aggregate
pressure vectors, covering 26,437,320 cell comparisons. Legacy compatibility,
new-format routing and rejection of incomplete/mixed captures also passed.
Reader SHA256:
`0f60e9fca218f2c95d2b2a7cf4ba5c13167b1c721ceab166583cb1cfd70cc487`.

Every candidate exercised 32 warm cache loads; the active-Patcher case executed
36 Patcher calls. The three-year TOF-to-forest cohorts remained below harvest
thresholds after conversion, so positive post-conversion pressure is established
by the earlier targeted native probes, not by those short full-graph cohorts.
One native DLL-lock failure occurred before scientific outputs and is preserved
in scratch; a fresh native retry succeeded. See the
[detailed integration report](windows_annual_eligibility_regression.md).

After updating the canonical model, 13 v14 static tests, four cache-contract
tests, two I/O tests and table-transform idempotence/incremental/altered-graph
checks passed. The final builder exactly reproduced both release models.

## Preparing the eight MDG folders

`tools/prepare_mdg_annual_eligibility.py` requires successful hash-bound
integration and reader gates. It verifies all eight previous installations,
checks every D annual input for 2000–2050, and preflights all code backups before
writing. It updates only the four D model files and preparation manifests.
F code remains unchanged. Existing D code/manifests are retained under
`_code_backups/windows_annual_eligibility_2026-10-06`.

Installation and independent readback completed successfully. Temporary
`deployment_installed.json` and `deployment_readback.json` are under the task
scratch folder above; each D run retains its own updated preparation manifest
and code backups. All F model/helper/launcher files and preparation manifests
were verified unchanged. Existing production inputs and scientific outputs
were not altered by preparation.

| Drive | Model | LUC | BAU MC reruns | ICS MC reruns |
| --- | --- | ---: | --- | --- |
| F | v13 | 1 | Yes | No |
| D | corrected v14 | 3 | Yes | No |

Use the Windows 2.4.1 wizard or each run's `RUN_MDG_optimized.cmd`. The command
launcher selects the existing 2.4.1 engine, two processors and isolated temporary
storage. Reopen the updated EGOML from its run folder if an older copy is already
open in the wizard. Use at most four concurrent runs on this machine. Each ICS must wait
for its matching BAU's **new** complete `Temp/mc_batch_ready.csv`; a previous
run's ready file is insufficient. BAU dynamics may continue while ICS starts.

Report generation remains enabled. The optional report switch also controls
fNRB summary tables/vectors; leave it enabled for the complete standard bundle.
This preparation does not start production simulations.
