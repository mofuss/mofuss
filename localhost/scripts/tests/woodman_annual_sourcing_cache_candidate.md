# Annual sourcing-cache correction candidate

**Historical candidate evidence, 2026-10-06.** The user subsequently approved
this correction. Its integration, updated sourcing reader, full-graph checks
and deployment status are recorded in the
[annual eligibility release](woodman_v14_annual_eligibility_release.md).
The original performance-only release did not include the correction.

## Mechanism and timing

Windows v14 applies the current landscape mask inside the cold-cache branch:
`v6000` masks the walking (W) base with `v90003`; `v6010` masks the vehicle (V)
base with the non-TOF domain `v90013`. That already-masked base is reused during
the remaining years of the same IDW snapshot. The annual domain can change
while the cached mask remains frozen.

For example, a cell classified as TOF when the V cache is built receives a
zero V base. If it later becomes forest, that cached zero can continue to
exclude it after its biomass becomes eligible, until a subsequent cache
refresh recomputes the base. The existing conversion-year stock/growth rules
still apply: this finding does **not** imply that the cell should supply wood
in its conversion year. Domain exit/reentry can also leave stale cached
participation and change normalization across other cells.

The candidate caches only the existing float32 NPA-adjusted base, then applies
the original landscape-mask expression every year, after the cold/warm cache
junction. New raw-base IDs are `v91000`/`v91010`; existing consumer IDs
`v362`/`v377` receive the annual masked bases. Initial model-stock support,
zero/NoData rules, biomass thresholds, Patcher, demand and growth/harvest
equations are retained. Allocation and subsequent stocks can nevertheless
change after domain transitions; this is a scientific correction requiring
separate adoption, not a mechanics-neutral speed improvement.

## Completed evidence

The Windows native probe executed **20 miniature fixtures**: fixed/changing
land cover × two MC draws × steps 1, 2, 3, 11 and 12, covering two IDW snapshots
and cold/warm caches. Eight-cell inputs include TOF→forest→TOF, domain
exit/reentry, initial NoData, numeric zero, NPA and eligibility masks.

| Check | Result |
|---|---:|
| Candidate versus full annual recomputation | **120/120 vectors exact**, including NoData |
| Fixed-domain original versus candidate | **60/60 vectors exact** |
| Original dynamic cache versus annual reference | **36/60 vectors differ** |
| Dynamic normalized-pressure differences | **12 vectors; 42 cell records** |
| Candidate static-base + annual-mask replay | **40/40 exact** |

Three graph-contract tests also passed. These are targeted cache-fragment
tests, not a full-country or full-period validation of a deployed correction.

## Candidate and adoption contract

- Transform: `localhost/scripts/tools/fix_woodman_annual_sourcing_cache.py`.
- Tests: `localhost/scripts/tests/test_woodman_annual_sourcing_cache.py`.
- Contract identifier: `annual_domain_after_static_npa_cache_v1`.
- Temporary evidence: `E:/MoFuSS_Active/mdg_windows_performance_2026-10-06/annual_cache/native_probe_v1/`
  (`candidate_report.json`, `native_comparisons.csv`, `check.log`). This folder
  is disposable computational evidence; these conclusions and the candidate
  code are preserved in the repository.

The adopted reader contract updates `.rs_year_inputs()` in
`localhost/scripts/postprocessing_sourcing/2post_runtime_sourcing_v1.R` with
an explicit, backward-compatible capture contract:

1. Read `Sourcing/static/{W,V}_npa_baseCCC_SS.tif` for corrected runs instead
   of legacy `{W,V}_baseCCC_SS.tif`.
2. Use the corrected annual eligibility captures, which also encode the
   current landscape domain. Existing `.rs_eligible()` arithmetic replayed
   them exactly in the probe.
3. Read `Sourcing/MCxxx/accumulator_domainYY.tif` for the selected MC/year,
   replacing the legacy static accumulator-domain input for corrected runs.
4. Preserve the legacy reader route for existing runs and reject mixed
   contracts. Distinct filenames intentionally prevent silent reuse by the
   old reader.

The figures above describe the original fragment probe. See the linked release
report for subsequent source changes and verification; those later results do
not retroactively broaden the scope of this probe.
