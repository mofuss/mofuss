# Dinamica v12/v13: runtime country sourcing and domestic W correction

## Which version to run

`10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml` is the new domestic-W model.
`v11` is retained unchanged. `v12` is the observational/performance comparison
version: it retains the old scientific mechanics, including the old pooled W
redistribution. Do not use v12 to enforce the domestic-only W policy.

No IDW recalculation is required for the four prepared ECSA folders. Their
existing W components already have disjoint domestic domains; V components
already use the permitted bilateral source domains. No permissions, demand
scenarios, biomass-growth formulas, Monte Carlo draws, stock cap, or V allocation
rules are changed by this release.

## An actual bug found in the old regional W path

Country-specific W demand was normalized separately, but its shortfall on
trees outside forests (TOF) was subsequently pooled across the region and
redistributed using regional forest weights. Thus strictly domestic IDWs did
not guarantee strictly domestic **final** W harvest.

v13 computes each origin's TOF shortfall and redistributes it only through that
same origin's eligible forest weights. If that origin has no eligible forest,
the shortfall remains unmet. v13 changes the old `v93` redistribution result;
subsequent biomass and harvest changes are the intended consequence of this
bug correction, not a performance approximation. Shared stock clipping and the
rest of the model remain unchanged.

A native-engine four-cell test reproduced 40 units of cross-border W leakage
and removed it. In a no-domestic-forest case, 80 units previously moved to the
other country; v13 leaves those 80 unmet. With no TOF shortfall, old/new realized
harvest TIFFs were SHA-256 identical. See
`tests/test_dinamica_domestic_tof_v13.py`.

For ECSA, all 13 W source masks were checked against the country-ID raster:
9,317,660 domestic source cells, zero overlaps, zero uncovered country cells,
and zero wrong-country cells. Corresponding masks and the administrative
raster are identical across all four runs.

## What is recorded

Each run receives a `Sourcing` directory:

- `static`: exact Dinamica-written float32 component bases after NPA adjustment
  and analysis masking, one per origin/channel/decadal snapshot; plus the
  initial accumulator's zero/null domain.
- `MC001`, `MC002`, etc.: compact annual eligibility/Patcher masks and scalar
  records for each origin. v13 also records the forest selector and each W
  origin's TOF shortfall/forest normalization sum.
- `metadata`: installation receipt and frozen input/country crosswalk
  snapshots, when prepared using the release installer.

The scalar records use integer pieces to preserve binary64 values exactly;
ordinary Dinamica CSV decimal output does not provide sufficient precision.
This codec was tested bit-for-bit for 69 values in both native and interpreted
engine modes. The normal release does **not** save every full origin map every
year. Existing annual scientific outputs retain their original filenames.

The static files also act as a cache. The first MC draw refreshes a base at each
new decadal snapshot; later annual steps and MC draws reload the same lossless
float32 file. The two original rounding stages are preserved, not combined.
Unused statistical summaries are disabled/removed; model arithmetic and origin
addition order are otherwise preserved in v12.

## After the simulations finish

Run `postprocessing_sourcing/0post_runtime_sourcing_pipeline_v1.R`. Its editable
configuration currently targets the four ECSA `mc3` folders and writes to:

`E:/_postprocessing_draft/ECSA_1000m_ics3_2050_mc3/runtime_sourcing`

The core reader is `postprocessing_sourcing/2post_runtime_sourcing_v1.R` and can
also be called for other regions with explicit run, country-zone, crosswalk and
output paths. Neither script runs Dinamica or IDW.

Tables include annual and period origin-to-source matrices, demand/harvest
balances, domestic/import percentages, and MC summaries. W, V, and combined
W+V summaries use volume sums, not averages of percentages. Default reporting
periods are 2020–2030, 2030–2040, 2040–2050 and 2020–2050, all **inclusive**.
Adjacent decades therefore share their boundary year; do not sum them to obtain
2020–2050—use that period's separate row.

## Scientific interpretation and safeguards

These are **model-implied sourcing accounts, not observed trade**. Runtime
origin pressures are reconstructed and checked against the saved aggregate
pressure before attribution. At a pixel where demand origins/channels compete
for limited stock, realized harvest is allocated proportionally to their final
pressures. That is an explicit accounting convention: the original model does
not select which consumer wins that competition.

Domestic/import/source shares use realized `Harvest_tot` extraction as their
denominator. Deforestation-wood credits are reported separately. Their mapped
location is where a neighborhood credit is **applied**, not necessarily where
the felled biomass originated; these records do not geographically trace that
separate supply process. Thus the result distinguishes demand, credited supply,
realized attributed extraction, and unmet demand rather than claiming all
household consumption has an independently observed origin.

The reader rejects pressure-reconstruction discrepancies. It reports float32
decomposition residuals separately and checks origin/source reconciliation.
The normal batch stops on signed negative legacy adjustments; diagnostic mode
may retain them but withholds misleading sourcing percentages. Frozen semantic
metadata is verified so later country-index changes cannot silently relabel
an earlier run. Do not edit or replace the captured `Sourcing` files.

v13 requires domestic, non-overlapping W domains, or a single-country/own-area
component. Old region-wide W-origin input packages must be regenerated before
using v13. The country-table reader requires individually identified country
origins; it must not invent country identities for an unpartitioned custom AoI.

## What equality and speed claims mean

The release tests use the installed Dinamica EGO 2.4.1 engine, identical frozen
Monte Carlo tables, fixed engine settings and independent temporary folders.
The old model first passed a repeatability check. v12 must match every existing
scientific TIFF/CSV by SHA-256; new sourcing files are additional outputs.
The tests cover capped/uncapped dynamics, multiple MC draws, decadal transitions
and a synthetic two-country runtime fixture. The v13 bug correction is tested
separately and is **not** claimed to be SHA-identical where it changes W supply.

These are checks of the tested dynamics, not a universal guarantee for every
possible dataset. R initialization/reporting calls are omitted identically in
the paired fixtures to reuse the same MC draws; the production R bundle is
unchanged. Full ECSA production runs have not been launched by this work.

The cache removes repeated raster work, but a full-region net speedup has not
yet been measured. Small fixtures have fixed compilation/capture overhead and
can be slower. Do not interpret structural optimization as a promised runtime
percentage. The test harness gives each process its own temporary directory to
avoid concurrent native-expression compiler file collisions.

## Reproducibility

- `tools/build_dinamica_sourcing_v12.py`: mechanically builds v12, or v13 with
  `--domestic-tof-correction`; `--cache-static-bases` enables the exact cache.
- `tools/dinamica_v12_transform.py`: tightly constrained statistics cleanup.
- `tools/dinamica_scalar_codec_v1.py`: exact scalar encoding.
- `tools/install_runtime_sourcing_model_v1.py`: read-only by default; explicit
  `--install` copies a new model beside v11 and SHA-verifies input snapshots.
- `tests/dinamica_runtime_regression_v1.py`: isolated full-engine paired runs.
- `tests/test_dinamica_sourcing_graph_v1.py`: graph/observer/cache contracts.
- `tests/test_runtime_sourcing_v1.R`: sourcing-accounting edge cases.

Generated tests, logs and raster diagnostics are disposable task material under
`E:/MoFuSS_Active/ecsa_sourcing_and_speed_v1`; model source, tests and this
methodological record remain in the source repository.
