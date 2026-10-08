# Within-scenario Monte Carlo parallelism: legacy web MoFuSS

This audit concerns only `7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml`, the legacy
web-server model, and the existing single-scenario job contract. It does not
propose replacing the model with the newer localhost model. Source line numbers
below refer to the original v3 file; node aliases and port IDs are the more
durable identifiers.

## Finding and boundary

The Monte Carlo realizations are mathematically independent once the existing
R preprocessing has prepared the shared parameter tables. Their only explicit
cross-realization graph state consists of nine result-collection tables. This
makes realization-level concurrency feasible in principle without changing the
model equations, parameter files, or final output names.

This graph finding does **not** establish that the installed Dinamica 2.11
scheduler can execute independent realizations concurrently inside one EGOML.
Removing feedback edges, duplicating groups, or adding a processor-count setting
must not be described as parallel MC execution without engine documentation and
a concurrent-execution test on that version. A processor policy may govern
threads inside a raster operator rather than simultaneous realizations.

During this audit, a separately installed local Dinamica runtime identified
itself as `2.4.1.20140602`. Small scheduler probes on that binary used four
independent two-second external tasks, `-processors 4`, and
`-scheduler AutoFunctorScheduler`. An outer `Repeat`, independent sibling leaf
functors, and independent sibling `Group` containers each took approximately
eight seconds. The one-processor `Repeat` baseline also took approximately
eight seconds. A Dinamica `8.13.0` positive control ran the same four tasks in
approximately 2.3 seconds with four processors and approximately 8.6 seconds
with one processor. This confirms that the probes can detect concurrency and
that newer-runtime behavior cannot be assumed for the older binary. Those
tests observed serial scheduling on the local 2.4 binary; the declared server
version 2.11 still requires its own check. Probe artifacts were
kept in the disposable directory
`E:\MoFuSS_Active\webmofuss_performance_audit\scheduler_probe`.

The graph cannot safely be parallelized by changing the outer loop alone:

- Nine lookup-table feedback cycles must become ordered collection/reduction.
- Twenty-five annual raster writers use filenames shared by all realizations.
- Annual biomass, harvesting, and land-transition state remains sequential
  within each realization.
- Concurrent realizations multiply raster memory and disk traffic.
- The legacy R preprocessing and finalization must each run once per job, in
  their existing order, rather than once in every worker.

## Audited graph

The outer `Repeat`, alias `repeat775` (line 219), obtains its iteration count
from `v282` and emits the realization step `v8`. `Number of MC runs` (line 4095)
gets the unchanged `monte_carlo_runs` value from
`LULCC/TempTables/parameters_dinamica.csv` via `getTableValue1911` (line 4102).

The nested `Repeat`, alias `repeat874` (line 545), emits temporal step `v39`.
Its count comes from `calculateValue4948` (line 3134), whose existing expression
is `v1 * 48 / v2 + 1`. Keep that expression and the meaning of its temporal steps
unchanged. Calling every step a year is shorthand only; the configured iteration
length controls the actual period.

The temporal loop has real dynamic dependencies:

| State | Feedback node | Initial input | Feedback input |
| --- | --- | --- | --- |
| Biomass stock | `muxMap268`, line 550 | `v200` | `v98` |
| Cumulative nonrenewable harvest | `muxMap780`, line 659 | `v191` | `v106` |
| Cumulative total harvest | `muxMap343`, line 665 | `v191` | `v107` |
| Loss landscape | `muxCategoricalMap5159`, line 768 | `v329` | `v70` |
| Gain landscape | `muxCategoricalMap5166`, line 781 | `v328` | `v187` |
| Cumulative deforestation fuelwood | `muxMap1856`, line 915 | `v191` | `v117` |
| Cumulative expected harvest | `muxMap2039`, line 957 | `v191` | `v118` |
| Cumulative deforestation quantity | `muxMap2073`, line 963 | `v191` | `v119` |

These cannot be divided among workers by time without carrying the full preceding
state. Realizations, rather than temporal steps, are the appropriate coarse
unit of parallel work.

### The nine cross-realization collections

Each outer `MuxLookupTable` starts from the same empty `"Key" "Value"` table.
Its only simulation-side consumer is the corresponding `SetLookupTableValue`.
The updated table feeds back to that mux and is saved after the MC loop. None of
these accumulated tables feeds the biomass or harvesting calculations. These
nine updated tables are also the only graph values exported from `repeat775`.

| Final file under `Temp/` | Updated port | Mux alias, line | Setter alias, line | Key port | Value port / unchanged expression |
| --- | --- | --- | --- | --- | --- |
| `3_NRB.csv` | `v20` | `muxLookupTable601`, 270 | `setLookupTableValue579`, 300 | `v18` | `v196`, `calculateValue1168`: `t1[12]` |
| `3_CON_TOT.csv` | `v21` | `muxLookupTable1335`, 258 | `setLookupTableValue1333`, 308 | `v17` | `v195`, `calculateValue1165`: `t1[12]` |
| `3_CON_NRB.csv` | `v22` | `muxLookupTable761`, 264 | `setLookupTableValue765`, 315 | `v19` | `v198`, `calculateValue1171`: `t1[12]` |
| `3_EXP_CON_TOT.csv` | `v24` | `muxLookupTable2058`, 439 | `setLookupTableValue2050`, 432 | `v17` | `v224`, `calculateValue2046`: `t1[12]` |
| `3_FW_DEF.csv` | `v26` | `muxLookupTable2096`, 469 | `setLookupTableValue2083`, 454 | `v17` | `v225`, `calculateValue2085`: `t1[12]` |
| `x_Cons_W_all.csv` | `v29` | `muxLookupTable2021`, 514 | `setLookupTableValue2020`, 475 | `v33` | `v229`, `calculateValue2014`: `t4[31]` |
| `x_Cons_W.csv` | `v30` | `muxLookupTable2025`, 520 | `setLookupTableValue2026`, 482 | `v33` | `v228`, `calculateValue2010`: `t3[31]` |
| `x_Cons_V.csv` | `v31` | `muxLookupTable2029`, 526 | `setLookupTableValue2032`, 489 | `v33` | `v227`, `calculateValue2008`: `t1[31]` |
| `x_Cons_V_all.csv` | `v32` | `muxLookupTable2017`, 508 | `setLookupTableValue1995`, 496 | `v33` | `v226`, `calculateValue2005`: `t2[31]` |

Key ports `v17`, `v18`, `v19`, and `v33` are all `Step` aliases for global MC
step `v8`. Gather results by that original global integer ID, not completion
order. Recreate the same empty table, apply the existing setters in increasing
MC ID order, and retain each original save-node type and options. In particular,
the two `W` files currently use `SaveTable`; the remaining seven use
`SaveLookupTable`. This is keyed collection, not summation across MCs: adding or
averaging worker rows would change the outputs.

### Parameter draws and global identity

Existing R-generated input tables are loaded once outside the MC loop:

- `Temp/i_st_all.csv`: `loadTable5426`, line 3779, port `v242`.
- `Temp/rmax_all.csv`: `loadTable5430`, line 3787, port `v243`.
- `Temp/k_all.csv`: `loadTable5428`, line 3796, port `v244`.
- `Temp/Harvest_pixels_W.csv`, `Prune_factor_V.csv`, `Prune_factor_W.csv`, and
  `Harvest_pixels_V.csv`: lines 3694–3719, ports `v233`–`v236`.

`calculateMap1114` (line 3186), `calculateMap5412` (3247),
`calculateMap1748` (3324), and `calculateMap5414` (3466) index parameter tables
using `t[[v1][i1 + 1]]`, with `v1` supplied by `v10`, a copy of the global MC
step. `getLookupTableValue5151`, `5153`, `5155`, and `5157` (lines 3518–3542)
use `v9`, another copy of that same step, as the harvesting-parameter key.

Thirty copies of the unmodified script with `monte_carlo_runs=1` therefore do
**not** implement MC IDs 1 through 30. Each copy would select MC row/key 1,
write the same realization suffix, run preprocessing and reports repeatedly,
and collide on shared outputs. A worker must keep the full original parameter
tables and receive its original global realization ID through every use of
`v8` and its aliases, independently of its local loop counter.

The script contains four stochastic `Patcher` nodes:
`patcher5095` (line 746), `patcher2190` (2834), `patcher219` (2888), and
`patcher5097` (2907). It has no explicit seed input or random expression. Two
harvest patchers sit behind the existing `Bypass Patchers` control, whose v3
constant is `.yes` (`v313`, line 4505). Retain the existing branch conditions;
do not remove the alternatives based on current defaults.

The user permits different random draws, while requiring the same calculations
and file contract. This permits altered draw order but not duplicated worker
streams or changed distributions. A process-worker implementation needs a
documented Dinamica-supported per-worker/per-realization seed strategy. Never
assume simultaneous process startup automatically gives distinct seeds. Keep
the existing R parameter draws shared and generated once; rerunning `rnorm_v3.R`
per worker changes both the draws and the preparation workflow. If repeatability
is needed, record the internal seed-to-global-MC mapping without adding required
web inputs.

## File collision analysis

The `Temp/2_*` raster and table saves inside the MC loop already receive `v8` as
their `step` and use `suffixDigits=2`. Preserve those values and Dinamica's own
filename formatting, including behavior above 99 MCs. Do not reconstruct names
using a different padding convention.

The following 25 raster outputs instead use temporal step `v39` and fixed
`Debugging/` paths. Every original serial realization overwrites the prior
realization's files. Successful final output therefore contains the **last
global MC's** version of each temporal file.

| Writer alias, line | Fixed filename under `Debugging/` |
| --- | --- |
| `saveMap5175`, 787 | `Cum_Sim_loss.tif` |
| `saveMap5057`, 796 | `Fw_def_tot.tif` |
| `saveMap5060`, 811 | `Sim_loss.tif` |
| `saveMap5064`, 820 | `Sim_gain.tif` |
| `saveMap5141`, 829 | `Expect_harv_tot.tif` |
| `saveMap5215`, 838 | `Harv_pix_W.tif` |
| `saveMap5218`, 847 | `Harv_pix_V.tif` |
| `saveMap5148`, 861 | `ProbHarv_V.tif` |
| `saveMap5151`, 870 | `ProbHarv_W.tif` |
| `saveMap8564`, 879 | `Proj_harv_Wtot.tif` |
| `saveMap8567`, 888 | `Proj_harv_Vtot.tif` |
| `saveMap8570`, 897 | `Non_harv_AGR.tif` |
| `saveMap8573`, 906 | `Ex_agr_harv.tif` |
| `saveMap7209`, 921 | `fnrb.tif` |
| `saveMap7212`, 930 | `nrb.tif` |
| `saveMap1948`, 939 | `Cum_harv.tif` |
| `saveMap2035`, 948 | `Cum_exp_harv.tif` |
| `saveMap2078`, 969 | `Cum_Fw_def.tif` |
| `saveMap3924`, 1008 | `harv_AGR.tif` |
| `saveMap3979`, 1023 | `Proj_harv_Vdef.tif` |
| `saveMap3997`, 1032 | `Proj_harv_Wdef.tif` |
| `saveMap3978`, 1041 | `Fw_def_totnb.tif` |
| `saveMap7137`, 1050 | `IniProb_V.tif` |
| `saveMap1072`, 1059 | `IniProb_W.tif` |
| `saveMap5178`, 2922 | `Cum_Sim_gain.tif` |

No `LoadMap`/`LoadCategoricalMap` in the audited EGOML reads any of these
`Debugging/` outputs. All four `RunExternalProcess` nodes lie outside the MC
loop. Saving these 25 families only when global MC ID equals the original MC
count preserves successful final file contents and removes `25 * (N - 1) * T`
overwritten writes, where `N` is MC count and `T` is temporal iteration count.
This is also required to prevent collisions under concurrency. It changes the
availability of diagnostic files during an unfinished/failed run, so a live
consumer of those intermediates would need separate consideration.

The other annual diagnostic outputs use `debugging_<MC ID>/...` created from
`v38`, another alias of global `v8`. Their `Growth_less_harv`, `Harvest_tot`,
`Growth`, `age`, and `Harvest_tot_nrb` outputs must remain present for every
relevant MC and branch. Do not apply the shared-path optimization to them.
`createString1826` (line 2931) describes a per-MC gain path but has no connected
output; `saveMap5178` actually uses the fixed `Debugging/Cum_Sim_gain.tif` path.

The `webmofuss` copy does not include the old `rnorm_v3.R`, `bypassMC.R`,
`NRB_graphs_datasets2.R`, `maps_animations7.R`, `bypass_maps_animations.R`, or
`finalogs.R` files invoked by the model. Their input/output assumptions and any
external progress monitoring require validation against the server copy. Newer
localhost scripts are not evidence that the old scripts behave identically.

## A practical worker design, conditional on runtime support

The intended scheduling unit is one entire realization of one scenario. The
following design describes the necessary behavior; it is not a claim that
Dinamica 2.11 already exposes a parallel-loop primitive.

1. Execute the existing job setup and MC parameter generation once. Complete
   all preparation before workers read the shared tables and source rasters.
2. Assign global MC IDs `1..N` to a bounded worker pool. Give each worker one
   or two compute threads and a private output/staging directory. Inputs can
   remain shared and read-only; do not copy large raster inputs unnecessarily.
3. Execute the unchanged realization body and unchanged serial temporal loop.
   Substitute global MC identity consistently for all uses of the outer step,
   including parameter selection, summary keys, and filenames.
4. Return the nine scalar summary contributions keyed by global MC ID. Keep
   realization-suffixed `Temp/2_*` outputs and per-MC diagnostic directories
   private until that worker finishes successfully. Only the last global MC
   supplies the shared `Debugging/` files.
5. After every required MC succeeds, validate the full expected ID set, reject
   duplicate/missing IDs, and gather with the original nine table setters in
   ascending ID order. Restore the exact existing output paths and formats.
6. Run `NRB_graphs_datasets2.R`, the existing selected maps/animations step,
   and `finalogs.R` once, after the complete gather. A worker's finish order
   must never determine which scenario is considered complete.

An in-process parallel-loop implementation could return the nine scalars to
the parent graph directly. A native graph-unrolling implementation could use
bounded batches of independent realization branches followed by the same
ordered setters. Either requires proven sibling/iteration concurrency in the
target engine; otherwise the unrolled graph only increases complexity and
memory. A process pool needs an orchestration layer and explicit child-thread
limits. It is not a drop-in EGOML-only change unless an existing runtime facility
can supply that orchestration without new server dependencies or launch changes.

For 30 MCs, 30 workers at one thread each can use approximately 30 compute
cores; 30 workers at two threads each can use approximately 60. These are CPU
budgets, not predicted speedups or guarantees of utilization. On eight available
cores, reasonable benchmark candidates are eight one-thread workers or four
two-thread workers, subject to memory and I/O. On a shared server, use the cores
allocated to this job, not the machine's total hardware count.

Choose the worker cap from measured limits:

```text
workers = max(1, min(
    MC_count,
    floor(allocated_compute_threads / threads_per_worker),
    floor((available_RAM - shared_resident_RAM - reserve_RAM)
          / incremental_peak_RAM_per_worker),
    measured_IO_safe_worker_cap
))
```

All memory terms must be measured for a representative region and configuration.
Check that at least one worker fits before starting; `max(1, ...)` is not a
permission to exceed memory. Process workers may duplicate data cached in each
process even when source files are shared. Include native-library threads in
the CPU budget to avoid, for example, 30 workers each trying to use 70 threads.

Use private staging to make failures diagnosable and retries safe. A failed
worker must not publish a partial summary or partially replace final output.
Retry only the failed global MC with the same prepared inputs and recorded
seed, discard its previous incomplete staging only, and leave successful MCs
untouched. Never report successful completion or run the final report with a
subset of MCs. These staging directories are disposable job work, not canonical
runs or evidence archives.

## Acceptance evidence required before server replacement

The graph audit is not runtime certification. Compare the original and candidate
on the installed Dinamica version and the same frozen, representative job inputs.
Validation must cover both CTrees and the existing alternative branch, relevant
patcher settings, small and representative larger regions, and the supported MC
counts. Check the full filename inventory, CSV headers/types/row order, raster
dimensions/type/CRS/nodata, and expected deterministic numerical agreement.
For allowed changed random draws, compare the intended distributions and model
accounting invariants rather than asserting byte-identical stochastic maps.

Measure wall time, peak RAM, CPU utilization and disk throughput at one worker,
then bounded increases such as two, four and eight workers. Add higher worker
counts only while the measured resource headroom and speedup justify them.
Test missing input and worker failure handling: the job must fail visibly and
must not publish a successful partial report. Keep the original script available
for rollback until full server acceptance passes.
