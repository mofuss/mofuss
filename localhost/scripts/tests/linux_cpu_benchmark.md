# Angola capped CPU/MC scheduling experiment

Date: 2026-09-29. Dinamica EGO 8.13.0.20260827, Intel i7-7700,
4 physical cores / 8 logical CPUs, 31 GiB RAM, Linux.

## Result and decision

| Execution | Engine wall time | Change from four workers |
| --- | ---: | ---: |
| Sequential MC, 4 EGO workers | 215.49 s | baseline |
| Sequential MC, 8 EGO workers | 184.26 s | 14.5% less time |
| 3 simultaneous MC jobs, 1 worker each | 346.30 s | 60.7% more time |
| 3 simultaneous MC jobs, 2 workers each | 270.95 s | 25.7% more time |

Parallel MC execution is **not included in the production launcher**. It did
not meet the requested threshold for a meaningful gain and would add job
isolation, shared-cache handling, and merging of nine summary tables.

Automatic worker selection uses physical cores within the available CPU
allocation, giving **4 workers on this machine**, matching the completed
Windows/Linux validation configuration. Eight workers remain an explicit
`--processors 8` option. The modest gain in this sample does not establish a
faster complete report pipeline or exact equality of every numeric output.

## Method and limits

Each case used the same capped inputs and frozen MC CSV batch, all three
realizations, and the first five annual iterations. The benchmark changed only
the simulation length and worker/job arrangement in isolated model copies.
Preprocessing and reporting were excluded. Existing completed runs were retained.

The isolated MC jobs reset their own annual state, keep the original MC index
for input lookup and output names, build separate sourcing caches, and concatenate
summary rows without changing their numeric strings. A worker budget of three
or six was used for the simultaneous jobs, below the eight available logical CPUs.

This was one pass on a shared workstation, with warm filesystem caches and some
overlapping short verification tasks. It is a practical decision check rather
than a general scaling study. The server restarted during the 3 × 1 case; the
engines completed and their continuous wall times were recovered from GNU time.
Quoted times use engine wall time, not the restarted orchestration timer.

For all three alternative cases, all **132 raster products** had identical
decoded cell values to the four-worker reference. Across the **57 tables**,
17 values (3 × 1), 23 values (3 × 2), and 29 values (eight workers) differed in
their last floating-point digits (maximum absolute difference about `1.05e-5`
in large aggregate totals).
These are not bit-for-bit identical results. No new Windows run was needed for
this scheduling comparison.

Raw models, inputs references, logs, GNU time records, timing JSON, and
per-product comparisons are retained outside Git in the capped scenario at
`_migration/benchmarks/cpu_mc/`.

## Related engine documentation

- [Dinamica Console worker options](https://csr.ufmg.br/dokuwiki/doku.php?id=dinamica_console)
- [Data flow and loop-carried dependencies](https://csr.ufmg.br/dokuwiki/doku.php?id=basic_data_flow)

The model's MC loop has table muxes that carry summary results between
realizations. The yearly loop also carries biomass state between years. Increasing
the engine worker count does not make either loop's dependent iterations independent.
