# Lookup-cache compatibility and fallback probe

Date: 2026-10-08. This is a bounded local-engine investigation, **not a server
compatibility certification**. No shipped EGOML or candidate-builder code was
changed by this investigation.

## Confirmed incompatibility in the unguarded lookup cache

The original expressions access the current Monte Carlo row only at columns
selected by raster categories and executed expression branches. The proposed
selected-row cache eagerly reads every column.

A native microprobe appended one unused quoted text column to otherwise
unchanged parameter tables. The original four v3 expressions completed and
saved 12 TIFFs (four expressions, three MC rows). The unguarded selected-row
version failed before writing any TIFF:

`CalculateLookupTable`: `Variant type is not real.`

Temporary evidence is in
`E:\MoFuSS_Active\webmofuss_performance_audit\lookup_contract_probe\results.json`
and its `original`/`optimized` subdirectories. These temporary artifacts may be
deleted after conclusions are preserved; this note is the permanent finding.

Blank CSV cells are a separate issue: an earlier probe rejected both original
and optimized models at `LoadTable`. That observation does not remove the
confirmed unused-text-column regression.

## Verified type guard and fallback mechanism

The exact local executable used was
`C:\Program Files\Dinamica EGO\DinamicaConsole.exe`. Its runtime banner was:

`Dinamica EGO 64, 2.4.1.20140602 (Awesometastic) [build 1401730828]`

The installed help page
`C:\Program Files\Dinamica EGO\Help\doku.php@id=get_table_info.html`
documents `GetTableInfo`'s `Column_Type` as 0 for strings and 1 for real numbers.
The native metadata probe confirmed both codes. The documentation date is not
an engine version; neither the user's recollection of the server version nor
the backup establishes that the server runs this local build.

For each of the three parameter tables, the disposable guard prototype used:

1. `GetTableInfo` -> `GetTableColumn("Column_Type")` -> `LookupTable`.
2. `ExtractLookupTableAttributes`, with statistical flags off and dynamic
   key/value attributes on.
3. Compare attribute 31 (sum of type codes) with attribute 1 (column count).
   Equality means every column, including the key, is numeric.
4. Combine the three comparisons into one Boolean outside the MC repeat.
5. Put original expressions in `IfNotThen` and cached expressions plus all
   row-cache helpers in `IfThen`. Rejoin each result using `MapJunction` under
   its original output ID. Keep the existing output writers outside the
   branches.

This conservative guard falls back for any text column, including an unused
one. It does not coerce, replace, drop, or edit the input data.

The native probe used the four original v3 calculation expressions, three MC
rows, and the existing small raster fixtures. It tested the ordinary numeric
tables and the same tables with unused text columns:

| Inputs | Original exit / TIFFs | Guarded exit / TIFFs | Comparison |
| --- | --- | --- | --- |
| Numeric | 0 / 12 | 0 / 12 | All 12 TIFFs byte-identical |
| Unused text column | 0 / 12 | 0 / 12 | All 12 TIFFs byte-identical |

The numeric guarded run executed nine `CalculateLookupTable` operations
(three tables times three MC rows); the text guarded run executed zero. This
confirms that the unsafe helpers were not evaluated on the fallback path.

Elapsed times were approximately 3.23 versus 5.41 seconds for numeric inputs
and 2.93 versus 6.07 seconds for text inputs. These tiny runs include native
compilation and show **overhead, not a demonstrated speed benefit**. A full
representative case is needed to assess whether faster raster evaluation
outweighs guard and compilation costs.

Temporary probe files are under
`E:\MoFuSS_Active\webmofuss_performance_audit\lookup_guard_probe`:

- `metadata.egoml`, `metadata.log`, `mixed_info.csv`, `numeric_info.csv`, and
  their type/attribute/guard CSVs establish the metadata codes and condition.
- `fallback_results.json` records the four native runs and exact comparisons.
- `numeric_original`, `numeric_guarded`, `text_original`, and `text_guarded`
  retain disposable models, outputs, and logs.

## Limits and deployment posture

The numeric-type guard resolves the demonstrated text-column failure. It is
**not a proof of universal compatibility**:

- A numeric table may lack a requested MC key. The original may never access
  it on an empty/NoData map or bypassed expression branch, while an eager
  row-cache helper could still fail. The type guard does not test row-key
  existence, map emptiness, or other structural validity.
- In v3, initial-stock table `v242` is only read by expression `v201` when
  tree-cover control `v270` is false. Eager caching changes this expression's
  lazy access when all tables pass the numeric guard.
- All three original `LoadTable` nodes already sit outside conditional scopes;
  absent files are not newly required by the cache.
- Expression `v207` lies under the AGB-map `IfThen` branch, but its capacity
  table `v244` is also used by unconditional expression `v203`. That optional
  branch does not introduce a uniquely optional table file.
- This probe does not reproduce the entire v3 graph's conditional execution,
  external R stages, complete output collection, server launcher, or server
  engine. It does not test every numeric edge case or initialization mode.

The actual backup's `rnorm_v3.R` generates numeric `Key` plus `LULC_*` columns,
retains only those generated data columns, and replaces NA/NaN with `0.11111`
before writing `k_all.csv`, `rmax_all.csv`, and `i_st_all.csv` (lines 643–678).
However, the backup at `F:\webmofuss_speedup_sandbox` contains no prepared
simulation case or frozen MC tables, so that actual runtime contract has not
been verified on a completed job.

Do not describe the unguarded lookup candidate as universally interchangeable.
Keep statistics-only changes as the narrower compatibility option. A guarded
lookup candidate remains an implementation and full-regression task until a
prepared case and the actual Windows launch command are available.
