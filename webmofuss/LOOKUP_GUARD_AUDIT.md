# Lookup-cache compatibility and fallback probe

Date: 2026-10-08. This is a bounded local-engine investigation, **not a complete
workflow compatibility certification**. The original model is untouched. The
guard is now implemented in the WebMoFuSS builder and shipped fast candidate;
the first sections below retain the evidence that motivated that improvement.

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
an engine version. The user subsequently confirmed that the server model runs
on this same local engine/computer; the native banner above is the tested
version evidence.

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

## Limits of the first, type-only prototype

The numeric-type guard resolves the demonstrated text-column failure. The
first prototype left the following issues, which motivated the additional
row-presence guard described below:

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

## Implemented full-model guard

`tools/build_webmofuss_fast.py` now generates the guard directly; the checked-in
`7_dyn_Sc17_webmofuss_ctrees_g_v3_fast.egoml` exactly equals its default output.
Its SHA-256 is
`15d9e2ff6f5aa9b166ab8a9ddc18f3f21cbd0960a18ef4c25e426fb5f5db5932`.
The original SHA-256 remains
`e4ce6ab47a12bbc7ed1f290a95ad6c1477182d21e95fba1428d816ecc85bca2d`.

- Metadata chains `v92000`–`v92002`, `v92004`–`v92006`, and
  `v92008`–`v92010` are computed once per run, inside simulation `group3674`.
  Attribute tables `v97000`–`v97002` and condition `v97099` check that all
  columns in all three source tables have numeric type codes.
- `GetTableKeys` and `LookupTable` pairs `v97200`–`v97205` execute only inside
  an `IfThen(v97099)`. They convert numeric keys, not parameter values, and
  are also computed once per run. String-key tables therefore cannot trigger
  an unsafe conversion on the fallback path.
- For each positive MC key `v10`, `GetLookupTableValue` nodes
  `v97100`–`v97102` find that key in each key list with missing-value default
  zero. Condition `v97103` requires all three returned keys to equal `v10`.
  The numeric-type false branch supplies zero through `v97104`;
  `ValueJunction v97105` is the final cache condition. The MC repeat starts
  at one, so a zero default cannot be mistaken for its current key.
- Only `IfThen(v97105)` executes selected-row caches `v92003`, `v92007`,
  and `v92011`. Missing numeric MC rows fall back to the original expressions,
  which retain their original lazy table accesses on bypassed or NoData cells.
- Each of `v201`, `v203`, `v207`, and `v213` is replaced **at its original
  scope** by cached and original branches, then a `MapJunction` retaining the
  original output ID. Original calculations have private IDs
  `v94000`–`v94003`; cached calculations have `v95000`–`v95003`. In particular,
  the `v207` branches remain under original AGB-map `ifThen1735`.

All original load/save nodes, paths, R process calls, writer scopes, arithmetic,
map cell types, constants, and root wizard metadata remain unchanged. The
original fallback calculation XML is structurally identical except for its
private result ID. No input columns are changed or discarded.

The original R initialization dependency is retained: simulation `group3674`
contains `v7` referring to `v294` in `group2500`, which also contains
`runExternalProcess2510` with `waitProcessCompletion=.yes` (original lines
166–169 and 4208–4221). Original parameter loaders are in nested `group5424`;
new metadata remains inside `group3674`. The complete top-level group peer
dependency graph is unchanged. There is no separate explicit dependency from
each original table loader to the external process. The completed fresh
two-MC full-callback case confirms ordering in this workflow: generated
matrices and all 4,074 scientific outputs match. Frozen-input microprobes
alone could not establish that ordering.

## Native regression of the implemented guard

Eight one-core native runs used the builder's actual replacement and helper
nodes, the same four original map expressions, and three MC rows. All eight
exited zero without error messages. Each pair produced the same 12 TIFFs with
identical bytes:

| Frozen-input case | Original / guarded TIFFs | Cached row evaluations | Exact equality |
| --- | --- | --- | --- |
| All numeric columns and rows present | 12 / 12 | 9 | All 12 |
| Extra unused text column in every table | 12 / 12 | 0 | All 12 |
| Initial-stock row 2 missing, tree-cover branch bypasses that table | 12 / 12 | 6 | All 12 |
| Row 2 missing from all tables, LUC map entirely NoData | 12 / 12 | 6 | All 12 |

The last two pairs directly test the eager-access regression left by the first
prototype. MC rows 1 and 3 use caches; row 2 uses the original expressions.
Temporary evidence, input hashes, model hashes, outputs and logs are under
`E:\MoFuSS_Active\webmofuss_performance_audit\lookup_guard_probe\builder_guard_v2`;
the reproducible scratch driver is its sibling `run_builder_guard_v2.py`.
A separate `row_presence` microprobe established the key-list lookup's
present/missing results as 1, 1, and 0 for MC keys 1–3.

The static suite now has 11 passing tests, including shipped artifact equality,
unchanged I/O, fallback expressions, original conditional placement, MC cache
scope, type/row guards, unique/resolved peers, and unchanged group dependencies.
Run it with `python -B -m unittest discover -s webmofuss/tests -p test_webmofuss_fast.py`.

These microprobes remain slower overall because of native compilation and
guard overhead; they are compatibility checks, not speed evidence. The added
guard does not certify every possible numeric edge case, malformed table,
production reporting option, or memory constraint. The separate full workflow
now passes the declared two-MC capped fixture with all R callbacks, reports
and animation; see `SMALL_CASE_VALIDATION.md`. Its competing workloads prevent
an end-to-end speed conclusion. A statistics-only candidate remains the
narrower alternative if guarded lookup overhead outweighs its pixel savings.

## Scope qualification from graph review

The numeric-row guard assumes the generated parameter matrices have one key
column. `GetTableKeys` returns only the first key column; the guard does not
prove compatibility with arbitrary numeric composite-key tables. The supplied
`rnorm_v3.R` writes a single numeric `Key=1:MC` for all three matrices. The fresh
full-callback test verifies that contract: each table has keys 1 and 2, 760
numeric parameter columns and exclusively finite values. The three tables
are byte-identical between the original and guarded runs. Log counters show
exactly three added selected-row evaluations per MC. The demonstrated text-column
and missing-row fallbacks should not be read as universal validation of every
possible Dinamica table structure.
