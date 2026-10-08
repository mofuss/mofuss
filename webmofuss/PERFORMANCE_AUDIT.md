# Legacy WebMoFuSS performance audit

Status: **candidate for testing, not yet certified for server replacement**.
The supplied server backup has been inspected; it contains shared source/data
but no prepared WebMoFuSS case or Windows launcher. Linux runs preparation and
hands the simulation to Windows. The literal Windows Dinamica version remains
to be verified. No server files or canonical simulation directories were changed.
See [SERVER_BACKUP_AUDIT.md](SERVER_BACKUP_AUDIT.md) for the 2026-10-08 findings
and the missing files needed for an end-to-end test.

Later partial additions under `F:\webmofuss_speedup_sandbox\webmofuss_files`
contain three case folders (`6840ecfcad627`, `68e88410b267d`, `68e895bd7eca3`).
They currently supply source/preparation data, not runnable dynamics inputs or
completed outputs. All three configure Windows R 4.5.0 in `Rpath.csv`; their
companion TXT discovery files identify Linux R. This is consistent with the
Linux-preparation/Windows-simulation handoff. It does not identify the Dinamica
build. No partial-case preprocessing or simulations have been launched.

**Known compatibility counterexample:** appending an unused text column to the
numeric MC tables succeeds with the original but fails in the lookup candidate
with `Variant type is not real`. The fast candidate must not be deployed as an
unconditionally compatible replacement. Confirm the actual server's numeric
table contract or narrow the change before deployment. A statistics-only
candidate would avoid this eager-row-read issue. The small probe's original
produced all 12 expected TIFFs; its candidate produced none. Evidence is in
`E:\MoFuSS_Active\webmofuss_performance_audit\lookup_contract_probe\results.json`.

On 2026-10-08 a separate small local 2.4.1 probe tested a numeric-type guard
with an original-expression fallback. Both numeric and mixed-type cases
completed, each with all 12 TIFFs byte-identical to its original-expression
baseline. This addresses the demonstrated text-column failure in that probe;
it is not yet integrated into the full candidate and does not prove all
conditional/empty-table behavior. See [LOOKUP_GUARD_AUDIT.md](LOOKUP_GUARD_AUDIT.md).

## Deliverable

`7_dyn_Sc17_webmofuss_ctrees_g_v3_fast.egoml` is a standalone EGOML candidate.
It requires no Python helper at runtime. The original
`7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml` is unchanged.

The candidate preserves the old scientific model and single-scenario workflow.
It does not introduce paired scenario runs, change the number of realizations,
or substitute the newer localhost model.

Two conservative changes are included:

1. Select each realization's numeric parameter row once, then use numeric
   lookup tables in four raster expressions. The original arithmetic order,
   double-valued parameters, float32 output stages, class indices, and MC row
   identity are retained. These four maps are initialized once per MC, not once
   per simulated year. The local legacy engine cannot compile the original
   general-table raster accesses into native expressions.
2. Disable eight attribute flags whose results are not consumed: unnecessary
   statistical summaries on six maps, plus dynamic/statistical attributes on
   one map used only for cell width. Required sums and cell counts stay enabled.

All original loaders, output writers and their scopes, external R calls,
wizard metadata, constants, paths, precision settings, and Patcher settings
are retained. The reproducible builder checks the exact reviewed source hash
and rejects other model revisions.

The builder also offers `--last-mc-debugging`, disabled by default. That option
writes 25 shared diagnostic raster families only on the last realization.
For 30 MCs, it would remove 29/30 of those particular writes, not 29/30 of total
runtime. Intermediate file availability changes, so this option is not included
in the shipped candidate and needs a check for live web readers before use.

## Validation and limitations

Source repository: `C:\Users\UNAM\Documents\mofuss`.
Read-only fixture input source: `F:\LSO_1000m_bau1_2050_mc3_capped`.
Disposable tests and logs:
`E:\MoFuSS_Active\webmofuss_performance_audit`.

The local executable is Dinamica `2.4.1.20140602`, using two processors and
`-predefined-seed`. Tests run the original and candidate sequentially on separate
copies with identical frozen inputs. They remove the same four external R calls
from each test copy so initialization cannot regenerate MC draws and reporting
cannot write outside the fixture. The deliverable retains those calls.

The newer local fixture provides decennial IDW files. The first unadapted test
correctly failed on missing `In/IDW_C++_fw_v02.tif`. Subsequent fixtures copy each
decade's IDW to missing annual filenames solely in disposable test storage.
This permits a controlled old-model comparison, but does not make that fixture
a substitute for the actual web case. Every adaptation is recorded and hashed.

All three successful original/candidate pairs had byte-identical scientific
files and no missing or added files:

| Frozen fixture | MCs | Annual steps | Identical files | Original | Candidate | Elapsed time reduction |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Capped growth | 3 | 3 | 177 (150 TIFF, 27 CSV) | 111.07 s | 65.33 s | 41.2% |
| Uncapped/CTrees growth | 3 | 3 | 186 (159 TIFF, 27 CSV) | 112.58 s | 62.85 s | 44.2% |
| Capped growth, longer run | 2 | 21 | 740 (719 TIFF, 21 CSV) | 144.21 s | 97.73 s | 32.2% |

These are single whole-process timing observations for each variant. The
different growth branches are separate regression fixtures, not a change to
the web's one-scenario-per-job workflow. There were 1,103 byte-identical file
comparisons across the three pairs. This is not a benchmark of the server or
the full R/web workflow, and it does not negate the mixed-table counterexample.

The eight static tests passed. The process-MC prototype was checked in memory
but no actual MC worker processes were staged or launched. Full workflow tests
await the complete real web case and Windows launcher requested from the user.

Static checks cover unchanged I/O and external calls, expression arithmetic,
map precision, MC helper scope, live normalizers, unique/resolved IDs, exact
shipped-artifact reproducibility, and rejection of unreviewed source revisions.

The parameter-row optimization assumes the numeric matrices produced by the
legacy R preprocessing. The supplied backup's `rnorm_v3.R`, lines 643–678,
generates numeric Key/LULC columns, drops helper columns, and replaces NA/NaN
with 0.11111 before writing all three matrices. No generated matrices are in
the backup, so the real case's data still needs checking. Eagerly reading a full row differs from conditionally
reading individual columns for the confirmed mixed-type counterexample, and
also touches absent map categories and initial-stock cells that tree-cover mode
would otherwise bypass.

## Real multicore execution

See [MC_PARALLELISM.md](MC_PARALLELISM.md) for the exact graph and file audit and
[RUNTIME_COMPATIBILITY.md](RUNTIME_COMPATIBILITY.md) for runtime evidence.

MC realizations are independent after input generation; the nine cross-MC
feedback tables only collect results. Annual state within each realization is
dependent and must remain sequential. This is a good candidate for coarse
parallel execution, provided global MC indices and output collection are handled
correctly.

The local old engine did not overlap independent loop iterations or sibling
groups when given four processors. Identical scheduler probes did overlap under
the separately installed 8.13 engine. Consequently, setting the processor count
or copying graph groups is not sufficient evidence of parallel MC on the server.

Thirty processes with `-processors 1`, or thirty with `-processors 2`, are a
plausible architecture for 30 or 60 logical processor capacity, respectively.
They need private working/temp directories, distinct global MC identities,
appropriate random streams, and one ordered gather/report stage. They must also
fit memory and I/O capacity and the web job's allocated CPU budget. This is an
orchestration change beyond the current single-file candidate.

Worker count must be calculated per job, not hard-coded to 30 or 70. The sample
backup allocates eight Linux CPUs but passes only a session ID to the Windows
launcher. No existing model input or visible handoff communicates a Windows CPU
budget. Automatic hardware detection is not evidence of that budget. The missing
launcher must establish how the user's 8–70 CPU allocation applies on Windows.

Running thirty unmodified scripts with MC count set to one is incorrect: each
would select the first parameter draw, rerun preprocessing/reporting, and
overwrite common files. The prototype under `tests/` is a frozen-input mechanism
experiment only; it is not a production worker launcher or a validated RNG policy.

## Reproduce and continue

Run source checks from the repository:

```powershell
python -B -m unittest discover -s webmofuss/tests -p test_webmofuss_fast.py -v
```

Build to a new, unused path (the builder refuses overwrites):

```powershell
python -B webmofuss/tools/build_webmofuss_fast.py --output C:/path/to/new_candidate.egoml
```

`tests/run_webmofuss_regression.py` provides `stage`, `run`, and `compare`
subcommands with an explicit temporary `--root`. It delegates the frozen-input
copying and engine launch to the existing localhost regression harness. Its
comparison additionally rejects candidate-only scientific files.

Before replacing the production script, inspect the supplied web case and exact
engine build, run the complete old/candidate workflow on independent copies,
compare the complete final file set, CSV schema and values, raster geometry,
NoData and pixels, and confirm all R reports finish. Also check the options the
web interface actually exposes. Measure elapsed time and peak memory on that
machine. Keep the original available for rollback. Neither XML validation nor
the local frozen-input tests alone establish end-to-end server compatibility.

Original SHA-256:
`e4ce6ab47a12bbc7ed1f290a95ad6c1477182d21e95fba1428d816ecc85bca2d`.
Candidate SHA-256:
`a9a661abb802a66f4639ddbf7903bd660251b97f4fc0c8b1627e8255862ec54e`.
