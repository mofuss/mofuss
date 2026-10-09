# Runtime compatibility and parallel-execution evidence

Audit dates: 2026-10-07–08, local time. Scope: the existing web-server model
`7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml`, preserving its inputs, scientific
calculation, and output contract. This document records runtime evidence; it
does not certify a replacement for deployment or report a MoFuSS speedup.

**Current deployment clarification, 2026-10-08:** the user confirmed that this
same Windows computer runs the actual server EGOML and corrected the maximum
CPU count to **8**, replacing the earlier 70-core statement. Eight is the total
ceiling to respect while accounting for other active jobs; it is not eight
additional idle cores. The discussion of larger machines below is retained as
historical investigation, not a requirement for this deployment. The Windows
launcher was subsequently located and confirms the legacy executable and
`-processors 0` automatic detection, as described below.

**Completed validation, 2026-10-08:** the original and guarded candidate both
completed the fresh Nakuru 33 × 33 case, two MC realizations over 2000–2050,
including all four R callbacks. All **4,074 scientific files** matched byte
for byte (1,752 TIFFs and 2,322 CSVs), two GeoPackages matched semantically,
and all 5,804 produced filenames matched. Report text and all ten rendered
PDF pages matched exactly; PDF metadata differed. The animation also matched.
All 51 annual official-C++ IDW pairs had completed and passed the fixture
checks before these runs. See [the complete small-case validation](SMALL_CASE_VALIDATION.md)
for staging adaptations, evidence, and coverage limits.

The original took 546.302 seconds and the guarded candidate 708.428 seconds
with four other jobs active. This timing pair does **not** establish a speedup
or comparable isolated performance. It establishes matching outputs for the
tested fixture and callback path. The absent production `idwW.R` wrapper and
untested scenario/size branches remain limits; the completed local fixture is
not represented as a recovered production session. Earlier preparation and
probe statuses below describe their historical stages.

## Exact versions and the server-version question

Two separately installed Windows runtimes were inspected and exercised:

| Executable | Version reported by the executable | Build |
| --- | --- | --- |
| `C:\Program Files\Dinamica EGO\DinamicaConsole.exe` | `Dinamica EGO 64, 2.4.1.20140602 (Awesometastic)` | `1401730828` |
| `C:\Users\UNAM\AppData\Local\Programs\DinamicaEGO-8.13\DinamicaConsole8.exe` | `Dinamica EGO 8, 8.13.0.20260827 (Nightingale Nuggets)` | `631132458` |

The user's initial version recollection was **2.11**. The identification is now
resolved for this deployment: the user confirmed the same computer executes
the server model, and the supplied Windows launcher
`F:\webmofuss_speedup_sandbox\scripts\run_dinamica.cmd` names
`C:\Program Files\Dinamica EGO\DinamicaConsole.exe`, whose verified version is
**2.4.1.20140602**. It passes `-processors 0 -log-level 4` and the original
`7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml` under its job base directory. All four
active Dinamica processes inspected on 2026-10-08 used this same legacy
executable. The launcher was inspected as text and not executed; no network
credentials are reproduced in this document.

The installed historical help includes 2.0.1 through 2.0.10, then 2.2 and 2.4.
Official version 3 notes introduce an experimental `ParallelForEach` container
and describe a ten-processor automatic-detection limit that could be overridden
manually. That is a historical version-specific limit, not evidence that every
current installation has the same limit.
[Source: version 3 release notes](https://dinamicaego.com/dokuwiki/doku.php?id=what_is_new_3).

Version 5 rebuilt the parallel execution system and explicitly supports
independent functors and independent loop iterations using a worker pool. These
capabilities must not be inferred for a 2.x runtime from present-day manuals.
[Source: version 5 release notes](https://dinamicaego.com/dokuwiki/doku.php?id=what_is_new_5).

## Measured scheduler probes

Each probe performs four independent tasks. A task runs Windows `ping.exe`
against `127.0.0.1` with three replies, taking approximately two seconds. The
model waits for each external process to finish. The graph variants are:

- `repeat`: one `RunExternalProcess` inside `Repeat`, with four iterations.
- `siblings`: four independent `RunExternalProcess` functors at script scope.
- `groups`: four independent sibling `Group` containers, each containing one task.

Every legacy run explicitly selected `AutoFunctorScheduler`. Modern runs used
the default scheduler; their logs confirmed the requested worker count and
enabled parallel functors, parallel steps, and parallel map loading. All eight
executions reported successful model completion and process exit code zero.

| Graph | Requested processors/workers | Local 2.4.1 elapsed seconds | Local 8.13.0 elapsed seconds |
| --- | ---: | ---: | ---: |
| Repeat, four iterations | 1 | 8.796 | 8.605 |
| Repeat, four iterations | 4 | 8.732 | 2.325 |
| Four sibling functors | 4 | 8.577 | 2.269 |
| Four sibling Groups | 4 | 8.548 | 2.312 |

These are single observations of wall-clock time including executable startup.
They demonstrate serial execution of these constructs in the tested legacy
runtime and overlap in the modern positive control. They do not measure raster
work, CPU scaling, memory demand, or MoFuSS execution speed. In particular, they
do not establish a fourfold MoFuSS improvement, nor rule out parallel algorithms
inside individual legacy raster operators.

The practical finding is narrower: replacing the MC loop with independent
sibling Groups is **not demonstrated to enable concurrent MC runs on the old
engine**. The same probes establish that the measurement method detects
concurrency in an engine that supports it.

Disposable evidence resides under
`E:\MoFuSS_Active\webmofuss_performance_audit\scheduler_probe`:

- `measurements.json`, the three `.egoml` files, and `repeat_1.txt`,
  `repeat_4.txt`, `siblings_4.txt`, `groups_4.txt` record the 2.4.1 probes.
- `8_13_control\measurements.json` and the corresponding scripts/logs record
  the 8.13 positive control.

This document preserves the measurements if that temporary folder is deleted.
No canonical simulation run was moved or overwritten by these probes.

## Reproducing the small Windows probe

Use a new task subfolder below `E:\MoFuSS_Active`, or an equivalent disposable
server location. Run from that directory because Dinamica writes logs. Do not
run from the source repository, a canonical simulation folder, or the installed
program directory. The following PowerShell creates all three small models:

```powershell
$probeDir = 'E:\MoFuSS_Active\webmofuss_scheduler_probe_recheck'
New-Item -ItemType Directory -Path $probeDir -ErrorAction Stop | Out-Null
Set-Location -LiteralPath $probeDir
$taskXml = @'
<functor name="RunExternalProcess">
  <inputport name="fileName">&quot;C:/Windows/System32/ping.exe&quot;</inputport>
  <inputport name="parameters">&quot;127.0.0.1 -n 3&quot;</inputport>
  <inputport name="waitProcessCompletion">.yes</inputport>
  <inputport name="secondsToWait">0</inputport>
</functor>
'@
$prefix = '<?xml version="1.0" standalone="yes"?><script>'
$repeatXml = $prefix + '<containerfunctor name="Repeat">' +
  '<inputport name="iterations">4</inputport>' + $taskXml +
  '</containerfunctor></script>'
$siblingsXml = $prefix + ($taskXml * 4) + '</script>'
$groupsXml = $prefix + (('<containerfunctor name="Group">' +
  $taskXml + '</containerfunctor>') * 4) + '</script>'
[IO.File]::WriteAllText((Join-Path $probeDir 'repeat.egoml'), $repeatXml)
[IO.File]::WriteAllText((Join-Path $probeDir 'siblings.egoml'), $siblingsXml)
[IO.File]::WriteAllText((Join-Path $probeDir 'groups.egoml'), $groupsXml)
```

For the tested legacy executable, the invocation was equivalent to:

```powershell
$engine = 'C:\Program Files\Dinamica EGO\DinamicaConsole.exe'
& $engine -version
& $engine -help
Measure-Command {
  & $engine -processors 4 -scheduler AutoFunctorScheduler -log-level 3 .\repeat.egoml
}
```

Repeat with `-processors 1`, then the `siblings.egoml` and `groups.egoml`
filenames at four processors. The 8.13 control used its executable path and
omitted `-scheduler`. Save console output and verify successful completion as
well as time. An exit code alone is insufficient: during version inspection,
8.13 returned zero after reporting that a log path was unwritable. The probe
relies on Windows loopback ping and is not a portable Linux benchmark.

## What processor settings mean

Legacy `ProcessorPolicy` specifies a maximum number of processors, optionally
forcing the supplied count. It does not itself establish that MC iterations
can execute concurrently.
[Source: Processor Policy](https://dinamicaego.com/dokuwiki/doku.php?id=processor_policy).

Modern `ExecutionPolicy`, previously named `ProcessorPolicy`, governs task
granularity and computing target. A global `-processors` setting governs the
worker pool; neither is documented as reserving a private fixed pair of CPU
cores for each MC realization. Separate processes each configured for one or
two workers provide a different architecture, requiring safe orchestration,
independent writes, and a final aggregation stage.
[Sources: Execution Policy](https://dinamicaego.com/dokuwiki/doku.php?id=execution_policy)
and [console options](https://csr.ufmg.br/dokuwiki/doku.php?id=dinamica_console).

In the modern engine, independent loop execution additionally requires no
direct Mux, no loop-produced value consumed outside the loop, and no loop member
exported as a submodel output. Removing one feedback edge does not guarantee
parallelization. A Table Manager with independent subtable registration is a
documented way to collect results without a cross-iteration Mux. Compatibility
of these mechanisms with the actual server remains a separate requirement.
[Sources: basic data flow](https://dinamicaego.com/dokuwiki/doku.php?id=basic_data_flow)
and [table handling](https://dinamicaego.com/dokuwiki/doku.php?id=manipulating_tables_and_lookup_tables).
See `MC_PARALLELISM.md` for the model-specific dependency and writer audit.

## Compilation and profiling are version-dependent

The inspected 2.4.1 `-help` exposes `-processors`, `-scheduler`,
`-predefined-seed`, `-dont-run`, and `-disable-native-expressions`. It does not
list modern `-profile`, `-disable-parallel-steps`,
`-disable-parallel-functors`, or `-memory-allocation-policy` options. The current
console manual documents those modern controls; use only options present in
the target binary's own help. Modern `-profile` can expose worker activity and
resource use, but is not a verified legacy profiling command.
[Source: current console options](https://csr.ufmg.br/dokuwiki/doku.php?id=dinamica_console).

Native expression compilation is another performance variable. The current
Windows 8 Enhancement Plugin provides native compilation for calculation
functors. This is an installation/runtime prerequisite, not something a
replacement EGOML alone installs or guarantees.
[Sources: Enhancement Plugin for Dinamica EGO 8](https://csr.ufmg.br/dinamica/dokuwiki/doku.php?id=plugins_8_windows)
and [performance guidance](https://csr.ufmg.br/dinamica/dokuwiki/doku.php?id=useful_tips).

The local legacy MoFuSS baseline log independently recorded native-compilation
warnings and `_TABLE_VALUE_CAN_NOT_BE_COMPILATED_AS_NATIVE_CODE_` compiler
errors. The same-computer confirmation identifies this as the server's legacy
installation; those diagnostics still describe the tested invocation, not
every production job's compiler environment. Compare baseline and candidate under the same actual compiler
configuration; do not describe a benchmark with expression compilation disabled
as a measurement of the production native-compiled configuration.

## Validation coverage and remaining deployment checks

The executable/build, host, eight-processor ceiling, and launcher arguments
are resolved. Original and candidate execution, all four callbacks, and the
output comparison are complete for the isolated two-MC Nakuru fixture using
two workers and the documented case-local R adaptations. The checklist below
states the broader deployment criteria; it is not a renewed request for the
already supplied launcher or runtime version. See
[SMALL_CASE_VALIDATION.md](SMALL_CASE_VALIDATION.md) for what the completed
case does and does not cover.

1. Identify the exact executable/build, OS, launch arguments, scheduler/worker
   settings, compiler/plugin state, and R environment used by the web job.
2. Parse both original and candidate with that engine. A successful `-dont-run`
   check establishes parsing/validation only, not correct execution or outputs.
3. Execute both against isolated copies of the same representative web inputs,
   using production settings. Preserve canonical runs and protect production
   destinations from every map, table, log, and external-process write.
4. Verify file presence, names, table schemas and headers, raster dimensions,
   georeferencing, cell types and NoData conventions, MC counts, annual state,
   and R finalization. Include relevant optional branches. The user permits
   different random draws; that does not permit changed equations, parameter
   distributions, output contracts, or incomplete realizations. Use controlled
   runs for deterministic comparison and scientifically appropriate checks
   where stochastic scheduling prevents matching individual draws.
5. Measure elapsed time, memory, disk pressure, successful completion, and
   outputs at representative sizes. If claiming MC concurrency, demonstrate
   overlapping realizations on the target engine; processor settings alone
   are insufficient evidence. Account for concurrent web jobs when choosing a
   worker limit.
6. Retain the original script and a reversible replacement procedure. Publish
   measured target-server results and remaining limitations before declaring
   the replacement ready.

The local scheduler probes support an architectural decision. They are not
complete web-job certification and provide no basis for extrapolating
performance to 30 simultaneous MC realizations. The originally discussed
70-core hardware was subsequently corrected by the user to an eight-core
maximum on this computer.

## 2026-10-08: automatic core detection and per-job limits

This core-detection follow-up used read-only documentation and binary inspection.
It added no runtime probes, launcher changes, or simulation benchmarks. The
Linux-to-Windows handoff and subsequent resolution of the missing launcher are recorded in
[the server backup audit](SERVER_BACKUP_AUDIT.md).

### No verified core-count output functor

No functor returning the hardware core count or the job's CPU allocation was
verified in the inspected 2.4.1 help/registration log or the 2.4.1 and 8.13
binaries. The current official functor list also provides no such documented
getter. This is a finding about the inspected installations and sources; it
does not exclude a custom submodel or plugin on the server.
[Source: official functor list](https://dinamicaego.com/dokuwiki/doku.php?id=functor_list).

`ProcessorPolicy` is present in the local legacy runtime. Its optional inputs
are `maximumNumberOfProcessors` (nonnegative integer, default 0) and
`forceSpecifiedNumberOfProcessors` (Boolean, default false). It has **no output
ports**. It governs the processor limit for contained work; it neither reports
cores nor makes independent MC realizations run concurrently.
[Source: Processor Policy](https://dinamicaego.com/dokuwiki/doku.php?id=processor_policy).

The modern `GetEnvironmentValue` has a required **Key: Enum** input and a
**Value: String** output. It is a fixed enumeration, not a generic reader of
operating-system environment-variable names. The inspected 8.13 plugin contains
keys for model/application/user folders, temporary folders, OS, executable
extension, and application version. No CPU/worker-count or `OMP_NUM_THREADS`
key was found. Its implementation and registration were not found in the
inspected legacy 2.4.1 plugin/help. Adding it to the old EGOML is therefore not
a verified compatible way to discover a CPU budget.
[Source: Get Environment Value](https://dinamicaego.com/dokuwiki/doku.php?id=get_environment_value).

### Hardware detection is not an allocation

Official console documentation describes `-processors 0` as automatic core
detection and positive `-processors N` as an explicit worker-count override.
Automatic detection cannot establish a private allocation on a shared host.
The detected count, process restrictions, and the web job's authorized CPU
budget must be distinguished. Neither the worker setting nor a processor
policy reserves exclusive CPU cores for the job.
[Source: console options](https://www.dinamicaego.com/dokuwiki/doku.php?id=dinamica_console).

The Linux R process and remote Windows Dinamica process can have different
resource allocations. A Slurm allocation or a CPU count measured by Linux R
must not be treated as the Windows job's allocation without an explicit,
verified mapping. R's own `detectCores()` documentation warns that it does not
give the number of allowed cores. `OMP_NUM_THREADS` is an OpenMP control, not a
documented substitute for Dinamica's worker setting or a remote resource grant.
[Sources: R detectCores](https://www.stat.ethz.ch/R-manual/R-devel/library/parallel/html/detectCores.html)
and [OpenMP environment variables](https://openmp.org/spec-html/5.0/openmpch6.html).

Windows hardware queries also need care. Logical processors, physical cores,
and CPU sockets are different counts. On Windows 10 and server releases before
Windows Server 2022, an ordinary process defaults to one processor group, which
contains at most 64 logical processors. Windows 11 and Server 2022 changed that
default to span groups. This matters when a claimed allocation crosses the
64-logical-processor boundary; it is not evidence that every Dinamica build
has a fixed 64-core ceiling.
[Source: Microsoft processor groups](https://learn.microsoft.com/en-us/windows/win32/procthread/processor-groups).
The user's later eight-core correction removes this greater-than-64 case from
the current deployment scope.

Legacy `GetLogicalProcessorInformation` reports the calling thread's group;
`GetProcessAffinityMask` can report only a group/primary-group mask and has
special behavior for multi-group processes. The inspected 2.4.1 core DLL
contains these API names and `SetThreadAffinityMask`; group-aware counterpart
names were not found. These observations justify checking actual server
behavior, not extrapolating an affinity-mask bit count to the entire machine.
[Sources: GetLogicalProcessorInformation](https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getlogicalprocessorinformation)
and [GetProcessAffinityMask](https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getprocessaffinitymask).

Even a correct hardware total or allowed affinity set does not establish a
job's CPU-time quota. Windows Job Objects can impose CPU-rate limits separately
from processor placement.
[Source: Windows job CPU-rate control](https://learn.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-jobobject_cpu_rate_control_information).

### Adaptive concurrency rule

If a compatible concurrent-MC implementation is introduced and validated, use
the following conservative cap on the number of simultaneous realizations:

```text
MCWorkers = min(
    MC,
    floor(WindowsBudget / threadsPerMC),
    memoryLimit,
    IOLimit
)
```

`WindowsBudget` is the trusted allocation for this Windows job, reduced where
necessary for effective OS restrictions and verified runtime limitations.
`threadsPerMC` is a positive configured count. `memoryLimit` and `IOLimit` are
limits on concurrent realizations established from representative memory and
storage measurements. Account for other concurrently active stages and any
additional native-library threads. Launch only when the resulting cap permits
at least one realization; otherwise reduce the per-realization thread count or
report insufficient resources. Do not force a minimum that exceeds the budget.

There is no hard-coded 70-worker choice. For separate Dinamica processes, each
would receive its own explicit `-processors` limit; a single shared worker pool
does not guarantee a fixed private set of cores per realization.

The minimal integration point is the Windows launcher: obtain the trusted
allocation for the existing job ID, respect local restrictions, and pass the
appropriate positive `-processors` value without changing scientific inputs or
output files. During the first inspection the launcher was missing; it was
later supplied and confirms `-processors 0` on this eight-logical-processor
host. No per-job allocation reader has been verified. Automatic detection is
the production setting, but it does not account for four overlapping jobs.
Do not infer a private eight-worker allocation for each job from that setting,
or assume the Linux allocation transfers over SSH. Initial disposable tests
use an explicit two-worker limit while the other jobs remain active; record
that difference from production. A later sequential sweep may include 0
(production automatic detection) and 8 when the host has capacity.

## 2026-10-08: confirmed local host and R preparation prerequisites

This follow-up inspected installations, package metadata, package namespace
loading, and process/resource snapshots. It did not source a MoFuSS R script,
install a package, launch a simulation, send a notification, or control an
existing process. The user confirmed that this computer runs the server EGOML
and that eight cores is the maximum. R reports Windows 10 x64, build 19045;
`.NET Environment.ProcessorCount` reports eight available logical processors.
This does not independently establish the count of physical cores.

The only R installation found under `C:\Program Files\R` and in the inspected
program registry is `C:\Program Files\R\R-4.6.0`. Its explicit
`bin\Rscript.exe --vanilla` reports **R 4.6.0 (2026-04-24 ucrt),
x86_64-w64-mingw32**. R 4.5.0 was not found. Rscript is not on the inspected
PowerShell PATH, so a test case must use the verified explicit executable
path. The R libraries inspected were:

- `C:/Users/UNAM/AppData/Local/R/win-library/4.6`
- `C:/Program Files/R/R-4.6.0/library`

| Package or dependency | Installed version / observation |
| --- | --- |
| sf | 1.1-1; namespace loads |
| terra | 1.9-27; namespace loads |
| raster | 3.6-32; namespace loads |
| truncnorm | 1.0-9; namespace loads |
| msm | 1.8.2; namespace loads |
| ggplot2 | 4.0.3; namespace loads |
| rmarkdown / knitr / tinytex | 2.31 / 1.51 / 0.59 |
| Spatial libraries used by sf and terra | GDAL 3.12.1, PROJ 9.7.1, GEOS 3.14.1 |
| QGIS | 3.44.13, `C:\Program Files\QGIS 3.44.13` |
| RStudio | 2026.09.0+174 |
| RStudio's bundled Pandoc | 3.10, `C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools\pandoc.exe` |
| MiKTeX | `pdflatex.exe` found on the R process PATH |
| mapshaper / Node.js | Executables found on the R process PATH |

All 23 checked namespaces used by `rnorm_v3.R`, `maps_animations7.R`, and
`NRB_graphs_datasets2.R`, plus `truncnorm` and `ggplot2`, loaded successfully:
`msm`, `raster`, `tidyverse`, `readxl`, `readr`, `tibble`, `dplyr`, `animation`,
`data.table`, `fBasics`, `glue`, `jpeg`, `plyr`, `png`, `sf`, `tiff`, `spam`,
`tictoc`, `mapview`, `rmapshaper`, `foreach`, `truncnorm`, and `ggplot2`.
Namespace loading alone did not prove the legacy scripts worked with these
newer package versions. The later isolated full comparison completed all four
callbacks for both models with the documented R 4.6 fixture adaptations;
see [SMALL_CASE_VALIDATION.md](SMALL_CASE_VALIDATION.md).

The backup's broader `_setup_packages.R` bootstrap also requests unavailable
`tmap`, `osmdata`, `openxlsx`, and `readODS`, and the unavailable retired
packages `rgdal`, `rgeos`, and `maptools`. It was inspected as text, not sourced:
its final statement invokes an installer. These broader dependencies should
not be installed automatically merely to test a path that does not use them.
Optional packages `officer`, `flextable`, `gifski`, and `gganimate` were also
absent; they are not required by the inspected callback headers.

Standalone `rmarkdown::find_pandoc()` returned no available Pandoc, although
RStudio's bundled executable exists. Set a case-local path only if the tested
report uses Rmarkdown. The inspected backup's `LaTeX/generate_modern_report.R`
uses `pdflatex` directly. The backup contains `ffmpeg64/bin/ffmpeg.exe` and
`ffmpeg32/bin/ffmpeg.exe`, and its animation callback expects these relative
to the case directory. Preserve those files when staging the case. R startup
reported failures to set several `C.UTF-8` locale categories; record the
effective locale and verify text/CSV outputs in the full test rather than
silently assuming identical Linux and Windows locale behavior.

Both installed Dinamica `mingw/bin/gcc.exe` executables report **GCC 4.4.4
20100212 (prerelease)**. The separate Rtools45 compiler at
`C:\rtools45\x86_64-w64-mingw32.static.posix\bin\gcc.exe` reports GCC 14.3.0.
Compiler presence is not successful native-expression compilation, and the
Rtools compiler is not a verified substitute for Dinamica's bundled compiler.
Keep compiler settings identical for baseline/candidate comparisons and
inspect compilation diagnostics before attributing speed differences.

### Resource snapshot and initial concurrency

At approximately 07:50-07:56 local time, four existing processes used
`C:\Program Files\Dinamica EGO\DinamicaConsole.exe` (PIDs 4172, 27068, 34924,
38024). All had an affinity mask of 255, allowing the same eight logical
processors. In a two-second sample, their combined CPU-time increase was
approximately **3.64 busy logical-processor equivalents**; their combined
working set was approximately **3.85 GiB**, and private memory approximately
**4.24 GiB**. This short observation is neither their peak demand nor a
guarantee of future idle capacity.

`ps::ps_system_memory()` reported **63.83 GiB total RAM** and **34.56 GiB
available**. Filesystem metadata reported approximately 1,654 GiB free on the
temporary E: volume. These are transient snapshots, not resource reservations.

While the four processes continue, a conservative starting point is one small
preparation/baseline job with at most **one or two Dinamica workers**, subject
to monitoring and the total eight-core ceiling. Do not start a sweep of
concurrent test jobs based solely on the momentary spare CPU count. For useful
timing comparisons, run original and candidate under stable, comparable
background load, preferably after these jobs finish; preserve their processes
and output directories. The later worker sweep should measure 1, 2, 4, and 8
workers sequentially only when the shared host has capacity, and must not be
presented as isolated scaling evidence if competing workloads are active.

## 2026-10-08: native IDW source-cell probe

Before treating the supplied `IDW_Sc3.egoml` as a possible generator of a small
test fixture, an isolated synthetic probe checked its origin behavior. This
probe is not a test of server C++ IDW equivalence, representative performance,
or complete MoFuSS simulation outputs. No production input or model was edited.

The graph was copied byte-for-byte from
`localhost/scripts/older_versions/IDW_Sc3.egoml` in the canonical repository.
Its source and copied SHA-256 were both
`e8e66b044df241c9ca1c0b4aaf18c922edb9af13a52b27b3ecaf9192d9c10b3e`.
It retained the supplied one-pass cost calculation, diagonal penalty, relative
friction, exponent 1, float32 accumulation, and cutoff of 1,209,600 seconds.
The original two SaveMap nodes produced `In/Indice_w.tif` and
`In/Indice_v.tif`; no output node or numerical guard was added.

Synthetic inputs were a 3 by 3 grid of one-metre cells, extent `[0,3,0,3]`,
tagged EPSG:32614, with friction 1 at all nine cells. Each channel's category
map contained category 1 only at the centre cell and NoData elsewhere. Its
two-column lookup table gave that category positive demand 100. All inputs,
temporary compiler files, model copy, and outputs were confined to
`E:\MoFuSS_Active\webmofuss_performance_audit\native_idw_origin_probe`.

The verified legacy executable ran with `-processors 1 -log-level 4`, private
TEMP/TMP/TMPDIR, and `OMP_NUM_THREADS=1`. It returned exit code zero and reported
successful model completion. Observed wall-clock engine time was **5.103
seconds**, including startup and any compilation, while other host jobs were
active. This single tiny observation is not a scaling measurement.

Both channels returned the same float32 pressure raster:

```text
70.71067810058594  100                 70.71067810058594
100                NoData            100
70.71067810058594  100                 70.71067810058594
```

There were eight valid finite pixels, ranging from 70.71067810058594 to 100.
Terra read the centre as NaN. Independent raw Rasterio/GDAL inspection showed
that the stored centre value was **-9999**, matching the declared GeoTIFF NoData
value and an invalid mask at that cell. There were **no stored Inf or NaN
values**. The two output GeoTIFFs were byte-identical, with SHA-256
`f8d04c51b1c0aafc890671a5ef075050d4a169831b20253fb7527c7aa3b4bbf6`.

The log also reported, for each channel, `Cost map is NOT optimum (More than 1
pass required).` The official cost-map documentation identifies source cells
with zero accumulated cost and distinguishes a fixed pass count from the
zero setting that iterates to an optimized result. The supplied pressure
formula divides positive demand by cost without a source-cell guard. These
facts explain why a successful execution does not establish a suitable
complete pressure surface; no formula or pass-count correction was applied.
[Source: Calc Cost Map](https://dinamicaego.com/dokuwiki/doku.php?id=calc_cost_map).

The native fallback was rejected for the complete annual fixture following
this probe. In particular, do not silently replace the missing
source pressure or claim equivalence with the separate server C++ engine.
This one-origin probe does not establish behavior for dense or multiple-origin
maps. The completed fixture instead used the unchanged official C++ source
under the declared settings documented below; the unavailable production
wrapper remains a separate provenance limit.

Disposable reproducibility evidence includes `setup.R`, `inspect.R`,
`model.egoml`, `engine.log`, `evidence.json`, `raster_inspection.json`,
`raw_raster_inspection.json`, and the two `pressure_cells_*.csv` files in the
probe directory. This section preserves the critical source, inputs, observed
values, hashes, and limits if temporary evidence is later deleted.

## 2026-10-08: official C++ IDW build and smoke test

The official MoFuSS `CostDistance_IDW` repository documents Windows builds
using MinGW and GDAL and supplies OpenMP variants. A bounded snapshot of 22
source/license/build files (103,388 bytes) was fetched at commit
`cdb1c36453f3aa6d9906c26526a8d63f5bfd9964`, under the existing temporary task's
`costdistance_source` directory. The native EGOML fallback was not used on the
real prepared fixture after its source-cell probe failed the intended gate.
[Source: official CostDistance_IDW repository](https://github.com/mofuss/CostDistance_IDW).

The original `OMP_specificYear` C++ sources built successfully with the
installed Rtools45 GCC 14.3.0 compiler and its GDAL 3.12.1 static libraries. No
system package was installed. Missing TCLAP command-line-parser headers were
kept private under the temporary task: stable version 1.2.5, mirror commit
`58c5c8ef24111072fc21fb723f8ab45d23395809`. TCLAP is a header-only library.
[Sources: TCLAP project](https://tclap.sourceforge.net/)
and [pinned parser release](https://github.com/mirror/tclap/tree/58c5c8ef24111072fc21fb723f8ab45d23395809).

The build changed no upstream `.cpp` or `.h` file. Five private forwarding
headers adapted the source's `gdal/...` include names to the installed SDK's
flat header directory. Compilation used `-std=c++17 -O2 -fopenmp -static
-static-libgcc -static-libstdc++`; the static library list came from the local
SDK's `lib/pkgconfig/gdal.pc` and was enclosed in a linker start/end group.
The SDK `bin` directory was added only to the compiler child's PATH so its
assembler could be found. An initial attempt without that PATH prefix failed;
the second build succeeded. The compiler warned that upstream raster read/write
return codes are ignored. Input and output checks are therefore required around
this executable. WSL was not available for this task (`--status` returned 50).

The resulting temporary executable is
`E:\MoFuSS_Active\webmofuss_performance_audit\costdistance_source\build_specific_year\CostDistance_IDW_specificYear.exe`,
with SHA-256
`0e91b219a3b2150ee125cf36d39b437477a7b7a056180579995c6cb2301a2fb0`.
Its exact compiler command, source/dependency hashes, and diagnostics are in
`snapshot_manifest.json`, `tclap_manifest.json`, and
`build_specific_year/build_attempt2_manifest.json` plus its build log.

### Input layout, units, and scenario settings

The inspected program's `-1`, `-2`, and `-3` options take walking friction,
origin raster, and demand CSV; `-4`, `-5`, and `-6` take their vehicle
counterparts. `-r` selects relative friction, `-p` selects worker count, `-t`
is the exploration limit in hours, `-e` is the IDW exponent, and `-y` is a
one-based demand-column index after the ID column. It is not a calendar year.
Output naming uses characters 6 through 9 of the selected column header and
the two-digit index, requiring headers such as `2000_fw_w` and `2000_fw_v`.
[Source: pinned command-line and execution code](https://github.com/mofuss/CostDistance_IDW/blob/cdb1c36453f3aa6d9906c26526a8d63f5bfd9964/OMP_specificYear/main.cpp).

Do not pass annual `Key,Value` lookup CSVs directly as if they were the full
demand tables. Although index 1 can select their numeric values, the `Value`
header supplies no channel suffix and both channels would use the same output
name. Higher indices exceed a two-column table. Prefer the generated full
`BaU_fwch_w.csv` and `BaU_fwch_v.csv` with their actual annual column names and
indices; validate column count and ID coverage before execution.

The C++ program divides every selected demand value by 1,000. The supplied
`3_demand4IDW_v8.R` derives its full CSVs from tonne-valued demand rasters;
`6a_scenarios.R` copies those values into annual lookup tables without scaling.
For the isolated validation fixture, multiplying only a **copied C++ input
CSV's demand columns by 1,000** supplies kilograms to the existing division and
retains the intended tonne demand. The original model demand tables must stay
unchanged. Preserve source/converted hashes, original headers, mapped years,
and that exact scale in the fixture provenance. This case-only unit adaptation
was explicitly authorized during the audit.

The absent `idwW.R` wrapper still prevents proving the server's selected source
variant, numerical switches, or unit adaptation. The small local test uses an
explicitly declared scenario of 12 hours, exponent 1, relative friction, and
one worker; these settings are not represented as recovered server settings.

### Two-year, one-origin smoke result

The compiled executable's `--help` completed successfully. A new isolated
3 by 3 test then used one-metre cells, positive friction 1, EPSG:32614, an
origin of category 1 at the centre, and matching -9999 input NoData metadata.
Full-layout CSV headers were `ID,2000_fw_w,2001_fw_w` and their vehicle
counterpart. The two demand values were 100,000 and 200,000 kg, becoming 100
and 200 tonnes inside the unchanged program.

Separate invocations with `-y 1` and `-y 2` both returned zero and reported
completed walking and vehicle scenarios. Year 1, for both channels, was:

```text
70.71067810058594  100                 70.71067810058594
100                50                100
70.71067810058594  100                 70.71067810058594
```

All nine pixels were valid, finite, and positive, with no stored Inf, NaN, or
NoData pixels. Year 2 was exactly twice year 1 in every float32 cell. Both
outputs preserved the input grid and CRS. The centre value reflects the
original C++ algorithm's return-path cost behavior; it was not replaced with
an invented source value. The year-1 W/V TIFF hash was
`f3ab82ab5e5de26f1a59fc8b3974a4056c81bb83d639699289412b3f74d21c26`;
the year-2 W/V hash was
`2227aecd34f9a6746ff0cc24239d6f79f109717328bf6a460c05008fc27060d6`.

Observed wall times were 0.740 and 0.690 seconds including process startup.
They are tiny smoke-test observations under competing load, not representative
IDW or MoFuSS performance estimates. The successful build and smoke test
support testing the official algorithm on the prepared small case; they do
not certify its equivalence to the unavailable production wrapper. Temporary
evidence is in `costdistance_source/origin_smoke/evidence.json`, `help.log`,
`year_1.log`, `year_2.log`, the two input CSVs, and input/output rasters.

### Bounded annual C++ fixture adapter

`tests/prepare_cpp_idw_fixture.py` stages the compiled, hash-pinned program's
inputs and runs annual indices sequentially in a disposable case beneath
`E:/MoFuSS_Active`. It defaults to two workers and the declared 12-hour,
exponent-1 scenario. No executable is downloaded or installed by the adapter.
The prepared national BaU CSVs inspected on October 8 were 102,376,096 bytes
(walking) and 97,643,700 bytes (vehicle), with columns 2000 through 2050.
The adapter streams these tables, retains every row and original header, and
multiplies only private copies of annual demand values by 1,000. It permits
national table IDs outside the cropped raster but requires every raster
category to have a table row. Duplicate, fractional, or float32-inexact
category IDs are rejected before execution.

The upstream reader converts NoData to an integer and compares origin values
against the friction map's NoData value. Therefore private raster copies use
the same -9999 sentinel. The adapter verifies exact preservation of every
valid value, the NoData mask, grid geometry, and CRS; float64 private storage
avoids truncating original valid values. Original case maps and demand tables
remain unchanged. A positive friction domain, unique integer origin cells,
matching geometry, and bounded map size are preconditions. Every run records
source, private-input, executable, and output hashes, return status, log, and
duration. Both output channels must pass finite/nonnegative pressure and
positive-demand origin checks before installation under the expected annual
filenames. Continuation beyond index 1 requires an explicit recorded review
of that successful first period; existing unverified outputs are never
overwritten.

A separate 3 by 3 adapter smoke case used NaN NoData metadata, one NoData
corner, one central origin, tonne-valued 100/200 annual demand, and one unused
CSV category. Staging preserved exact values/masks and generated the expected
100,000/200,000 kg private values. Both years produced eight finite positive
cells, preserved the NoData corner, and retained source-cell pressure. Year 2
was exactly twice year 1. The review gate blocked premature continuation, and
rerunning completed periods skipped only hash-verified outputs. Two-worker
process durations were 0.580 and 0.721 seconds; these are functional smoke
observations, not representative performance measurements. The first-period
W/V TIFF hash was
`cbd3e725bfd0669013515c098d7b3679dedae116dfbdd6f41a3938f9485833fb`.
Temporary evidence is in
`costdistance_source/adapter_smoke_case/adapter_validation.json` and its
`_cpp_idw` manifests/logs. The real prepared case had not been staged or run
with this adapter when this smoke evidence was recorded.

### Rectangular prepared grid: retain the original cost convention

The completed small-case harmonizer produced a 33 by 33 EPSG:3395 grid with
pixel width 1011.9953708479633 m and height 1005.235817016008 m (width/height
1.0067243463847326). Walking friction and origin maps have exactly matching
transforms and CRS. The adapter's initial square-cell precondition rejected
this grid before staging writes; inspection showed that restriction was
stronger than the original program requires, so it was removed. Rotated,
non-metre, or mismatched grids remain rejected.

The pinned C++ `Raster.cpp` assigns `scale = adfGeoTransform[1]`, retaining it
as a float. `main.cpp` multiplies all orthogonal movement friction by that
pixel width, and diagonal movement by width times sqrt(2). Output creation
writes the full unchanged geotransform, including pixel height. Thus the
adapter accepts rectangular cells **while explicitly preserving this original
width-based cost calculation and full map geometry**. It records width,
height, aspect ratio, and the float32 cost scale in the input/output metadata;
it does not resample or modify the C++ algorithm.
[Pinned raster reader/writer](https://github.com/mofuss/CostDistance_IDW/blob/cdb1c36453f3aa6d9906c26526a8d63f5bfd9964/OMP_specificYear/Raster.cpp),
[pinned cost calculation](https://github.com/mofuss/CostDistance_IDW/blob/cdb1c36453f3aa6d9906c26526a8d63f5bfd9964/OMP_specificYear/main.cpp).

This must not be described as a universal Dinamica convention. Both the
installed 2.4 help and the official `Extract Map Attributes` documentation
identify table index 5 as cell height and index 6 as cell width. The original
web EGOML uses `t1[5]^2` in its harvest-threshold rescaling. Those existing
calculations remain untouched; no equivalence between the C++ cost algorithm
and Dinamica's native cost functor is implied.
[Extract Map Attributes](https://dinamicaego.com/dokuwiki/doku.php?id=extract_map_attributes).

A separate synthetic 3 by 3 test with two-metre width and one-metre height
preserved its complete rectangular transform and NoData mask. All eight
valid output pressures were finite, positive, and exactly half those from
the otherwise identical one-metre-width control: centre 25, maximum 50.
That confirms the inspected width convention without changing an input
value or resampling. Runtime was 0.478 seconds with two workers, a functional
smoke observation only. Evidence is in
`costdistance_source/adapter_rectangular_smoke_case/adapter_validation.json`.
No actual prepared-case C++ run was performed as part of this correction.

### Retain origins whose own friction cell is NoData

Further preflight inspection found 78 of 816 walking origins and 48 of 858
vehicle origins outside their respective valid friction masks. Removing those
origins would discard original demand. The pinned C++ code intentionally
starts each selected origin at cost zero without requiring its own friction
cell to be positive, then enters positive-friction neighboring cells. Negative
friction cells remain NoData in the output. The adapter therefore preserves
all original origins and demand rows, records the outside-origin IDs/counts,
and applies the positive source-cell pressure requirement only to active
origins inside the valid friction domain. Its unchanged output-mask equality
check verifies that outside-origin cells remain NoData. It never adds edges
with negative friction or fills those cells.
[Original source initialization, neighbor traversal, and output masking](https://github.com/mofuss/CostDistance_IDW/blob/cdb1c36453f3aa6d9906c26526a8d63f5bfd9964/OMP_specificYear/main.cpp).

A new 3 by 3 synthetic check placed its sole origin (100 tonnes demand) on
the central NoData friction cell, surrounded by eight unit-friction cells.
The unchanged C++ program produced eight finite positive values: orthogonal
neighbors 100 and diagonal neighbors 70.71067810058594. The origin remained
NoData. Original input hashes were unchanged, and the manifest/result recorded
one active outside origin and zero active inside origins. The two-worker run
took 0.598 seconds; this is functional evidence only. Evidence is in
`costdistance_source/adapter_outside_origin_smoke_case/adapter_validation.json`.
No actual prepared-case run was performed for this check.

The adapter also compares CRS meaning using `rasterio.CRS` equality while
requiring exact raster dimensions and affine transforms. This permits the
legacy GDAL writer's differing descriptive WKT names for the same EPSG:3395
projection; the manifest retains both original WKT strings. No raster is
reprojected or resampled by these compatibility adaptations.
