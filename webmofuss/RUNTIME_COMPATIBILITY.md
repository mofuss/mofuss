# Runtime compatibility and parallel-execution evidence

Audit date: 2026-10-07, local time. Scope: the existing web-server model
`7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml`, preserving its inputs, scientific
calculation, and output contract. This document records runtime evidence; it
does not certify a replacement for deployment or report a MoFuSS speedup.

## Exact versions and the server-version question

Two separately installed Windows runtimes were inspected and exercised:

| Executable | Version reported by the executable | Build |
| --- | --- | --- |
| `C:\Program Files\Dinamica EGO\DinamicaConsole.exe` | `Dinamica EGO 64, 2.4.1.20140602 (Awesometastic)` | `1401730828` |
| `C:\Users\UNAM\AppData\Local\Programs\DinamicaEGO-8.13\DinamicaConsole8.exe` | `Dinamica EGO 8, 8.13.0.20260827 (Nightingale Nuggets)` | `631132458` |

The user identifies the server version as **2.11**. Neither local executable is
that version. The primary sources inspected did not establish a release named
2.11, 2.1.1, or 2.0.11. This is an unresolved identification question, not proof
that such a build does not exist. Do not silently reinterpret it as 8.11 or
substitute local 2.4 results for server results. Capture the literal server
executable path, `-version` output, operating system, and launcher command.

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
errors. These concern that local 2.4.1 run and do not establish the server's
compiler state. Compare baseline and candidate under the same actual compiler
configuration; do not describe a benchmark with expression compilation disabled
as a measurement of the production native-compiled configuration.

## Gate before replacing the server script

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
target-server certification and provide no basis for extrapolating performance
to 30 simultaneous MC realizations or to 70-core hardware.

## 2026-10-08: automatic core detection and per-job limits

This follow-up used read-only documentation and binary inspection. It added no
runtime probes, launcher changes, or simulation benchmarks. The discovered
Linux-to-Windows handoff and missing Windows-side files are recorded in
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
output files. An actual allocation reader cannot yet be implemented against
the production system because `C:\scripts\run_dinamica.cmd` and its allocation
source are missing from the supplied backup. Preserve the existing configured
limit while those details are unresolved; do not replace it with unrestricted
hardware detection or assume the Linux allocation transfers over SSH.
