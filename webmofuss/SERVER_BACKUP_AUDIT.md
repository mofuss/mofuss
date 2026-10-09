# Read-only server-backup launch audit

Inspected on 2026-10-08. The backup inspection changed no backup files,
executed no backed-up launcher, and contacted no remote services. Later local
validation ran isolated copies under the authorized temporary directory.
This document distinguishes the initial incomplete inventory from subsequently
supplied evidence; it does not assume every backup file is current on the
running server. Private remote identities, addresses, and authentication values
are intentionally omitted.

**Current status:** the Windows launcher and runtime identification are
resolved: this same eight-logical-processor computer runs Dinamica
2.4.1.20140602 with `-processors 0`. A fresh small real-data case and all 51
annual official-C++ IDW pairs are complete. Original and guarded candidate
both completed two MC realizations over 2000–2050, including all four R
callbacks, with matching scientific outputs and report content. See
[SMALL_CASE_VALIDATION.md](SMALL_CASE_VALIDATION.md) for the full evidence and
fixture limitations. The original production `idwW.R` wrapper remains absent;
its exact preparation settings are not claimed to have been recovered.

## Inspected locations and model identity

- Backup root: `F:\webmofuss_speedup_sandbox`.
- Submitted Linux job: `uploads\6ac6d40040271.slurm`.
- Source tree: `mofuss\mofuss\localhost\scripts`.
- Additional local preparation entry point:
  `mofuss\000_main_localhost_v1.R`.
- Intended temporary case location: `mofuss\temp_mofuss`.
- Canonical audited source:
  `C:\Users\UNAM\Documents\mofuss\webmofuss\7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml`.

The backup's
`mofuss\mofuss\localhost\scripts\7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml`
is byte-identical to that canonical original: both are 353,116 bytes and have
SHA-256
`e4ce6ab47a12bbc7ed1f290a95ad6c1477182d21e95fba1428d816ecc85bca2d`.
The existing static graph audit therefore applies to this backup model without
needing to infer compatibility from a similar filename.

At inspection time, `mofuss\temp_mofuss` was empty and `uploads` contained only
the submitted `.slurm` file. There was no prepared session directory containing
the job's `In`, `Temp`, and `LULCC/TempRaster` inputs. A recursive filename
inventory, including hidden files, initially found no `run_dinamica` launcher
and no `.cmd` or `.ps1` files anywhere in the backup. The launcher was later
supplied under `scripts`, as documented below. The old land-use preprocessing
`.bat` files in the source tree are different scripts; they do not supply the
referenced main Windows launcher.

## Actual launch sequence visible in the backup

Line references in this section refer to `uploads\6ac6d40040271.slurm`.

1. Lines 4–5 request one Slurm task and eight CPUs for that task:
   `#SBATCH --ntasks=1` and `#SBATCH --cpus-per-task=8`. Line 10 exports
   `OMP_NUM_THREADS=8` in the Linux job environment.
2. Lines 13–35 copy the source scripts into the session, prepare directories and
   source-data links, and create a `.env` containing job paths and reporting
   configuration. The generated `.env` has no core/thread/worker budget field.
3. Lines 38–39 start the Linux container with Apptainer and run
   `Rscript --vanilla 0_main.R` in the session's `scripts` directory.
4. Lines 42–92 invoke `idwW.R` for 51 periods. Each visible invocation includes
   the numeric argument `8`; these lines concern the Linux IDW preparation
   phase. They do not launch the MC simulation.
5. Line 95 copies `OStype.csv` and `Rpath.csv` into the session's
   `webmofuss/LULCC/TempTables` directory. The source copies of these two files
   are not present in this backup, so their actual configured values cannot be
   established here.
6. Lines 101–102 hand off to a Windows machine by SSH. The relevant remote
   command is `C:\scripts\run_dinamica.cmd 6ac6d40040271`: the only visible
   positional argument is the session ID. No core count, `-processors` option,
   or thread environment assignment is passed in this command.
7. Lines 103–110 re-enter the Linux container and create the download archive
   after the remote calls return. The archive includes `Summary_Report`, `Out`,
   `demand_atlas`, and prepared demand outputs.

During this initial inspection the main `DinamicaConsole.exe` invocation was
hidden inside the then-absent `C:\scripts\run_dinamica.cmd`. The subsequently
supplied launcher resolved the executable path, original model filename,
network-share working directory, and `-processors 0 -log-level 4` options;
its limitations are recorded in the final launcher section below.

The nested source tree's `0_main.R` is the R entry point invoked by this Slurm
job, not either `000_main_localhost_v1.R` file. Its lines 72–84 list preparation
scripts ending with `6d_parameters_dinamica_v1.R`. It does not launch the main
Dinamica model. The standalone `000_main_localhost_v1.R` files are interactive
local preparation workflows and are not evidence of the Windows
orchestration.

For completeness, `6a_scenarios.R` contains auxiliary Windows engine calls:
line 543 builds a `DinamicaConsole.exe -processors 0 -log-level 4` invocation for
`Friction3.egoml`, and line 550 does the same for `IDW_Sc3.egoml`, under their
respective branch conditions. Neither call targets the main simulation EGOML;
their `-processors 0` setting alone does not establish the main-run setting.
The supplied Windows launcher independently confirms that setting for the
main run.

## Core-budget conclusion

The observed eight-core setting is a **Linux Slurm allocation**. Its cgroup and
`OMP_NUM_THREADS` environment do not by themselves allocate or limit resources
inside a separately launched Windows process. The visible handoff does not
explicitly carry that budget onward. The later supplied Windows launcher uses
automatic processor detection on the confirmed eight-logical-processor host;
it does not read a separate per-job CPU budget.

There is no existing model-input field in this backup that communicates the
chosen core budget:

- `mofuss\mofuss\parameters\parameters.csv` has columns `Var,ParCHR` and 141
  data rows. Its variable names include `monte_carlo_runs` at line 30 but no
  CPU/core/thread/worker/parallelism setting. MC count is the number of
  realizations, not a resource allocation.
- `00_webmofuss.R`, lines 33–48, reads the `.env` path/reporting fields. It does
  not read a compute-budget variable.
- `6d_parameters_dinamica_v1.R`, lines 91–115, writes
  `LULCC/TempTables/parameters_dinamica.csv` with exactly five variable rows:
  `start_year`, `end_year`, `monte_carlo_runs`, `uncapped_regrowth`, and
  `npa_ease`. None carries a CPU budget.
- In the original EGOML, the string-key parameter lookups are
  `monte_carlo_runs` (`getTableValue1911`, line 4102), `npa_ease`
  (`getTableValue1928`, line 4130), `end_year` (`getTableValue1943`, line 4159),
  and `uncapped_regrowth` (`getTableValue1954`, line 4188). There is no CPU-budget
  lookup. The remaining `GetTableValue` calls use numeric key `1` for other
  tables such as the OS/R-path and report-support tables.

Thus a proposed MC process pool cannot safely assume that the web user's Linux
core choice is already available to the simulation EGOML. The actual launcher
setting is now known, but automatic detection is not a private allocation when
other Windows jobs are active. No new required model parameter was introduced;
the requested web input interface remains unchanged.

## Potential duplicate launch and ineffective retry

The submitted Slurm file contains two consecutive invocations of the same
Windows command:

- Line 101 makes an unconditional SSH call to the launcher for the session.
- Line 102 starts an `until ssh ...` loop that invokes the same launcher again,
  with up to five attempts and a ten-second delay between failed attempts.

If the first call returns success, execution reaches line 102 and invokes the
launcher a second time. The subsequently supplied launcher starts the model
and contains no completed-job guard, so this command sequence invokes it
twice when both SSH calls succeed. Production logs were not supplied to
measure actual duplicate runs; this is a launch-code finding, not an observed
production frequency.

There is also a retry-control issue: line 7 enables shell `set -e`. The first
SSH call on line 101 is not protected by the `until` condition. A nonzero exit
there can terminate the job before execution reaches the retry loop. Therefore
the retry loop does not protect the first call in the straightforward shell
execution path.

The subsequent launcher inspection found no completed-job guard or explicit
propagation of the Dinamica exit status. No launcher correction or backup
modification was made during this audit.

## Resolved evidence and remaining limits

The actual Windows launcher and executable version were subsequently supplied
or verified; they are no longer missing inputs. A complete fresh prepared
small case and successful original/candidate runs now provide local execution
and output evidence. They do not recover a historical server session or the
absent `idwW.R` wrapper's configuration. The declared official-C++ fixture
settings, local R adaptations, branch coverage, and uncontrolled background
load are documented in [SMALL_CASE_VALIDATION.md](SMALL_CASE_VALIDATION.md).
Production logs would still be needed to establish duplicate-launch frequency.

## Later partial case folders

A subsequent read-only inspection found three additional folders under
`F:\webmofuss_speedup_sandbox\webmofuss_files`:

| Case ID | Files present at inspection | Relevant content |
| --- | --- | --- |
| `6840ecfcad627` | 72 | 15 source TIFFs and 17 CSVs, plus setup assets |
| `68e88410b267d` | 40 | One source elevation TIFF and three utility CSVs, plus setup assets |
| `68e895bd7eca3` | 41 | One source elevation TIFF and three utility CSVs, plus setup assets |

Each case root contained only `LULCC` and `desktop.ini`. Each
`LULCC/TempRaster` contained only `desktop.ini`. None contained a model EGOML,
runtime log (`.log`, `.out`, or `.Rout`), Windows `.cmd`/`.ps1` launcher,
`parameters_dinamica.csv`, or the job's root `In`, `Temp`, `Out`, and
`Summary_Report` directories. These are partial source/setup copies, not yet
run-ready or evidence of a completed simulation. PDFs and presentation files
under `LULCC/Wizard_imgs` are bundled interface/help assets, not generated job
reports. No new model-variant hash comparison is possible without an EGOML in
these folders.

All three cases contain the same callback configuration:

- `LULCC/TempTables/Rpath.csv`, line 2: key `1` points to
  `c:\Program Files\R\R-4.5.0\bin\x64\R.exe`.
- `LULCC/TempTables/OStype.csv`, line 2: key `1` has value `64`.
- `LULCC/TempTables/Rpath.txt`, line 1: `/usr/bin/R`.
- `LULCC/TempTables/OS_type.txt`, line 1: `64-bit`.

The CSV therefore configures Windows R 4.5.0 for the main EGOML's R callbacks,
while the TXT files record Linux-style discovery values. This does **not**
identify the Dinamica executable, its version, processor setting, or successful
execution. Even the R version is evidence of a configured pathname, not a
captured runtime banner.

The discovery/replacement sequence explains why these values can coexist:

1. In the backed-up source, `2_copy_files_v1.R`, lines 117–121, runs
   `LULCC/RpathOSsystem2.bat` on Windows or `RpathOSsystem2.sh` on Linux.
   It reads `Rpath.txt` at line 125 and writes `Rpath.csv` at line 133; it writes
   `OStype.csv` at line 146 or 175 after reading the OS TXT value. Running this
   preparation again could therefore regenerate the CSVs for the current OS.
2. The supplied Slurm script subsequently copies central `Rpath.csv` and
   `OStype.csv` into the job at line 95, before the Windows handoff. That
   replacement leaves the earlier TXT discovery files untouched. The partial
   cases' mixed values are consistent with this launch design, although those
   case IDs differ from the supplied Slurm job and their individual launch
   histories are absent.
3. The canonical web EGOML itself has no `.bat` invocation. It loads
   `LULCC/TempTables/Rpath.csv` at line 3917 and reads key `1`, column `Rpath`,
   through `getTableValue4000` (line 3924, output `v263`). Initialization node
   `runExternalProcess2510` (line 4217) uses `v263` directly. The three report
   callbacks receive the same path through `String` ports `v1` and `v332`.
   None of these four calls reruns the path-discovery batch script.

No preprocessing was rerun, no files in these partial cases were changed, and
no evidence of actual Dinamica engine version or repeated/completed simulation
became available from that partial-case inspection.

## Windows launcher supplied on 2026-10-08

The user subsequently supplied `scripts/run_dinamica.cmd` in the backup and
confirmed that the Windows simulation runs on this same computer, with a
maximum of eight logical processors. The launcher selects
`C:\Program Files\Dinamica EGO\DinamicaConsole.exe` and the canonical v3 model,
using `-processors 0 -log-level 4`. The selected executable was independently
measured as version **2.4.1.20140602**. The earlier informal version description
is therefore resolved for this launch path. Processor value zero already asks
the engine to select its available processors; it does not make independent
Monte Carlo iterations execute concurrently on this legacy scheduler.

The launcher mounts a network share and runs the case from that share. It has
no completed-job check, locking, or explicit propagation of the Dinamica exit
code. Its embedded authentication details are deliberately not reproduced in
this audit. The launcher was inspected, not executed.

This strengthens the duplicate-launch finding above: the supplied Slurm file
contains an unconditional launch followed by another launch in the retry loop,
and this CMD script does not skip an already completed job. When both SSH calls
succeed, the command sequence invokes the model twice. Actual production logs
are still needed to establish how often that occurs. Correcting the Slurm
sequence is a separate server change, not an EGOML optimization.

A disposable small-area case was prepared from the supplied global datasets
under `E:\MoFuSS_Active\webmofuss_performance_audit\small_area_case`. All 51
annual IDW pairs and both complete simulation runs have finished. For the
Nakuru 33 × 33 case (two MC realizations, 2000–2050), all 4,074 scientific
files matched byte for byte: 1,752 TIFFs and 2,322 CSVs. Two GeoPackages matched
semantically, all 5,804 produced filenames matched, the animation matched,
and all ten rendered PDF pages and extracted text matched. PDF metadata
varied. All four R callbacks completed in both runs.

Original and guarded-candidate durations were 546.302 and 708.428 seconds,
respectively, with four other jobs active. These observations do not establish
an isolated performance comparison or speedup. The complete record is in
[SMALL_CASE_VALIDATION.md](SMALL_CASE_VALIDATION.md). The source backup and
production launcher remain unchanged.
