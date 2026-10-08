# Read-only server-backup launch audit

Inspected on 2026-10-08. No backup files were changed, no scripts were executed,
and no remote services were contacted. This document records the files present
at inspection time; it does not assume the backup is complete or current on the
running server. Private remote identities, addresses, and authentication values
are intentionally omitted.

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
inventory, including hidden files, found no `run_dinamica` launcher and no
`.cmd` or `.ps1` files anywhere in the backup. The old land-use preprocessing
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

The actual `DinamicaConsole.exe` invocation for the main model is hidden inside
the absent `C:\scripts\run_dinamica.cmd`. This backup cannot establish the
executable version, main-model filename selected by that launcher, working
directory, file-transfer/mount arrangement, processor policy, concurrency
settings, or whether the launcher waits for the full simulation and reports.

The nested source tree's `0_main.R` is the R entry point invoked by this Slurm
job, not either `000_main_localhost_v1.R` file. Its lines 72–84 list preparation
scripts ending with `6d_parameters_dinamica_v1.R`. It does not launch the main
Dinamica model. The standalone `000_main_localhost_v1.R` files are interactive
local preparation workflows and are not evidence of the missing Windows
orchestration.

For completeness, `6a_scenarios.R` contains auxiliary Windows engine calls:
line 543 builds a `DinamicaConsole.exe -processors 0 -log-level 4` invocation for
`Friction3.egoml`, and line 550 does the same for `IDW_Sc3.egoml`, under their
respective branch conditions. Neither call targets the main simulation EGOML;
their `-processors 0` setting must not be attributed to the server's main run.

## Core-budget conclusion

The observed eight-core setting is a **Linux Slurm allocation**. Its cgroup and
`OMP_NUM_THREADS` environment do not by themselves allocate or limit resources
inside a separately launched Windows process. The visible handoff does not
explicitly carry that budget onward. The Windows launcher might obtain a budget
elsewhere, but that cannot be confirmed without the launcher and relevant
configuration.

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
core choice is already available to the simulation EGOML. The actual Windows
resource contract remains unknown. No new required parameter should be invented
before inspecting the launcher; doing so would violate the intended unchanged
web input interface.

## Potential duplicate launch and ineffective retry

The submitted Slurm file contains two consecutive invocations of the same
Windows command:

- Line 101 makes an unconditional SSH call to the launcher for the session.
- Line 102 starts an `until ssh ...` loop that invokes the same launcher again,
  with up to five attempts and a ten-second delay between failed attempts.

If the first call returns success, execution reaches line 102 and invokes the
launcher a second time. If the launcher starts a complete simulation each time,
this could repeat the whole Windows run. If it has an idempotency/completion
guard, queue-only behavior, or another protective contract, it might avoid
duplicate computation. The absent launcher prevents deciding which behavior
occurs. This is a concrete launch-code risk, not an observed duplicate run.

There is also a retry-control issue: line 7 enables shell `set -e`. The first
SSH call on line 101 is not protected by the `until` condition. A nonzero exit
there can terminate the job before execution reaches the retry loop. Therefore
the retry loop does not protect the first call in the straightforward shell
execution path.

Inspect the Windows launcher before changing this sequence. In particular,
establish whether it runs synchronously, checks existing completion, and returns
the actual Dinamica exit status. No launcher correction or backup modification
was made during this audit.

## Evidence still needed

The next useful inputs are the actual `C:\scripts\run_dinamica.cmd` and any
scripts it calls, the engine version reported by that Windows installation,
and a complete prepared or completed web session. The existing Linux script
and identical model source are sufficient for this launch audit, but they are
not a complete reproduction of a server simulation.

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
became available from this inspection. The remaining case files and Windows
launcher are still needed before a full workflow test.
