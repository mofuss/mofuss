# MoFuSS script execution convention

This directory uses filenames to distinguish the two execution environments in
the active workflow.

## Active workflow scripts

- An R script in this directory whose filename starts with a number is sourced
  in the current RStudio session. These scripts may rely on objects created by
  an earlier numbered script, and a numbered main script may source them on the
  user's behalf.
- Other active R scripts are invoked from the command line, normally by
  Dinamica EGO, unless their own header explicitly states otherwise.
- Numbered scripts under `postprocessing_emissions` support regular RStudio
  Source, RStudio Source as Background Job, and direct `Rscript` execution from
  PowerShell or another terminal. Sourced execution uses each script's RStudio
  settings; direct `Rscript` execution uses its command-line arguments.
- Files under `older_versions` are archival and are not part of the active
  workflow.

## Windows simulation launchers

The localhost preprocessing workflows (`000_main_localhost_v1.R`, including its
batch runner, and `0_main.R`) finish with
`10_prepare_windows_launcher_v1.R`. On Windows this creates **`RUN_MoFuSS.cmd`**
inside each country/region working folder. It does not launch the simulation.
The earlier working-folder cloning step alone does not generate this launcher.

After preprocessing and the usual IDW output installation are complete,
double-click `RUN_MoFuSS.cmd` in Windows File Explorer. It uses the installed
Windows Dinamica 2.4.1 console at
`C:/Program Files/Dinamica EGO/DinamicaConsole.exe`, two processors, and a
separate temporary folder for each invocation under
`E:/MoFuSS_Active/windows_runs`. The command window shows the exit code and stays
open until a key is pressed. Missing engine/model files or unavailable temporary
storage produce a visible error. The command does not change the engine seed.

Step 2 configures the copied model's LUC selector immediately; the final R step
checks it again and configures the Monte Carlo wizard constant and BAU/ICS
report label. The repository's model and all scientific expressions remain intact:

| Prepared land-cover channels | Selected Windows model | LUC |
| --- | --- | ---: |
| Woodman enabled (`LULCt3map=YES`, including when MODIS is also enabled) | v14 | 3 |
| MODIS enabled, Woodman disabled | v14 | 1 |
| Only Copernicus enabled | v13 | 2 |

New MODIS and Woodman runs share the v14 woodfuel NRB attribution contract.
Their NRB/fNRB excludes prescribed land-cover stock resets and downward
preharvest capacity clamps, while retaining growth offsets. Both use the same
signed annual balance exported in `debugging_<MC>/Woodfuel_balanceNN.tif`.
Historical v13 files remain available for replay. Copernicus-only LUC2 and the
Linux launcher still use v13 legacy NRB accounting; corrected v14 attribution
has not been validated for those workflows.

`scenario_ver` starting with `BaU` sets **MC rerun = Yes**; `ICS` or `CCTS` sets
**No**. Other scenario prefixes are rejected rather than assigned an assumed
MC policy. Each ICS/CCTS must wait for its matching BAU's **new complete** MC
batch. Existing `bypassMC_v8.R` checks still select and validate the matching
BAU; the launcher does not guess a partner from the country name.

On the current four-core workstation, use at most four simultaneous simulations.
Launch each folder once and allow the preparation step to finish. Do not run
the wizard and CMD simultaneously for the same folder. Open the `.egoml` in
the wizard when interactive settings are needed; `.cmd` files run from Explorer.

For another Windows computer, change the settings at the top of
`10_prepare_windows_launcher_v1.R`. The paths also accept the environment
overrides `MOFUSS_DINAMICA_CONSOLE` and `MOFUSS_WINDOWS_TEMP_ROOT` at preparation
time. Repository edits alone do not update existing launchers or running
simulations. Do not rerun preprocessing over a completed/running run just
to generate a launcher, because earlier preprocessing steps rebuild inputs.

The eight existing MDG folders on F: and D: use their existing
`RUN_MDG_optimized.cmd` for the October 8, 2026 update. All use corrected v14:
F: uses fixed MODIS LUC1; D: uses annual Woodman LUC3. In both capped and
uncapped folders, BAU regenerates the three-draw MC batch and ICS reuses its
matching BAU batch. Each ICS `bau_mc_source.txt` pins the BAU on the same drive
with the same capped/uncapped setting. Finish the matching BAU before launching
ICS. No additional generator or preprocessing step is needed to double-click
these CMD files. Replaced runtime code is backed up under each run's
`_code_backups/mdg_v14_nrb_launchers_2026-10-08`.

## Script header contract

Active numbered scripts identify the script version and date, execution mode,
purpose, main inputs, outputs, and material side effects. They also expose a
`2dolist` section for pending work and an `Internal parameters` section for
settings intended to be reviewed or tuned.

The short SPDX identifier and accompanying notice refer to the repository's
Apache License 2.0. The repository license remains the authoritative license
text.

## Safe use

Read each script's side-effects line and internal settings before sourcing it.
In particular, cleanup and emissions postprocessing scripts can intentionally
delete a validated output directory before rebuilding it. Paths and scenario
settings should be changed only in their documented configuration blocks.

Parameter consolidation and library cleanup are intentionally outside the
documentation-only header pass and should be handled in a separate revision.
