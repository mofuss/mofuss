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
