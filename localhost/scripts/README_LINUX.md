# MoFuSS v13 on Linux

The Linux model is `10_dyn_Sc17_webmofuss_ctrees_g_v13_linux.egoml`.
It uses the normal Windows folder/file layout and scenario parameter CSVs.
The original Windows model remains available alongside it. The shared V8 R
scripts support both operating systems.

## Copy the code to a working folder

Step `2_copy_files_v4.R` now copies both models, all six R scripts, Linux
launchers, input-preparation helpers, and the report dependency. It checks for
missing repository files **before** starting its existing preprocessing reset.

For an existing working folder, copy the runtime bundle without running the
full preprocessing reset. From R, with this repository as the working directory:

```r
source("localhost/scripts/deploy_runtime_bundle_v1.R")
mofuss_copy_runtime_bundle(
  "localhost/scripts",
  "/absolute/path/to/your/working/folder"
)
```

This copies only the code listed by `mofuss_runtime_bundle_files()`. It preserves
scenario parameters, input data, MC draws, and existing results, and sets Linux
shell permissions. The same listed files can be copied manually.

## Prepare and run

In each working folder after copying:

```bash
./run_linux.sh --prepare-inputs
./run_linux.sh --check
./run_linux.sh
```

Preparation does not simulate. It installs missing single-country sourcing
components through the existing project installer and repairs the known legacy
World Mercator CRS metadata only when the raster grid matches the reference.
Pixel values are checked before/after; changed files are backed up under
`Logs/linux_input_backups/`. Existing correctly installed components are retained.
For a regional scenario, finish the normal directional-IDW preprocessing first.

Preparation also sets the two model role controls from `scenario_ver`:

| Scenario | `BaU vs ICS scenario` | `Re-run MonteCarlo?` |
| --- | --- | --- |
| BAU | `BaU` | Yes: generate the BAU batch |
| ICS/CCTS | `ICS` | No: reuse the matching BAU batch |

These are the same selections described in the Windows model. All other model
controls and parameter CSVs are retained. The launcher checks the role before
initializing outputs. Run each BAU before its matching ICS; capped and uncapped
pairs remain separate. The bypass verifies the BAU batch and creates Linux
lookup tables from its exact decimal strings, with no new MC draws.

For the AGO folders, ICS capped reads the BAU capped sibling, and ICS uncapped
reads the BAU uncapped sibling. The match uses the parameter CSVs, including
`uncapped_regrowth`, years, geography, resolution, and MC count. ICS still runs
all three dynamic realizations; bypassing MC skips only new parameter draws.
The source batch must have its `Temp/mc_batch_ready.csv` manifest and unchanged
MC tables. The bypass stops if the matching source is missing or ambiguous.

Copy the full bundle before preparation; copying only the Linux `.egoml` is
insufficient. The template has BAU role controls until `--prepare-inputs` sets
them for its destination. Normal execution rejects inconsistent role controls.

Outputs use `Temp/`, `Out/`, `Summary_Report/`, `HTML_animation/`, `LaTeX/`,
`Logs/`, and `Sourcing/`. Unused `Debugging/` exports are disabled. The three
annual raster series used by reports remain in `debugging_1/`, `debugging_2/`,
etc. Normal full runs replace generated outputs as before.

## Workers and reproducibility

The default `--processors auto` selects physical cores within the process's
CPU affinity and any detected scheduler/cgroup allocation. On a 4-core/8-thread
machine this selects **4 workers**. An explicit override is available:

```bash
./run_linux.sh --processors 2
```

The MC realizations remain sequential within the model. Their raster
calculations already use EGO workers. CPU count alone does not establish the
fastest worker setting; benchmark results are documented in
`tests/linux_cpu_benchmark.md` in the repository.

Defaults retain original report resolution (up to 1000 dpi), use R seed
`20260929`, and request EGO's predefined seed. Override with `MOFUSS_SEED=12345`
or `MOFUSS_PLOT_DPI=300` before the command. For Windows/Linux comparisons, reuse
the **same MC input CSV batch**; a matching seed across different R/package
versions is insufficient. Run logs record the worker selection, parameter and
MC input hashes, timing, and completion status. They do not certify cross-platform
equality automatically.

## Linux paths in the R pipelines

The emissions pipeline, calibration pipeline, and directional-IDW installer
share one neutral analysis name per region:

| Region | `analysis_folder` |
| --- | --- |
| AGO | `AGO_1000m_2050_mc3` |
| ECSA | `ECSA_1000m_2050_mc30` |
| GOG | `GOG_1000m_2050_mc30` |
| MDG | `MDG_1000m_2050_mc3` |
| LSO | `LSO_1000m_2050_mc3` |
| MLI | `MLI_1000m_2050_mc3` |
| GAB | `GAB_1000m_2050_mc3` |
| GLEA | `GLEA_1000m_2050_mc3` |

Each value is a shared label for the four-run regional analysis. It leaves out
`bau1` and `ics3` because each batch contains both scenarios. For AGO, for
example, the Linux root setting is `root = "/home/mofuss/Documents"`.

`root` is the absolute parent containing the four working folders. `folders`
lists their existing names, with spelling and capitalization preserved.
`analysis_folder` is a single output folder name, not an absolute path.

`PIPELINE_GLOBAL_ANALYSIS_PARENT` in the emissions pipeline and
`PIPELINE_POSTPROCESSING_ROOT` in the calibration pipeline both point to
`/home/mofuss/Documents/mofuss_postprocessing`. The AGO analysis will therefore
be written to:

```text
/home/mofuss/Documents/mofuss_postprocessing/AGO_1000m_2050_mc3
```

The emissions entry point passes the selected paths to Stages 1–5; their
individual scripts need no duplicate folder edits. From the repository root,
check the configuration without calculating emissions:

```bash
Rscript localhost/scripts/postprocessing_emissions/0post_emissions_pipeline_v2.R --check
```

Stage 5 scratch is below `~/MoFuSS_Active` on Linux (`E:/MoFuSS_Active` on
Windows). Calibration additionally needs its external AGB observation directory
configured locally, its `PIPELINE_TEMP_ROOT` directory created, and the emissions
analysis completed first. Updating its batch paths alone does not supply that
observation dataset.

## Machine-local runtime

Validated with EGO **8.13.0.20260827**, native R, FFmpeg, PDFLaTeX (including
Latin Modern fonts), zip, and Python 3/GDAL. Runtime binaries are not committed.

Configure an existing isolated runtime bundle using
`~/.config/mofuss/linux-runtime.env`:

```bash
export MOFUSS_RUNTIME_DIR="/absolute/path/to/the/runtime/bundle"
```

The bundle contains `tools/env.sh`, its libraries/report tools, and
`downloads/DinamicaEGO-8130-Ubuntu-LTS.AppImage`. Alternatively, set `MOFUSS_EGO`
to a native EGO console/AppImage and provide R/report tools on `PATH`.
`MOFUSS_R` and `MOFUSS_LIBRARY_PATH` override native R and its library path.
`MOFUSS_LINUX_CONFIG` selects a different configuration file.

This workstation already has the verified runtime configured outside the
repository. Keep that shared bundle in place when copying code to other folders.

## Repeatable setup on Linux

From a checkout of this repository, prepare any existing working folder:

```bash
bash "/path/to/mofuss/localhost/scripts/prepare_working_folder_linux.sh" \
  "/drive/path/to/working_folder" \
  "/drive/path/to/runtime_bundle"
```

The runtime bundle is the directory containing `tools/env.sh` and the verified
Dinamica AppImage under `downloads/`. Copy the complete bundle when moving it
to another drive or compatible Linux computer. Install R and the required R
packages on that computer; the validation reports missing dependencies. The
optional second argument overrides this machine's configured runtime location.
For a native installation, omit it and configure `MOFUSS_EGO` as described above.

This command copies the complete current code bundle, configures the model role
from the folder's parameter files, prepares missing Linux inputs, and runs checks.
It replaces model code with the repository template, so preserve any personal
model-code edits first. It does not start a simulation. Repeat it for each working
folder. After success, use the printed `bash .../run_linux.sh` command. Paths with
spaces must be quoted. ICS requires the matching BAU Monte Carlo batch; keep the
paired folders as siblings, or use the explicit BAU source described above.
