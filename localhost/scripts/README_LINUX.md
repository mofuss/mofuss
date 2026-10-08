# MoFuSS v13 on Linux

The woodfuel-only NRB attribution correction is implemented and tested in the
Windows v14 graph, including fixed MODIS (LUC1) and annual Woodman (LUC3).
The Linux launcher still selects the historical v13 Linux graph and has not
been validated for the v14 balance contract. Its engine NRB/fNRB outputs retain
legacy stock-difference accounting; do not treat them as corrected LUC
attribution. The presence of a copied Windows v14 file does not change the
Linux launcher's selected graph.

The Linux model is `10_dyn_Sc17_webmofuss_ctrees_g_v13_linux.egoml`.
It uses the normal Windows folder/file layout and scenario parameter CSVs.
The original Windows model remains available alongside it. The shared V8 R
scripts support both operating systems.

## Human workflow

1. Create the four working folders with their scenario parameter tables.
2. Run the R preprocessing scripts sourced by `000_main_localhost_v1.R` for
   each folder. Step `2_copy_files_v4.R` copies the Windows/Linux model files,
   required R scripts, report code and Linux launcher into each folder.
3. Run directional IDW externally for each folder, or restore its backed-up IDW.
4. Run `localhost/scripts/9_install_directional_IDW_outputs_v4.R` from the
   repository for the four working folders. Use its user-input batch parameters
   to select the working-folder parent and folder names.
   Batch reruns verify and skip completed single-country component installs,
   so an already-installed BAU pair does not block later ICS installation.
5. Run the capped and uncapped BAU simulations first; then run their matching
   ICS/CCTS simulations.

On Linux, the final step in each working folder is:

```bash
cd "/drive/path/to/working_folder"
./run_linux.sh
```

`mofuss_r_linux.sh` is an internal R launcher called by the Linux model.
`run_linux.py` contains the simulation checks and compatibility preparation.
Neither is a separate human workflow step. There is no extra setup, deployment
or input-preparation command to run. Optional `./run_linux.sh --check` prepares
compatibility metadata and validates readiness without simulating.

### Sparse Telegram updates

`run_linux.sh` can use the existing webMoFuSS bot to send a short start message,
one update every 30 minutes, and completion, failure, or interruption. Messages
identify the working folder and computer. Periodic updates show elapsed time
and the latest saved MC/year harvest, when available; they do not claim that
the report is finished before the launcher succeeds.

Copying the updated shell file is sufficient; the notifier is embedded in it
and uses Python's standard library and the
[Telegram sendMessage API](https://core.telegram.org/bots/api#sendmessage).
It reads `MOFUSS_TELEGRAM_BOT_TOKEN` and `MOFUSS_TELEGRAM_CHAT_ID` from the local
repository `.env` (including `mofuss/localhost/scripts/.env`), or the process
environment. It searches repository locations beside the scenario and under
`~/Documents/mofuss`. Credentials stay in that file and are not copied into
working folders. The `.env` is read as data, never executed as shell code.

The marked shell user-input block contains three controls:

- `MOFUSS_TELEGRAM_MSGS=1`: notifications enabled; `0` disables them.
- `MOFUSS_TELEGRAM_INTERVAL_MINUTES=30`: periodic interval, from 1 to 1440 minutes.
- `MOFUSS_TELEGRAM_ENV_FILE=""`: automatic discovery; set an absolute `.env`
  path if the repository is stored elsewhere on that computer.

You can also supply them in the terminal, for example:

```bash
MOFUSS_TELEGRAM_ENV_FILE="/data/mofuss/repository/localhost/scripts/.env" ./run_linux.sh
```

To send a single test message without starting a simulation:

```bash
./run_linux.sh --telegram-test
```

`--check` and `--help` send no messages. Missing credentials or a Telegram
network failure allow the normal simulation to continue; credentials and HTTP
error details are never printed. Notifications apply to runs started with the
updated launcher, including when four scenarios are launched separately.

The simulation launcher derives BAU/ICS role from the existing parameter CSVs:

| Scenario | `BaU vs ICS scenario` | `Re-run MonteCarlo?` |
| --- | --- | --- |
| BAU | `BaU` | Yes: generate the BAU batch |
| ICS/CCTS | `ICS` | No: reuse the matching BAU batch |

It sets only these two model controls; other model controls and parameter CSVs
are retained. It repairs recognized legacy World Mercator CRS metadata only on
matching grids, with pixel/NoData checks and backups under
`Logs/linux_input_backups/`. It does not install IDW or recreate scenario folders.
Missing directional components cause it to stop and request script 9.

The ICS bypass verifies the matching BAU's current `Temp/mc_batch_ready.csv`
and exact MC table hashes. It reuses the CSV decimal strings without generating
new draws, and still runs all ICS dynamic realizations. Capped and uncapped
pairs remain separate. Keep matching BAU/ICS folders as siblings; an explicit
BAU location can be supplied in a one-line `bau_mc_source.txt` when needed.

Outputs use `Temp/`, `Out/`, `Summary_Report/`, `HTML_animation/`, `LaTeX/`,
`Logs/`, and `Sourcing/`. Unused `Debugging/` exports are disabled. Annual raster
series needed by reports and sourcing remain under `debugging_1/`,
`debugging_2/`, etc. These per-MC folders contain scientific inputs for
postprocessing and must be retained.
Normal full runs replace generated outputs as before.

### Sourcing export correction (2026-09-30)

The earlier Linux migration mistakenly removed eight annual exports when
cleaning up optional debugging outputs: `Expect_harv_tot`, `Proj_harv_Wtot`,
`Proj_harv_Vtot`, `Proj_harv_Wdef`, `Proj_harv_Vdef`, `Non_harv_AGR`,
`Ex_agr_harv`, and `harv_AGR`. Both sourcing analyses depend on these maps.
This affected all region sizes; it was unrelated to AGO being a single country.

The Linux model now restores the Windows export nodes, with the same source
maps, compression, MC folders, and annual numbering. Model calculations and
MC controls are unchanged. Windows already contained these exports and needs
no corresponding model change. The optional shared `Debugging/` folder remains
disabled on Linux.

Step `2_copy_files_v4.R` already copies the corrected model into new working
folders. For a previously prepared folder, copy only the updated
`10_dyn_Sc17_webmofuss_ctrees_g_v13_linux.egoml` from this repository before
your next intended run; do not rerun preprocessing merely to update that file.
Continue to launch with `./run_linux.sh`, which sets the BAU/ICS role from the
folder's parameter tables. Preserve any other model settings you customized.
Updating the model does not recover missing exports from completed runs;
those require matching archived files or a new simulation. Existing completed
working folders were not modified as part of this correction.

`tests/test_dinamica_sourcing_exports.py` checks the Windows/Linux export
contract against the sourcing reader and verifies MC/year placement. With
`MOFUSS_TEST_EGO` set to a native Dinamica console, it also exercises the exact
production export nodes on temporary four-cell inputs (two MCs, two years),
checking all nine raster series, values, NoData, and grid geometry.

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

## Installed software on each Linux computer

The workflow uses installed programs, with no `_migration` directory, runtime
bundle, Codex session, or `~/.config/mofuss/linux-runtime.env` dependency.

Validated Dinamica version: **8.13.0.20260827**. Set `MOFUSS_EGO` in the clearly
marked user-input block of `run_linux.sh` to that computer's installed console
or AppImage, or provide `DinamicaConsole` on PATH. Paths may be on any drive.
For a launch without editing the copied script:

```bash
MOFUSS_EGO="/path/to/installed/DinamicaEGO.AppImage" ./run_linux.sh
```

Native R and Python 3/GDAL must be installed. R needs `msm`, `raster`, `tidyverse`,
`readxl`, `readr`, `tibble`, `animation`, `data.table`, `foreach`, `jpeg`, `png`,
`sf`, `tiff`, and the installer dependencies (`terra`, `digest`). Reporting needs
FFmpeg, zip, PDFLaTeX, `kpsewhich`, and the Latin Modern font package on PATH.
The launcher checks dependencies before starting the dynamics. If an R library
needs a custom library directory, set `MOFUSS_R_LIBRARY_PATH`; otherwise the
wrapper clears EGO's private library paths before starting native R.

`Logs/linux_r_launcher.log` retains errors starting R. Script errors appear in
the corresponding `.Rout` files. Windows retains the existing Windows model and
workflow; the shared R code continues to support both operating systems.

## Canonical completed Linux run

`AGO_1000m_bau1_2050_mc3_uncapped` is the reference full run for the Linux
workflow: three sequential realizations, 2000–2050, with complete outputs and
reporting. Its scientific inputs, model, MC batch, and completed results were
retained when obsolete migration helpers were removed. Reference hashes for the
model, MC batch, report code, completion log, and PDF are recorded in
`tests/canonical_ago_bau1_uncapped.json`. A new launcher does not certify Windows
equality; scientific comparisons must still use the same verified MC batch.

Only `run_linux.py` is needed as Python launcher code in each working folder.
The shell launcher uses Python's `-B` option, so bytecode cache folders are not
created. Old `prepare_linux_inputs.py`, `mofuss_linux_env.sh`, deployment/setup
helpers, and `_migration` are not part of the current working-folder layout.
Programs installed for this workstation are separate from the model code and
working folders. Other computers use their own installed programs on PATH.
