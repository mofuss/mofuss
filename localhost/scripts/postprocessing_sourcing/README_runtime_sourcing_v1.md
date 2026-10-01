# Country sourcing from recorded model runs

`2post_runtime_sourcing_v1.R` produces country demand and harvest balances after
the Dinamica model has finished. Start with `period_origin_balance.csv`: each
row identifies the run, Monte Carlo realization, period, demand country, and
channel (`W`, `V`, or combined `W+V`). It includes expected demand, realised
harvest, domestic and imported harvest, their percentages, clearing credits,
and unmet demand.

`period_sourcing_matrix.csv` shows which country supplied that attributed
harvest. `annual_*` files contain the same information for individual years.
`period_origin_mc_summary.csv` summarizes the available realizations with a
mean and empirical 2.5th/97.5th percentiles. Three MC runs provide a preliminary
description, not reliable uncertainty bounds.

`3post_runtime_sourcing_graphics_v1.R` turns the completed V-channel CSVs into
publication figures. For each selected period and regrowth setting it places
BAU1 and ICS3 on one page. Bars show domestic, imported, unmet and (if present)
clearing-credit shares of V demand by country; the adjacent matrix identifies
foreign supplier countries. Bar labels give imported tonnes and the share of
demand. Figures are saved as vector PDF and 300-dpi PNG, alongside the exact
aggregated plotting data.

## Run recorded sourcing by batch (Windows or Linux)

Open `0post_runtime_sourcing_pipeline_v1.R` in RStudio. Edit only the marked
`BEGIN USER INPUTS` / `END USER INPUTS` block, then Source the saved file.
This entry point runs the selected stages in separate R sessions for each batch.
Stage 3 (V sourcing graphics) is selected by default and reads the completed
Stage 2 tables. Set `SOURCING_STAGES <- 2L` to recompute the recorded tables,
or `SOURCING_STAGES <- 2:3` to run the tables and graphics together. Stage 2
does **not** require Stage 1 (the older approximation). Source the `0post_`
pipeline to run the selected stages; sourcing either helper file alone only
loads its functions.
It requires R with `terra` and `data.table` installed. It does not require Codex.

- Set each entry in `SOURCING_BATCHES` to `enabled = TRUE` or `FALSE`.
  ECSA is currently enabled; the other regional examples are disabled.
  The current Windows configuration matches the emissions analysis: the four
  completed `D:/ECSA_1000m_{bau1,ics3}_2050_mc3_{capped,uncapped}` folders,
  with outputs in
  `D:/mofuss_postprocessing/ECSA_1000m_2050_mc3/runtime_sourcing/`.
  The similarly named prepared folders on `E:/` are not the completed runs.
- `SOURCING_WORKING_ROOT = "AUTO"` finds the parent of the repository from
  the script location, independently of the current R working directory.
  This works when `mofuss/` and the scenario folders are siblings.
  Otherwise set it to, for example, `"E:/"` on Windows or `"/mnt/data"` on Linux.
  In R strings, use forward slashes on both systems.
- Each batch's `root = ""` inherits that root. Set an explicit root on batches
  whose four scenario folders live on another drive. On Linux use the mounted
  directory path; a Windows drive letter is not a Linux path.
- `analysis_folder` is a neutral folder name such as `AGO_1000m_2050_mc3`.
  With `SOURCING_ANALYSIS_PARENT = "AUTO"`, results go under
  `<batch root>/_mofuss_postprocessing/<analysis_folder>/`, in separate
  `model_implied_sourcing/` and `runtime_sourcing/` subfolders.
  Set an absolute analysis parent to store all batches elsewhere.
- `SOURCING_STAGES = 3L` draws from the completed Stage 2 CSVs without reading
  working-folder rasters. Use `1L`, `2L`, or `1:3` to select other workflows.
  `SOURCING_MC_RUNS = "all"` includes the available realizations. The graphics
  use the ratio of mean attributed tonnes to mean demand across the selected MC
  runs, rather than an unweighted average of percentages.
- `SOURCING_TEMP_DIR` holds disposable raster scratch, not final tables.
  On this Windows machine it is `E:/MoFuSS_Active/runtime_sourcing`;
  change it to a writable local path when moving the configuration to Linux.
- `SOURCING_CHECK_ONLY = TRUE` checks required inputs for all enabled batches
  without writing analysis results. Stage 2 can validate inputs even when its
  results already exist. Before normal processing the pipeline also rejects
  existing result files unless overwrite is enabled. Calculation-time
  reconciliation checks still run during the analyses.
- `SOURCING_OVERWRITE = FALSE` protects existing analysis results. Set it to
  `TRUE` to replace the selected analyses' output files. Working folders are
  read only.

When Stage 1 or 2 is selected, the pipeline discovers each run's country-zone
raster (`admin_c.tif`) and country crosswalk, preferring frozen metadata when
available. All four runs must agree. Optional batch fields `zones` and
`crosswalk` accept explicit paths to override discovery. Stage 3 uses only the
three completed Stage 2 CSVs and saves to
`<analysis_folder>/runtime_sourcing/runtime_sourcing_graphics/`. Keep all four scripts together
in the repository.

From a terminal with `Rscript` available, use the full script path, quoted
if it contains spaces. For example on this Linux computer:

```sh
Rscript /home/mofuss/Documents/mofuss/localhost/scripts/postprocessing_sourcing/0post_runtime_sourcing_pipeline_v1.R --check
```

Remove `--check` to process. On Windows, Source in RStudio works without
adding `Rscript.exe` to PATH; the pipeline locates it in the current R installation.

## Required completed-run exports

The compact `Sourcing/` records alone are insufficient. Both analyses also
need annual diagnostic rasters from each selected Monte Carlo realization in
`debugging_<MC>/`, numbered with the model time step (year minus start year
plus one):

- Stage 1: `Harvest_tot`, `Expect_harv_tot`, `harv_AGR`, `Proj_harv_Vdef`.
  It also needs installed W/V component IDWs, annual demand tables, parameters,
  and the `LULCC/TempRaster/npa_c.tif` raster.
- Stage 2: `Proj_harv_Wtot`, `Proj_harv_Vtot`, `Proj_harv_Wdef`,
  `Proj_harv_Vdef`, `Non_harv_AGR`, `Ex_agr_harv`, `harv_AGR`,
  `Expect_harv_tot`, `Harvest_tot`, together with the recorded scalar, mask,
  forest-state and static captures in `Sourcing/` and the model input indices.

If those annual exports were disabled during Dinamica, check mode stops and
identifies the missing files. Copying the postprocessing scripts cannot recover
them: use a matching archived export, or a future model run configured to save
the required diagnostics. Never fill missing pressure or clearing maps with zeros.

The Linux migration's removal of eight required exports was corrected in the
repository model on 2026-09-30. New preprocessing copies include the fix;
previously prepared Linux folders need the corrected `.egoml` before a future
simulation. Windows v13 already includes these exports. See
[the correction and update instructions](../README_LINUX.md#sourcing-export-correction-2026-09-30).

## Run Stage 2 directly

```r
source("/path/to/mofuss/localhost/scripts/postprocessing_sourcing/2post_runtime_sourcing_v1.R")
rs_main(c(
  "--run-dir=/path/to/completed_scenario",
  "--zones=PATH_TO_VALIDATED_COUNTRY_ZONE_RASTER",
  "--crosswalk=PATH_TO_FROZEN_COUNTRY_CROSSWALK.csv",
  "--output-dir=PATH_TO_THIS_RUNS_POSTPROCESSING_DIRECTORY"
))
```

The same arguments work with `Rscript`. Repeat `--run-dir` to process matching
grids together. Zones must be an aligned one-layer raster with country IDs;
the crosswalk must contain `source_id,source_iso3,source_name`, or the equivalent
`ID,GID_0,NAME_0`/`CountryID,GID_0,NAME_0` fields. Input indices must identify one
demand country per component. Frozen run indices and parameters are verified
against current files when the installation snapshot is present.

Default periods are **2020–2030, 2030–2040, 2040–2050, and 2020–2050**. Both
endpoints are included, so adjacent decades overlap at their boundary year.
Use the separate 2020–2050 row for the whole period. Percentages divide summed
volumes; they are not averages of annual percentages. Combined W+V rows also
sum volumes and must not be added to the W/V detail a second time.

## What the figures mean

These are **model-implied sourcing accounts, not observed trade**. The script
reconstructs each origin's normalized float32 pressure using the masks, base
maps, and exact scalar values recorded during the run. It checks the result
against the model's saved W/V pressure before accepting an attribution.
Clearing reductions and the common final biomass limit are then attributed
proportionally to those origin pressures.

The v12 capture preserves the legacy regional pooling of trees-outside-forest
(TOF) shortfalls. That contribution is explicitly labeled
`pooled_TOF_redistribution`. With v13 captures the script reconstructs each
origin's own redistribution and labels it
`origin_preserving_TOF_redistribution`. The QA table distinguishes direct W
crossing, pooled W crossing, origin-preserving W crossing, and forbidden V
sources.

For v13, the deficit check uses the captured per-origin shortfall scalars,
not an inferred deficit from the legacy `Non_harv_AGR` raster. That raster can
be zero even when the captured v13 origin redistribution is positive. The
2026-09-30 reader correction selects the appropriate deficit before checking
it; it does not alter the simulation or the attribution of recorded harvest.

`clearing_credit_tonnes` identifies the country of the pixel where a model
credit was **applied**, not the production location of cleared wood. Domestic,
import, and source percentages use realised `Harvest_tot` and exclude those
credits. Unmet demand is demand minus clearing credits minus realised harvest.

Signed negative adjustments are never silently set to zero. The command-line
default stops if they occur. `--signed-policy=report` is an explicit diagnostic
option for legacy behavior: it retains signed accounting and withholds trade
percentages for affected origins. `runtime_sourcing_qa.csv` records any small
float32 reconciliation residual and missing provenance snapshots.

This script reads completed captures; it does not run IDW, rerun Dinamica, or
establish equivalence between model versions. Model equivalence is checked
separately against the previous version's scientific outputs.
