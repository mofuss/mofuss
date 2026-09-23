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

## Run

```r
source("C:/Users/UNAM/Documents/mofuss/localhost/scripts/postprocessing_sourcing/2post_runtime_sourcing_v1.R")
rs_main(c(
  "--run-dir=E:/ECSA_1000m_bau1_2050_mc3_capped",
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
