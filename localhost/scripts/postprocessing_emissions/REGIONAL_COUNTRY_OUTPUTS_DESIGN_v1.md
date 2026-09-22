# Regional country outputs: implementation contract

The regional emissions pipeline preserves the existing regional totals and adds
country-level spatial-incidence outputs. Country-level quantities use these
definitions:

- Harvest/AGB effects are assigned to the country containing the harvest cell.
- End-use effects are assigned to the country containing the demand/cooking cell.
- Total effects are the sum of those two country-specific components.
- These results describe spatial incidence, not attribution of a harvest effect
  back to the country whose demand may have caused it under transboundary trade.

For Country and Regional AoIs, the model-grid
`LULCC/TempRaster/admin_c.tif` is authoritative for harvest/AGB zoning. The
filtered ADM0 features in
`LULCC/DownloadedDatasets/SourceData*/demand/demand_in/mofuss_regions0.gpkg`
provide ISO3 codes, country names, boundaries, and a zone-ID crosswalk. End-use
zoning uses cell-centre rasterization, with polygon-touch rasterization only for
otherwise unassigned valid cells in the one-cell reprojection fringe.

Stage 2 freezes that validated partition beside its country tables as
`country_harvest_zones.tif`, `country_enduse_zones.tif`, and
`country_boundaries.gpkg`. Stage 3 consumes those exact files instead of
reconstructing the partition from scenario inputs. This makes the spatial
accounting handoff explicit, auditable, and stable across later stages.

Every additive country result must reconcile to its regional result within a
documented numerical tolerance. No valid emissions cell may remain unassigned.
Country, Regional, and own-polygon AoIs are supported. Country AoIs produce a
one-country partition. An own-polygon AoI remains one explicitly labelled
analysis-area unit; the postprocessor does not invent country attribution when
the model input itself used a single custom-polygon unit. Existing regional
files remain available with their established meaning and layout.

## Stage 5: refreshing the consolidated global package

Keep `PIPELINE_STAGES <- 1:5` for normal batches. Stages 2-4 write each
region/singleton below `PIPELINE_GLOBAL_ANALYSIS_PARENT`; Stage 5 then discovers
all immediate child folders with one Stage 3 country-per-run CSV and rebuilds
`PIPELINE_GLOBAL_OUTPUT_DIR` from those country draws and Stage 4 map rasters.
Previously completed batches do not need to be enabled again. Global tables
and figures are refreshed together; uncertainty intervals are recomputed from
the draws, never appended or added from previously published summaries.

To recover after a Stage 5 failure without repeating Stages 1-4, temporarily
set `PIPELINE_STAGES <- 5L`, source the pipeline, then restore `1:5` for the next
batch. Keep `PIPELINE_CLEAN_REBUILD <- TRUE` to refresh the existing package.
`Rscript 0post_emissions_pipeline_v2.R --check` prints the plan without writes,
including the regionalization file checked before any batch starts.

`PIPELINE_GLOBAL_REGIONALIZATION_FILE` selects the current M85/B30 catalog.
Stage 5 also defaults to `admin_regions/regionalization_M85_B30_V1.csv` when
called directly; `--regionalization-file=<csv>` explicitly selects another
catalog. Every canonical region, including singleton IDs such as `M85_S_GAB`,
receives a non-missing grouping index. Coverage validation records the catalog.

In `partial` mode, older analysis partitions may contribute their original
country results while validation is underway. Countries are grouped using the
selected catalog, with shared Monte Carlo draws preserved across all countries
from each original analysis. A root spanning multiple current regions is
marked `legacy_partition`; an incomplete single region is marked
`development_component`. This changes reporting groups, not the simulation
that produced the results. Strict mode requires complete canonical partitions.

Discovery uses the folders present at each refresh, so removed analyses stop
contributing. Overlapping countries, duplicate analysis IDs, or repeated
country/configuration/run rows stop the refresh rather than double count.
When replacing legacy runs with new regional runs, keep only the intended,
non-overlapping analyses in the discovery parent. Stage 5 never deletes these
input folders itself.
