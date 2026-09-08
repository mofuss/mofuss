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
