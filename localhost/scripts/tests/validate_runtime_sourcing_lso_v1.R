#!/usr/bin/env Rscript
# Validate captured real Lesotho regression fixtures. Does not run Dinamica.
# All artifacts are confined to the designated temporary regression workspace.
source("localhost/scripts/postprocessing_sourcing/2post_runtime_sourcing_v1.R")
.rs_require()
args<-commandArgs(trailingOnly=TRUE)
if(!length(args))stop("Pass one or more completed LSO regression fixture directories",call.=FALSE)
root<-normalizePath("E:/MoFuSS_Active/ecsa_sourcing_and_speed_v1",winslash="/",mustWork=TRUE)
for(path in args) {
  run<-normalizePath(path,winslash="/",mustWork=TRUE)
  if(!startsWith(tolower(run),paste0(tolower(root),"/runtime_regression/lso_")))
    stop("This diagnostic accepts only LSO fixtures inside the task scratch",call.=FALSE)
  output<-file.path(root,"runtime_sourcing_validation",basename(run))
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  landscape<-terra::rast(file.path(run,"LULCC","TempRaster","LULCt1_c.tif"))
  zone_path<-file.path(output,"lso_source_zone.tif")
  terra::writeRaster(terra::ifel(!is.na(landscape),1,NA),zone_path,overwrite=TRUE,datatype="INT1U")
  crosswalk_path<-file.path(output,"lso_country_crosswalk.csv")
  data.table::fwrite(data.frame(source_id=1,source_iso3="LSO",source_name="Lesotho"),crosswalk_path)
  meta<-.rs_meta(run)
  result<-rs_main(c(paste0("--run-dir=",run),paste0("--zones=",zone_path),
      paste0("--crosswalk=",crosswalk_path),paste0("--output-dir=",output),
      sprintf("--periods=%d:%d",meta$start,meta$end),"--overwrite=YES"))
  if(any(result$qa$forbidden_direct_W_realised_tonnes>0) || any(result$qa$forbidden_direct_V_realised_tonnes>0))
    stop("Forbidden direct cross-border harvest in the singleton LSO fixture",call.=FALSE)
  cat(sprintf("LSO RUNTIME SOURCING VALIDATED: %s; %d annual MC rows\n",basename(run),nrow(result$qa)))
}
