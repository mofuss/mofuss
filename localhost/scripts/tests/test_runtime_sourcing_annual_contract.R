#!/usr/bin/env Rscript
# Reader routing and exact replay. All generated fixtures stay in R's TEMP.
# Optional real engine captures are read only; the model is never executed here.
source("localhost/scripts/postprocessing_sourcing/2post_runtime_sourcing_v1.R")
.rs_require()
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(terra))

args <- commandArgs(TRUE)
option <- function(key) {
  found <- args[startsWith(args,paste0(key,"="))]
  if(length(found))substring(found[1],nchar(key)+2L) else NULL
}
check <- function(ok,label) {
  if(!isTRUE(ok))stop(label,call.=FALSE)
  cat("PASS ",label,"\n",sep="")
}
expect_error <- function(expr,pattern) {
  error <- tryCatch({force(expr);NULL},error=conditionMessage)
  check(!is.null(error)&&grepl(pattern,error),paste("rejects",pattern))
}
same <- function(x,y) identical(is.na(x),is.na(y)) && identical(x[!is.na(x)],y[!is.na(y)])
v <- function(path) as.numeric(values(rast(path)))
contract <- "annual_domain_after_static_npa_cache_v1"

if(!"--replay-only" %in% args) {
  # Reuse the legacy fixture only after its existing codec, pressure, ledger,
  # signed-adjustment and public-CLI assertions have all passed.
  MOFUSS_KEEP_SOURCING_TEST_FIXTURE <- TRUE
  source("localhost/scripts/tests/test_runtime_sourcing_v1.R")
  legacy <- .rs_year(meta,1,2020,zones,cw,indices,block_mb=1)
  check(legacy$qa$sourcing_capture_contract=="legacy_static_domain_v12_v13",
        "legacy captures retain their explicit legacy route")
  static <- file.path(scratch,"Sourcing","static")
  cap <- file.path(scratch,"Sourcing","MC001")
  marker <- file.path(static,paste0(contract,".csv"))
  legacy_paths <- list.files(static,pattern="^[WV]_base.*\\.tif$",full.names=TRUE)
  corrected_paths <- file.path(static,sub("_base","_npa_base",basename(legacy_paths)))
  stopifnot(all(file.copy(legacy_paths,corrected_paths)))
  expect_error(.rs_year_inputs(meta,1,2020,indices),"Mixed runtime sourcing")
  # Rename only this test's tiny legacy captures to keep every input inspectable.
  stopifnot(all(file.rename(legacy_paths,paste0(legacy_paths,".legacy-test"))))
  expect_error(.rs_year_inputs(meta,1,2020,indices),"Missing corrected.*marker")
  fwrite(data.table(Key=1,Value=1),marker)
  expect_error(.rs_year_inputs(meta,1,2020,indices),"Incomplete corrected")
  accumulator <- file.path(cap,"accumulator_domain01.tif")
  stopifnot(file.copy(file.path(static,"accumulator_domain.tif"),accumulator))
  paths <- .rs_year_inputs(meta,1,2020,indices)
  check(paths$capture_contract==contract &&
        all(grepl("_npa_base",paths$paths[seq_len(nrow(paths$index))])) &&
        accumulator %in% paths$paths &&
        !file.path(static,"accumulator_domain.tif") %in% paths$paths,
        "corrected route selects all NPA bases and the current annual accumulator")
  corrected <- .rs_year(meta,1,2020,zones,cw,indices,block_mb=1)
  check(identical(legacy$matrix,corrected$matrix) && identical(legacy$demand,corrected$demand),
        "unchanged-domain corrected captures preserve the complete legacy ledger exactly")
  check(corrected$qa$sourcing_capture_contract==contract,"QA records the selected contract")

  wscalar_paths <- list.files(cap,pattern="^W_scalars.*\\.csv$",full.names=TRUE)
  wscalar_tables <- lapply(wscalar_paths,fread)
  for(k in seq_along(wscalar_paths))fwrite(wscalar_tables[[k]][1:18],wscalar_paths[k])
  expect_error(.rs_year_inputs(meta,1,2020,indices),"require v13 origin-preserving W scalars")
  for(k in seq_along(wscalar_paths))fwrite(wscalar_tables[[k]],wscalar_paths[k])

  moved <- corrected_paths[1]
  stopifnot(file.rename(moved,paste0(moved,".missing-test")))
  expect_error(.rs_year_inputs(meta,1,2020,indices),"Missing runtime/annual raster")
  stopifnot(file.rename(paste0(moved,".missing-test"),moved))
  expect_error(.rs_year_inputs(meta,2,2020,indices),"Missing runtime scalar")
  # Keep another year's accumulator present so absence of the requested year's
  # file cannot be concealed by run-level contract discovery.
  stopifnot(file.rename(accumulator,file.path(cap,"accumulator_domain02.tif")))
  expect_error(.rs_year_inputs(meta,1,2020,indices),"Missing runtime/annual raster")
  stopifnot(file.rename(file.path(cap,"accumulator_domain02.tif"),accumulator))
  fwrite(data.table(Key=1,Value=2),marker)
  expect_error(.rs_year_inputs(meta,1,2020,indices),"Invalid corrected.*marker")
  fwrite(data.table(Key=1,Value=1),marker)
  stopifnot(file.rename(marker,paste0(marker,".missing-test")))
  expect_error(.rs_year_inputs(meta,1,2020,indices),"Missing corrected.*marker")
  stopifnot(file.rename(paste0(marker,".missing-test"),marker))
  # Missing all new data must still be rejected by the surviving marker.
  empty <- tempfile("mofuss_annual_contract_empty_")
  dir.create(file.path(empty,"Sourcing","static"),recursive=TRUE)
  stopifnot(file.copy(marker,file.path(empty,"Sourcing","static",basename(marker))))
  expect_error(.rs_capture_contract(empty),"Incomplete corrected")
  cat("Synthetic capture fixtures: ",scratch,"\n",sep="")
}

probe <- option("--native-probe")
if(!is.null(probe)) {
  n <- 0L
  for(case in c("fixed","dynamic"))for(mc in 1:2)for(step in c(1,2,3,11,12))for(ch in c("W","V")) {
    run <- file.path(probe,case,sprintf("mc%d_step%02d",mc,step))
    snapshot <- if(step<11)1 else 11
    base <- v(file.path(probe,case,"candidate","Sourcing","static",
                        sprintf("%s_npa_base001_%02d.tif",ch,snapshot)))
    mask <- v(file.path(run,paste0("candidate_",ch,"_mask.tif")))
    eligible <- v(file.path(run,paste0("candidate_",ch,"_eligible.tif")))
    stopifnot(same(.rs_eligible(base,mask),eligible))
    scalars <- c(denominator=sum(eligible,na.rm=TRUE),demand=100,weeks=48,
                 slices=48,adjustment=0,source_step=snapshot)
    stopifnot(same(.rs_normalize(base,mask,scalars),
                  v(file.path(run,paste0("candidate_",ch,"_normalized.tif")))))
    n <- n+1L
  }
  check(n==40L,"40 native fixed/changing-domain eligibility and pressure vectors replay exactly")
}

run <- option("--replay-run")
if(!is.null(run)) {
  meta <- .rs_meta(run)
  indices <- setNames(lapply(c("W","V"),function(ch).rs_index(run,ch)),c("W","V"))
  count <- 0L; redistribute <- 0L; aggregates <- 0L
  for(mc in seq_len(meta$mc))for(year in seq.int(meta$start,meta$end)) {
    inputs <- .rs_year_inputs(meta,mc,year,indices)
    stopifnot(inputs$capture_contract==contract)
    index <- inputs$index; n <- nrow(index); step <- year-meta$start+1L
    captures <- file.path(run,"Sourcing",sprintf("MC%03d",mc))
    values <- as.matrix(terra::values(terra::rast(inputs$paths)))
    pressure <- vector("list",n)
    initial <- values[,n+3L+length(.rs_maps)]
    initial[is.na(initial)|initial==255] <- NA_real_
    for(k in seq_len(n)) {
      ch <- index$channel[k]; mask <- values[,n+if(ch=="W")1L else 2L]
      pressure[[k]] <- .rs_normalize(values[,k],mask,inputs$scalars[[k]])
      observer <- file.path(captures,sprintf("%s_component%03d_%02d.tif",ch,index$ComponentIndex[k],step))
      if(!same(pressure[[k]],v(observer)))stop("Exact component replay failed: ",observer)
      count <- count+1L
      if(ch=="W" && inputs$origin_preserving) {
        redistribution <- .rs_origin_tof(values[,k],mask,values[,ncol(values)],inputs$scalars[[k]])
        observer <- file.path(captures,sprintf("W_redistributed%03d_%02d.tif",index$ComponentIndex[k],step))
        if(!same(redistribution,v(observer)))stop("Exact TOF redistribution replay failed: ",observer)
        redistribute <- redistribute+1L
      }
    }
    for(ch in c("W","V")) {
      reconstructed <- .rs_accumulate(pressure[index$channel==ch],union_null=ch=="V",initial=initial)
      saved <- values[,n+2L+match(paste0("Proj_harv_",ch,"tot"),.rs_maps)]
      if(!same(reconstructed,saved))stop("Exact aggregate pressure replay failed: ",ch," MC",mc," year ",year)
      aggregates <- aggregates+1L
    }
  }
  check(TRUE,sprintf("%s: %d origin pressure, %d TOF redistribution and %d aggregate vectors replay exactly",
                     basename(run),count,redistribute,aggregates))
  replay_report <- option("--replay-report")
  if(!is.null(replay_report))fwrite(data.table(
    run=normalizePath(run,winslash="/",mustWork=TRUE),capture_contract=contract,
    mc_count=meta$mc,year_count=meta$end-meta$start+1L,grid_cells=nrow(values),
    origin_pressure_vectors=count,tof_redistribution_vectors=redistribute,
    aggregate_pressure_vectors=aggregates,all_vectors_exact=TRUE),replay_report)
}
cat("ALL ANNUAL SOURCING CAPTURE TESTS PASSED\n")
