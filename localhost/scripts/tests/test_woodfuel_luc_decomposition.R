# SPDX-License-Identifier: Apache-2.0
# Hand-calculated process fixtures, NoData collision recovery and original-grid
# support checks. Scratch is supplied outside the source/simulation folders.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply a temporary fixture directory.")
source("localhost/scripts/helpers/woodfuel_luc_decomposition.R")
suppressPackageStartupMessages(library(terra))
work <- normalizePath(args[1], winslash = "/", mustWork = FALSE)
dir.create(work, recursive = TRUE, showWarnings = FALSE)
stopifnot(!startsWith(tolower(paste0(work, "/")),
                     tolower(paste0(normalizePath(getwd(), winslash = "/"), "/"))))
terraOptions(tempdir = work)
near <- function(actual, expected, tol = 1e-8) {
  if (any(!is.finite(actual)) || any(abs(actual - expected) > tol))
    stop("Expected ", paste(expected, collapse = ","), "; got ", paste(actual, collapse = ","))
}
fails <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
}
lc <- rbind(c(0,0,0,2,0,0,0,0,0), c(1,2,0,1,0,NA,NA,0,0), c(1,2,0,1,0,0,0,0,0))
tf <- rbind(rep(0,9), c(1,0,0,1,0,NA,NA,0,0), c(1,0,0,1,0,0,0,0,0))
tr <- rbind(rep(0,9), c(1,0,0,3,0,NA,NA,0,0), rep(0,9))
pb <- rbind(c(80,100,60,60,9999,60,60,0,80), c(0,60,80,20,19999,NA,NA,2,80),
            c(20,60,100,20,20009,0,0,2,80))
pi <- rbind(c(100,120,80,80,10000,80,80,2,100), c(0,60,90,20,20000,NA,NA,2,100),
            c(20,60,100,20,20010,0,0,2,100))
cb <- rbind(c(20,20,40,40,1,40,40,100,20), c(20,20,20,40,NA,NA,40,98,20),
            c(20,20,0,40,NA,NA,40,98,20))
ci <- rbind(c(0,0,20,20,0,20,20,98,0), c(0,0,10,20,-10000,NA,20,98,0),
            c(0,0,0,20,-10010,NA,20,98,0))
h <- matrix(0,3,9); h[2,6] <- 2
raw <- c(100,120,100,100,10000,100,100,0,NA)
ini <- c(100,120,100,100,10000,100,100,100,100)
template <- rast(nrows = 1, ncols = 9, xmin = 0, xmax = 9, ymin = 0, ymax = 1, crs = "EPSG:4326")
put <- function(root, rel, val, datatype = "FLT4S", nodata = -1e30) {
  path <- file.path(root, rel); dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  rr <- setValues(template, val)
  writeRaster(rr, path, overwrite = TRUE, datatype = datatype, NAflag = nodata)
}
make <- function(root, p, c) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  model <- "10_dyn_fixture_v14.egoml"
  writeLines(paste0('<script><property key="mofuss.nrb.attribution.contract" value="woodfuel_attributed_signed_balance_v1"/>',
    '<functor name="Bool"><inputport name="constant">.no</inputport><outputport name="object" id="v253"/></functor>',
    '<functor name="Int"><inputport name="constant">3</inputport><outputport name="object" id="v302"/></functor></script>'), file.path(root, model))
  jsonlite::write_json(list(model = model, luc = 3), file.path(root, "windows_performance_preparation.json"), auto_unbox = TRUE)
  dir.create(file.path(root, "LULCC/TempTables"), recursive = TRUE, showWarnings = FALSE)
  write.csv(data.frame(Var = c("start_year", "end_year", "monte_carlo_runs", "uncapped_regrowth"),
    ParCHR = c(2000,2002,1,0)), file.path(root, "LULCC/TempTables/parameters_dinamica.csv"), row.names = FALSE)
  put(root, "Temp/2_IniSt01.tif", ini)
  put(root, "Temp/2_TOFvsFOR01.tif", tf[1,])
  put(root, "LULCC/TempRaster/agb3_c.tif", raw)
  put(root, "LULCC/TempRaster/LULCt3_c.tif", lc[1,])
  write.csv(data.frame(Key = 0:2, Value = c(30000,20,60)), file.path(root, "Temp/mc_k_01.csv"), row.names = FALSE)
  for (j in 1:3) {
    put(root, sprintf("LULCC/TempRaster/LULCt3_c_%d.tif", 1999+j), lc[j,])
    put(root, sprintf("LULCC/TempRaster/TOFvsFOR_mask3_%d.tif", 1999+j), tf[j,])
    put(root, sprintf("LULCC/TempRaster/LULCt3_transition_%d.tif", 1999+j), tr[j,])
    put(root, sprintf("debugging_1/Growth_less_harv%02d.tif", j), p[j,])
    put(root, sprintf("debugging_1/Growth%02d.tif", j), if (j == 1L) ini else p[j,])
    put(root, sprintf("debugging_1/Harvest_tot%02d.tif", j), if (j == 1L) ini - p[j,] else h[j,])
    # The real old ledger used -9999. Missing values here reproduce its exact
    # collision and subsequently propagated NoData while physical stocks exist.
    put(root, sprintf("debugging_1/Woodfuel_balance%02d.tif", j), c[j,], nodata = -9999)
  }
}
bau <- file.path(work, "BAU"); ics <- file.path(work, "ICS")
make(bau, pb, cb); make(ics, pi, ci)
status <- mofuss_luc_pair_status(bau, ics, 1, 2001, 2002)
stopifnot(status$available, length(status$auxiliary_paths) >= 6)
result <- mofuss_luc_decompose_pair(bau, ics, 1, 2001, 2002,
                                    output_dir = file.path(work, "result"), temp_dir = work)
s <- result$summary
near(s$net_stock_benefit_mg, -122)
near(s$raw_signed_woodfuel_effect_mg, -22)
near(s$direct_luc_net_effect_mg, -20)
near(s$direct_luc_loss_mg, 20); near(s$direct_luc_gain_mg, 0)
near(s$tof_allowance_net_effect_mg, -20)
near(s$capacity_clamp_net_effect_mg, -20)
near(s$capacity_class_changed_this_year_mg, -20)
near(s$other_net_effect_mg, -40)
near(s$luc_reversal_of_positive_gap_mg, 20)
near(s$luc_transition_exposed_saved_stock_mg, 20)
near(s$forest_to_tof_reversal_mg, 20)
near(s$luc_reversal_fraction, 1)
near(s$endpoint_support_pixels, 8); near(s$process_support_pixels, 6)
near(s$complete_ledger_support_pixels, 7)
near(s$nrb_support_pixels, 7)
stopifnot(identical(result$metadata$biomass_support,
                   "finite_original_agb3_c_and_finite_paired_postharvest_endpoints"))
near(s$unattributed_stock_benefit_mg, -20)
near(s$support_gap_stock_benefit_mg, -40)
near(s$recovered_exported_ledger_gap_pixels, 1)
near(s$aggregate_reconciliation_error_mg, 0)
wv <- values(rast(result$paths$rasters$raw_signed_woodfuel_effect))[,1]
stopifnot(is.na(wv[6]), is.na(wv[9])); near(wv[5], 0); near(wv[7], 0)
near(values(rast(result$paths$rasters$net_stock_benefit))[8,1], -2)
# Independent NRB API uses model support (including reference-NA cell9), carries
# a zero-harvest gap, recovers true sentinel collision and rejects positive-H gap.
rec <- mofuss_reconstruct_signed_period(bau, 1, 2, 3, "previous_postharvest", work)
near(rec$diagnostics$summary$nrb_support_pixels, 8)
stopifnot(identical(rec$diagnostics$metadata$biomass_support,
                   "finite_initial_model_stock_support"))
rv <- values(rec$signed)[,1]
near(rv[c(1,2,3,4,5,7,8,9)], c(0,0,-40,0,-10010,0,-2,0))
stopifnot(is.na(rv[6]))
pre <- mofuss_reconstruct_signed_period(bau, 1, 2, 3, "preharvest", work)
near(values(pre$signed)[c(3,5,8),1], c(-20,-10,0))
full <- mofuss_reconstruct_signed_period(bau, 1, 1, 3, "previous_postharvest", work)
near(values(full$signed)[c(1,2,3,4,5,7,8,9),1], c(20,20,0,40,-10009,40,98,20))
# An empty exposure denominator is undefined, not zero; no clipped-annual sum.
plan <- status
mat <- values(rast(plan$paths), mat = TRUE)
sub <- .mofuss_luc_block(mat[c(3,8),,drop=FALSE], plan)
near(sum(sub$values[,"clipped_nrb_saving"],na.rm=TRUE), 0)
uncapped_plan <- plan
uncapped_plan$bau$uncapped <- uncapped_plan$ics$uncapped <- 1L
uc <- .mofuss_luc_block(mat[-2,,drop=FALSE], uncapped_plan)
near(sum(uc$values[,"capacity_clamp_net_effect"],na.rm=TRUE),0)
fails(.mofuss_luc_block(mat,uncapped_plan),"Uncapped preharvest decline")
# A damaged finite ledger must fail verification; missing files fail preflight.
broken <- cb; broken[2,1] <- 123
put(bau, "debugging_1/Woodfuel_balance02.tif", broken[2,], nodata = -9999)
fails(mofuss_luc_decompose_pair(bau,ics,1,2001,2002,NULL), "disagrees")
put(bau, "debugging_1/Woodfuel_balance02.tif", cb[2,], nodata = -9999)
broken <- cb; broken[2,7] <- 50
put(bau, "debugging_1/Woodfuel_balance02.tif", broken[2,], nodata = -9999)
fails(mofuss_luc_decompose_pair(bau,ics,1,2001,2002,NULL), "disagrees")
put(bau, "debugging_1/Woodfuel_balance02.tif", cb[2,], nodata = -9999)
f <- file.path(bau,"debugging_1/Harvest_tot03.tif")
file.rename(f,paste0(f,".held"))
fails(mofuss_luc_pair_status(bau,ics,1,2001,2002), "Missing corrected")
file.rename(paste0(f,".held"),f)
# Invalid start with zero harvest carries the observer balance and must not
# credit its physical 2-Mg seed. Next year's valid recovery changes C by -1,
# not -3 (native missing_start_does_not_credit_seed regression).
previous <- list(step=1L,p=80,b=100,h=20,lc=0,tof=0,tr=0,k=100,state_valid=TRUE)
gap <- list(step=2L,p=0,b=0,h=0,lc=0,tof=0,tr=0,k=NA_real_)
g <- .mofuss_luc_increment(previous,gap,TRUE)
near(g$delta,0); stopifnot(!g$valid,g$carried_gap)
gap$state_valid <- g$valid
following <- list(step=3L,p=3,b=3,h=0,lc=0,tof=0,tr=0,k=100)
near(.mofuss_luc_increment(gap,following,TRUE)$delta,-1)
# Exercise the real shared NRB adapter, including stale upgraded-model metadata:
# actual legacy TIFF NoData tags must still trigger recovery with a safe model.
nrb_api <- new.env(parent = globalenv())
sys.source("localhost/scripts/helpers/woodfuel_nrb_attribution.R", envir = nrb_api)
raster::rasterOptions(tmpdir = work)
model_path <- file.path(bau,"10_dyn_fixture_v14.egoml")
model_text <- paste(readLines(model_path),collapse="\n")
for (null_value in c(".default", "-1e30")) {
  ledger_node <- paste0('<containerfunctor name="CalculateMap"><inputport name="nullValue">',
    null_value,'</inputport><outputport name="result" id="v93002"/></containerfunctor></script>')
  writeLines(sub("</script>",ledger_node,model_text,fixed=TRUE),model_path)
  context <- nrb_api$mofuss_nrb_context(bau, expected_steps=3, mc=1)
  stopifnot(context$reconstruct_signed_balance,
    context$signed_balance_read_policy == "physical_period_reconstruction_legacy_nodata_v1")
  nr <- nrb_api$mofuss_period_nrb(context,1,3,"previous_postharvest")
  nv <- raster::getValues(nr$nrb); fv <- raster::getValues(nr$fnrb)
  near(nv[c(1,2,3,4,5,7,8,9)],c(20,20,0,40,0,40,98,20))
  near(fv[c(1,2,3,4,5,7,8,9)],c(100,100,0,100,0,100,98,100))
  stopifnot(is.na(nv[6]),is.na(fv[6]))
  zero <- nrb_api$mofuss_period_nrb(context,2,3,"previous_postharvest")
  near(raster::getValues(zero$nrb)[1],0)
  stopifnot(is.na(raster::getValues(zero$fnrb)[1]))
}
## Frozen Woodman preserves the freeze year's transition once, then only its
## cover/TOF. Calendar-year output steps and the period opening stock continue.
frozen_fixture <- function(root, is_ics) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  model <- "10_dyn_fixture_v14.egoml"
  writeLines(paste0('<script><property key="mofuss.nrb.attribution.contract" value="woodfuel_attributed_signed_balance_v1"/>',
    '<property key="mofuss.woodman.freeze.contract" value="woodman_freeze_year_v1"/>',
    '<functor><inputport name="constant">.no</inputport><outputport id="v253"/></functor>',
    '<functor><inputport name="constant">3</inputport><outputport id="v302"/></functor></script>'),
    file.path(root, model))
  jsonlite::write_json(list(model = model, luc = 3),
    file.path(root, "windows_performance_preparation.json"), auto_unbox = TRUE)
  dir.create(file.path(root, "LULCC/TempTables"), recursive = TRUE, showWarnings = FALSE)
  write.csv(data.frame(Var = c("start_year", "end_year", "monte_carlo_runs", "uncapped_regrowth",
    "woodman_luc_freeze_year"), ParCHR = c(2000,2002,1,0,2001)),
    file.path(root, "LULCC/TempTables/parameters_dinamica.csv"), row.names = FALSE)
  cell <- function(value) c(value, rep(NA_real_, 8L))
  put(root, "Temp/2_IniSt01.tif", cell(100))
  put(root, "Temp/2_TOFvsFOR01.tif", cell(0))
  put(root, "LULCC/TempRaster/agb3_c.tif", cell(100))
  put(root, "LULCC/TempRaster/LULCt3_c.tif", cell(0))
  write.csv(data.frame(Key = 0, Value = 100), file.path(root, "Temp/mc_k_01.csv"), row.names = FALSE)
  for (year in 2000:2001) {
    put(root, sprintf("LULCC/TempRaster/LULCt3_c_%d.tif", year), cell(0))
    put(root, sprintf("LULCC/TempRaster/TOFvsFOR_mask3_%d.tif", year), cell(0))
    put(root, sprintf("LULCC/TempRaster/LULCt3_transition_%d.tif", year), cell(if (year == 2001) 2 else 0))
  }
  # No annual 2002 LUC/TOF/transition rasters exist: frozen input selection
  # must not require them or repeat the 2001 new-forest reset.
  post <- if (is_ics) c(100,0,20) else c(80,0,15)
  ledger <- if (is_ics) c(0,0,-20) else c(20,20,5)
  for (step in 1:3) {
    pre <- c(100,0,20)[step]
    put(root, sprintf("debugging_1/Growth_less_harv%02d.tif", step), cell(post[step]))
    put(root, sprintf("debugging_1/Growth%02d.tif", step), cell(pre))
    put(root, sprintf("debugging_1/Harvest_tot%02d.tif", step), cell(pre-post[step]))
    put(root, sprintf("debugging_1/Woodfuel_balance%02d.tif", step), cell(ledger[step]))
  }
  execution_path <- file.path(root, "debugging_1/woodman_luc_execution.csv")
  if (is_ics) write.csv(data.frame(Key=1:4, Value=c(3,2001,1,2000)),execution_path,row.names=FALSE)
  else writeLines(c("Key*, Value,","1, 3,","2, 2001,","3, 1,","4, 2000"),execution_path)
}
fb <- file.path(work,"frozen_BAU"); fi <- file.path(work,"frozen_ICS")
frozen_fixture(fb,FALSE); frozen_fixture(fi,TRUE)
fp <- mofuss_luc_pair_status(fb,fi,1,2001,2002)
stopifnot(fp$bs$years[[3]]$year == 2002, fp$bs$years[[3]]$land_cover_year == 2001,
          fp$bs$years[[3]]$transition == 0L,
          fp$bs$years[[2]]$transition != 0L)
fr <- mofuss_luc_decompose_pair(fb,fi,1,2001,2002,NULL,temp_dir=work)
near(fr$summary$direct_luc_loss_mg,20); near(fr$summary$raw_signed_woodfuel_effect_mg,5)
near(fr$summary$net_stock_benefit_mg,-15); near(fr$summary$aggregate_reconciliation_error_mg,0)
stopifnot(fr$summary$woodman_luc_freeze_year == 2001L,
          identical(fr$annual$woodman_transitions_enabled,c(TRUE,FALSE)),
          all(fr$annual$land_cover_year == 2001L))
freeze_nrb <- nrb_api$mofuss_nrb_context(fb,expected_steps=3,mc=1)
stopifnot(!freeze_nrb$reconstruct_signed_balance,
          freeze_nrb$woodman_luc_freeze_year == 2001L)
near(raster::getValues(nrb_api$mofuss_period_nrb(freeze_nrb,2,3,"previous_postharvest")$nrb)[1],0)
fr_rec <- mofuss_reconstruct_signed_period(fb,1,2,3,"previous_postharvest",work)
near(values(fr_rec$signed)[1,1],-15)
execution_path <- file.path(fi,"debugging_1/woodman_luc_execution.csv")
write.csv(data.frame(Key=1:4,Value=c(3,2000,1,2000)),execution_path,row.names=FALSE)
fails(mofuss_luc_pair_status(fb,fi,1,2001,2002),"BAU/ICS mismatch: woodman_luc_freeze_year")
write.csv(data.frame(Key=1:4,Value=c(3,2051,1,2000)),execution_path,row.names=FALSE)
fails(mofuss_luc_pair_status(fb,fi,1,2001,2002),"Conflicting executed Woodman")
file.rename(execution_path,paste0(execution_path,".held"))
fails(mofuss_luc_pair_status(fb,fi,1,2001,2002),"Missing executed Woodman freeze provenance")
fails(nrb_api$mofuss_nrb_context(fi,expected_steps=3,mc=1),"Missing executed Woodman freeze provenance")
file.rename(paste0(execution_path,".held"),execution_path)
cat("WOODFUEL_LUC_DECOMPOSITION_TESTS_OK\n")
