#!/usr/bin/env Rscript
# Reconstruct recorded Dinamica origin pressures, then account for model-implied
# harvest by demand-origin and source country. This is not observed trade.
# Sourceable: no work runs on source(). Run rs_main() or invoke with Rscript.
# Runtime masks/bases/scalars must come from the v12 capture contract; the
# existing annual model rasters remain the authority for total realised harvest.

.rs_stop <- function(...) stop(sprintf(...), call. = FALSE)
.rs_require <- function() {
  for (pkg in c("terra", "data.table")) {
    if (!requireNamespace(pkg, quietly = TRUE)) .rs_stop("Package '%s' is required", pkg)
  }
}
.rs_f32 <- function(x) {
  out <- readBin(writeBin(as.double(x), raw(), size = 4L),
                 what = "double", n = length(x), size = 4L)
  out[is.na(out)] <- NA_real_
  out
}
.rs_zero <- function(x) { x[is.na(x)] <- 0; x }
.rs_ratio <- function(x, y) {
  out <- numeric(length(x)); good <- is.finite(y) & y > 0
  out[good] <- x[good] / y[good]; out
}

# The codec writes exactly representable integer pieces. hi can be signed.
# x = (hi / 2^26 + lo / 2^53) * 2^e. Values in this contract are finite.
.rs_decode <- function(e, hi, lo) {
  if (any(!is.finite(c(e, hi, lo))) || any(e != trunc(e)) ||
      any(hi != trunc(hi)) || any(lo != trunc(lo)) ||
      any(e < -1022 | e > 1023) || any(lo < 0 | lo >= 2^27)) {
    .rs_stop("Invalid binary64 scalar codec")
  }
  value <- (hi / 2^26 + lo / 2^53) * 2^e
  if (any(!is.finite(value))) .rs_stop("Nonfinite decoded scalar")
  value
}
.rs_scalars <- function(path) {
  if (!file.exists(path)) .rs_stop("Missing runtime scalar capture: %s", path)
  tab <- data.table::fread(path, showProgress = FALSE)
  if (ncol(tab) < 2L) .rs_stop("Invalid runtime scalar table: %s", path)
  key <- suppressWarnings(as.integer(tab[[1L]])); value <- as.numeric(tab[[2L]])
  if (anyNA(key) || anyDuplicated(key) ||
      !(setequal(key, 1:18) || setequal(key, 1:24))) {
    .rs_stop("Expected codec keys 1..18 (v12) or 1..24 (v13 W) in %s", path)
  }
  nkey <- length(key); value <- value[match(seq_len(nkey), key)]
  decoded <- .rs_decode(value[seq(1,nkey,3)], value[seq(2,nkey,3)], value[seq(3,nkey,3)])
  names(decoded) <- c("denominator", "demand", "weeks", "slices", "adjustment", "source_step",
                     if(nkey==24)c("origin_tof_shortfall","origin_forest_denominator"))
  if (decoded[["denominator"]] < 0 || decoded[["demand"]] < 0 ||
      decoded[["weeks"]] <= 0 || decoded[["slices"]] <= 0 ||
      decoded[["adjustment"]] > 100 ||
      !decoded[["source_step"]] %in% c(1,11,21,31,41,51)) {
    .rs_stop("Runtime scalars outside supported ranges: %s", path)
  }
  if(nkey==24 && any(decoded[c("origin_tof_shortfall","origin_forest_denominator")]<0))
    .rs_stop("Negative v13 TOF scalar: %s",path)
  decoded
}
.rs_eligible <- function(base, state) {
  if (length(base) != length(state)) .rs_stop("Base/mask length mismatch")
  if (any(!is.na(state) & !state %in% c(0, 1, 2, 255))) .rs_stop("Invalid runtime mask state")
  pressure <- base
  pressure[!is.na(state) & state == 0] <- 0
  pressure[!is.na(state) & state == 1] <- .rs_f32(base[!is.na(state) & state == 1] * 0)
  pressure[is.na(state) | state == 255] <- NA_real_
  pressure
}
.rs_normalize <- function(base, state, scalars) {
  pressure <- .rs_eligible(base,state)
  if (scalars[["demand"]] <= 0 || scalars[["denominator"]] <= 0) return(rep(0, length(base)))
  # Preserve the original ordered double expression and its final float32 cast.
  out <- pressure * scalars[["demand"]]
  out <- out * scalars[["weeks"]]
  out <- out / scalars[["slices"]]
  out <- out * (100 - scalars[["adjustment"]])
  out <- out / 100
  out <- out / scalars[["denominator"]]
  .rs_f32(out)
}
.rs_origin_tof <- function(base,state,forest,scalars) {
  if(!all(c("origin_tof_shortfall","origin_forest_denominator") %in% names(scalars)))
    .rs_stop("Missing v13 TOF scalar capture")
  if(any(!is.na(forest) & !forest %in% c(0,1,255))) .rs_stop("Invalid forest-state mask")
  if(scalars[["origin_tof_shortfall"]]<=0 || scalars[["origin_forest_denominator"]]<=0)
    return(rep(0,length(base)))
  weight <- .rs_eligible(base,state)
  weight[is.na(forest) | forest==0 | forest==255] <- NA_real_
  .rs_f32(weight*scalars[["origin_tof_shortfall"]]/scalars[["origin_forest_denominator"]])
}
.rs_target <- function(s) s[["demand"]] * s[["weeks"]] / s[["slices"]] *
  (100 - s[["adjustment"]]) / 100
.rs_accumulate <- function(components, union_null = FALSE, initial = NULL) {
  if (!length(components)) .rs_stop("No origin components")
  total <- if(is.null(initial))components[[1L]] else initial
  first <- if(is.null(initial))2L else 1L
  if (length(components) >= first) for (i in seq.int(first,length(components))) {
    value <- components[[i]]
    if (union_null) {
      both <- !is.na(total) & !is.na(value)
      missing <- is.na(total)
      total[both] <- .rs_f32(total[both] + value[both])
      total[missing] <- value[missing]
    } else total <- .rs_f32(total + value)
  }
  total
}
.rs_match_pressure <- function(rebuilt, saved, label, domain) {
  # Initial accumulator is null outside the analysis landscape. Those pixels
  # have no source-country zone and cannot contribute to the country ledger.
  use <- domain & (!is.na(rebuilt) | !is.na(saved))
  different <- use & (is.na(rebuilt) != is.na(saved) |
                       (!is.na(rebuilt) & !is.na(saved) & rebuilt != saved))
  different[is.na(different)] <- FALSE
  if (any(different)) {
    delta <- abs(rebuilt[different] - saved[different])
    finite_delta <- delta[is.finite(delta)]
    .rs_stop("Reconstructed %s pressure differs from saved model output at %d cells (max absolute difference %s). No attribution accepted.",
             label, sum(different), if(length(finite_delta))format(max(finite_delta),scientific=TRUE) else "null-mask mismatch")
  }
  invisible(TRUE)
}

.rs_periods <- function(spec) {
  data.table::rbindlist(lapply(spec, function(s) {
    x <- suppressWarnings(as.integer(strsplit(s, ":", fixed=TRUE)[[1L]]))
    if (length(x) != 2L || anyNA(x) || x[1] > x[2]) .rs_stop("Invalid period: %s", s)
    data.table::data.table(period=sprintf("%d-%d", x[1], x[2]), period_start=x[1], period_end=x[2])
  }))
}
.rs_snapshot_input <- function(run,relative) {
  current<-file.path(run,relative)
  frozen<-file.path(run,"Sourcing","metadata","input_snapshot",relative)
  if(!file.exists(frozen))return(current)
  if(!file.exists(current)).rs_stop("Current input is missing but a frozen run snapshot exists: %s",current)
  same<-file.info(current)$size==file.info(frozen)$size &&
    identical(readBin(current,"raw",n=file.info(current)$size),readBin(frozen,"raw",n=file.info(frozen)$size))
  if(!same).rs_stop("Current input differs from frozen runtime snapshot: %s. Restore the run inputs or inspect the archived snapshot before attribution.",current)
  frozen
}
.rs_meta <- function(run) {
  path <- .rs_snapshot_input(run,file.path("LULCC", "TempTables", "parameters_dinamica.csv"))
  x <- data.table::fread(path, showProgress=FALSE)
  p <- setNames(as.character(x[[2L]]), as.character(x[[1L]]))
  required <- c("start_year", "end_year", "monte_carlo_runs", "uncapped_regrowth")
  if (!all(required %in% names(p))) .rs_stop("Incomplete parameters: %s", path)
  list(run=normalizePath(run,winslash="/",mustWork=TRUE), run_name=basename(run),
       start=as.integer(p[["start_year"]]), end=as.integer(p[["end_year"]]),
       mc=as.integer(p[["monte_carlo_runs"]]),
       provenance=if(all(file.exists(file.path(run,"Sourcing","metadata","input_snapshot",
           c("LULCC/TempTables/parameters_dinamica.csv","In/DemandScenarios/W_origin_component_index.csv",
             "In/DemandScenarios/V_origin_component_index.csv")))))
           "frozen_parameters_and_component_indices_verified" else "no_complete_frozen_input_snapshot; live_metadata_used",
       regrowth=if (p[["uncapped_regrowth"]] == "1") "uncapped" else "capped")
}
.rs_index <- function(run, channel) {
  path <- .rs_snapshot_input(run,file.path("In","DemandScenarios",sprintf("%s_origin_component_index.csv",channel)))
  x <- data.table::fread(path, showProgress=FALSE)
  required <- c("ComponentIndex","DemandISO3","AllowedSourceISO3")
  if (!all(required %in% names(x)) || !nrow(x) || anyDuplicated(x$ComponentIndex) ||
      anyDuplicated(x$DemandISO3) || any(!grepl("^[A-Z]{3}$",x$DemandISO3))) {
    .rs_stop("Expected one country per component, with unique indices/ISO3 codes: %s",path)
  }
  data.table::setorder(x, ComponentIndex)
  x
}
.rs_crosswalk <- function(path) {
  if (tolower(tools::file_ext(path)) %in% c("gpkg","shp")) {
    x <- data.table::as.data.table(as.data.frame(terra::vect(path)))
  } else x <- data.table::fread(path, showProgress=FALSE)
  if (all(c("ID","GID_0","NAME_0") %in% names(x))) {
    x <- x[,list(source_id=as.integer(ID),source_iso3=as.character(GID_0),source_name=as.character(NAME_0))]
  } else if(all(c("CountryID","GID_0","NAME_0") %in% names(x))) {
    x <- x[,list(source_id=as.integer(CountryID),source_iso3=as.character(GID_0),source_name=as.character(NAME_0))]
  } else if (all(c("source_id","source_iso3") %in% names(x))) {
    if (!"source_name" %in% names(x)) x[,source_name:=source_iso3]
    x <- x[,list(source_id=as.integer(source_id),source_iso3=as.character(source_iso3),source_name=as.character(source_name))]
  } else .rs_stop("Crosswalk needs ID/GID_0/NAME_0 or source_id/source_iso3/source_name")
  if (!nrow(x) || anyNA(x) || anyDuplicated(x$source_id) || anyDuplicated(x$source_iso3)) {
    .rs_stop("Country crosswalk has missing or duplicate identifiers")
  }
  x
}
.rs_zsum <- function(x, zone, ids) {
  use <- !is.na(zone) & is.finite(x)
  out <- numeric(length(ids))
  if (any(use)) {
    sums <- rowsum(x[use], zone[use], reorder=FALSE)
    idx <- match(as.integer(rownames(sums)),ids)
    if (anyNA(idx)) .rs_stop("Country zone is missing from crosswalk")
    out[idx] <- sums[,1L]
  }
  out
}
.rs_maps <- c("Proj_harv_Wtot","Proj_harv_Vtot","Proj_harv_Wdef","Proj_harv_Vdef",
              "Non_harv_AGR","Ex_agr_harv","harv_AGR","Expect_harv_tot","Harvest_tot")

.rs_year <- function(meta, mc, year, zones, crosswalk, indices, block_mb=64, signed_policy="report") {
  step <- year-meta$start+1L
  cap <- file.path(meta$run,"Sourcing",sprintf("MC%03d",mc))
  index <- data.table::rbindlist(lapply(c("W","V"),function(ch) {
    x <- data.table::copy(indices[[ch]]); x[,channel:=ch]; x
  }),fill=TRUE)
  scalars <- lapply(seq_len(nrow(index)),function(k) {
    .rs_scalars(file.path(cap,sprintf("%s_scalars%03d_%02d.csv",index$channel[k],index$ComponentIndex[k],step)))
  })
  source_steps <- vapply(scalars,`[[`,numeric(1),"source_step")
  v13_w <- vapply(scalars,function(s)"origin_tof_shortfall" %in% names(s),logical(1))
  if(any(v13_w & index$channel!="W") || any(v13_w) && !all(v13_w[index$channel=="W"]))
    .rs_stop("Mixed or invalid v12/v13 W scalar schema")
  origin_preserving <- any(v13_w)
  if (length(unique(source_steps)) != 1L) .rs_stop("Components disagree on IDW snapshot for %d MC%d",year,mc)
  base_paths <- vapply(seq_len(nrow(index)),function(k) file.path(meta$run,"Sourcing","static",
                 sprintf("%s_base%03d_%02d.tif",index$channel[k],index$ComponentIndex[k],source_steps[k])),character(1))
  other_paths <- c(vapply(c("W","V"),function(ch) file.path(cap,sprintf("mask_%s%02d.tif",ch,step)),character(1)),
    vapply(.rs_maps,function(stem) file.path(meta$run,sprintf("debugging_%d",mc),sprintf("%s%02d.tif",stem,step)),character(1)))
  paths <- c(base_paths,other_paths,file.path(meta$run,"Sourcing","static","accumulator_domain.tif"),
             if(origin_preserving)file.path(cap,sprintf("forest_state%02d.tif",step)))
  if (any(!file.exists(paths))) .rs_stop("Missing runtime/annual raster: %s",paste(paths[!file.exists(paths)],collapse="; "))
  r <- terra::rast(paths)
  if (!terra::compareGeom(r,zones,stopOnError=FALSE)) .rs_stop("Country zones differ from model grid")
  terra::readStart(r); on.exit(terra::readStop(r),add=TRUE)
  terra::readStart(zones); on.exit(terra::readStop(zones),add=TRUE)
  n <- nrow(index); ids <- crosswalk$source_id; ns <- length(ids)
  measure_names <- c("normalised_pressure_tonnes","clearing_credit_tonnes","requested_harvest_tonnes",
                     "realised_harvest_tonnes","negative_requested_tonnes","negative_realised_tonnes")
  accum <- array(0,dim=c(n,ns,length(measure_names)),dimnames=list(NULL,NULL,measure_names))
  own_tof <- array(0,dim=c(n,ns,4))
  deficit <- numeric(n)
  pooled <- matrix(0,nrow=ns,ncol=4,dimnames=list(NULL,c("requested","realised","negative_requested","negative_realised")))
  total_actual <- total_expected <- total_residual <- abs_residual <- 0
  negative_cells <- unknown_zone_cells <- 0L
  rows <- max(1L,floor(block_mb*1024^2/(terra::ncol(r)*8*(terra::nlyr(r)+n+20))))
  widx <- which(index$channel=="W"); vidx <- which(index$channel=="V")
  for (row in seq.int(1L,terra::nrow(r),by=rows)) {
    nr <- min(rows,terra::nrow(r)-row+1L)
    values <- terra::readValues(r,row=row,nrows=nr,mat=TRUE)
    z <- terra::readValues(zones,row=row,nrows=nr,mat=FALSE)
    known <- !is.na(z)
    if (any(known & !z %in% ids)) .rs_stop("Unknown country zone ID")
    mask_w <- values[,n+1L]; mask_v <- values[,n+2L]
    model <- values[,(n+3L):(n+2L+length(.rs_maps)),drop=FALSE]
    colnames(model) <- .rs_maps
    initial <- values[,n+3L+length(.rs_maps)]
    if(any(!is.na(initial) & !initial %in% c(0,255))) .rs_stop("Invalid accumulator-domain mask")
    initial[is.na(initial) | initial==255]<-NA_real_
    pressure <- lapply(seq_len(n),function(k) .rs_normalize(values[,k],
                      if(index$channel[k]=="W")mask_w else mask_v,scalars[[k]]))
    .rs_match_pressure(.rs_accumulate(pressure[widx],initial=initial),model[,"Proj_harv_Wtot"],"W",known)
    .rs_match_pressure(.rs_accumulate(pressure[vidx],TRUE,initial),model[,"Proj_harv_Vtot"],"V",known)
    actual <- .rs_zero(model[,"Harvest_tot"]); expected <- .rs_zero(model[,"Expect_harv_tot"])
    if (any(actual < 0) || any(actual>0 & expected<=0)) .rs_stop("Invalid positive harvest / final-pressure relation")
    if (any(!known & abs(actual)>1e-7)) .rs_stop("Realised harvest has cells without a source-country zone")
    unknown_zone_cells <- unknown_zone_cells+sum(!known & expected!=0)
    clip <- .rs_ratio(actual,expected)
    if (any(clip>1+1e-6)) .rs_stop("Actual harvest exceeds expected positive pressure")
    denom_w <- Reduce(`+`,lapply(pressure[widx],.rs_zero))
    denom_v <- Reduce(`+`,lapply(pressure[vidx],.rs_zero))
    credited_w <- .rs_zero(model[,"Proj_harv_Wtot"])-.rs_zero(model[,"Proj_harv_Wdef"])
    credited_v <- .rs_zero(model[,"Proj_harv_Vtot"])-.rs_zero(model[,"Proj_harv_Vdef"])
    # harv_AGR is the saved float32 (Wdef - Non_harv_AGR). Keep its signed
    # values; the legacy model may contain negatives when clearing is active.
    direct_w <- .rs_zero(model[,"harv_AGR"])
    direct_v <- .rs_zero(model[,"Proj_harv_Vdef"])
    pool <- .rs_zero(model[,"Ex_agr_harv"])
    if(origin_preserving) {
      origin_pool <- lapply(widx,function(k).rs_origin_tof(values[,k],mask_w,values[,ncol(values)],scalars[[k]]))
      pooled_rebuilt <- .rs_accumulate(origin_pool,TRUE,initial)
      .rs_match_pressure(pooled_rebuilt,model[,"Ex_agr_harv"],"origin-preserving W TOF",known)
    }
    for(k in seq_len(n)) {
      is_w <- index$channel[k]=="W"
      u <- .rs_zero(pressure[[k]])
      fraction <- .rs_ratio(u,if(is_w)denom_w else denom_v)
      requested <- fraction*if(is_w)direct_w else direct_v
      realised <- requested*clip
      credit <- fraction*if(is_w)credited_w else credited_v
      if (is_w) deficit[k] <- deficit[k]+sum((fraction*.rs_zero(model[,"Non_harv_AGR"]))[known],na.rm=TRUE)
      if(is_w && origin_preserving) {
        redist <- .rs_zero(origin_pool[[match(k,widx)]])
        parts <- list(redist,redist*clip,pmax(-redist,0),pmax(-redist*clip,0))
        for(j in seq_along(parts))own_tof[k,,j]<-own_tof[k,,j]+.rs_zsum(parts[[j]],z,ids)
      }
      arrays <- list(u,credit,requested,realised,pmax(-requested,0),pmax(-realised,0))
      for(j in seq_along(arrays)) accum[k,,j] <- accum[k,,j]+.rs_zsum(arrays[[j]],z,ids)
      negative_cells <- negative_cells+sum(requested[known]<0)
    }
    pooled_arrays <- list(pool,pool*clip,pmax(-pool,0),pmax(-pool*clip,0))
    for(j in seq_along(pooled_arrays)) pooled[,j] <- pooled[,j]+.rs_zsum(pooled_arrays[[j]],z,ids)
    residual <- actual-(direct_w+direct_v+pool)*clip
    total_residual <- total_residual+sum(residual[known])
    abs_residual <- abs_residual+sum(abs(residual[known]))
    total_actual <- total_actual+sum(actual[known]); total_expected <- total_expected+sum(expected[known])
  }
  if (negative_cells && signed_policy=="error") .rs_stop("Signed negative W adjustments occurred in %d component-cells; use --signed-policy=report for explicit signed accounting",negative_cells)
  if (abs_residual > max(1e-5,abs(total_actual)*2e-6)) .rs_stop("Attribution does not reconcile with actual model harvest: absolute residual %.12g",abs_residual)
  dtotal <- sum(deficit)
  if (dtotal<=0 && sum(abs(pooled))>1e-6) .rs_stop("Pooled TOF redistribution has no attributed origin deficit")
  if(origin_preserving) deficit[widx]<-vapply(scalars[widx],`[[`,numeric(1),"origin_tof_shortfall")
  matrix_rows <- lapply(seq_len(n),function(k) {
    origin <- index$DemandISO3[k]
    x <- data.table::as.data.table(matrix(accum[k,,],nrow=ns,ncol=length(measure_names)))
    data.table::setnames(x,measure_names)
    x[,`:=`(origin_iso3=origin,channel=index$channel[k],term="direct",source_id=ids)]
    if (index$channel[k]=="W") {
      share <- if(dtotal>0)deficit[k]/dtotal else 0
      attributed_pool <- if(origin_preserving)matrix(own_tof[k,,],nrow=ns,ncol=4) else pooled*share
      y <- data.table::data.table(normalised_pressure_tonnes=0,clearing_credit_tonnes=0,
        requested_harvest_tonnes=attributed_pool[,1L],realised_harvest_tonnes=attributed_pool[,2L],
        negative_requested_tonnes=attributed_pool[,3L],negative_realised_tonnes=attributed_pool[,4L],
        origin_iso3=origin,channel="W",term=if(origin_preserving)"origin_preserving_TOF_redistribution" else "pooled_TOF_redistribution",source_id=ids)
      x <- data.table::rbindlist(list(x,y))
    }
    allowed <- strsplit(index$AllowedSourceISO3[k],";",fixed=TRUE)[[1L]]
    x[,allowed_bilateral_source:=crosswalk$source_iso3[match(source_id,ids)] %in% allowed]
    x
  })
  matrix <- data.table::rbindlist(matrix_rows)
  matrix <- merge(matrix,crosswalk,by="source_id",all.x=TRUE,sort=FALSE)
  attributed_total <- sum(matrix$realised_harvest_tonnes)
  if(abs(attributed_total-total_actual)>max(1e-5,abs(total_actual)*2e-6))
    .rs_stop("Origin/source attribution differs from actual model harvest: %.12g",attributed_total-total_actual)
  matrix[,`:=`(run_name=meta$run_name,regrowth=meta$regrowth,mc_run=mc,year=year,
               period_endpoint_rule="inclusive; adjacent periods overlap")]
  demand <- data.table::data.table(origin_iso3=index$DemandISO3,channel=index$channel,
        demand_tonnes=vapply(scalars,.rs_target,numeric(1)),tof_deficit_tonnes=deficit,
        zero_normaliser=vapply(scalars,function(s)s[["denominator"]]<=0,logical(1)),
        run_name=meta$run_name,regrowth=meta$regrowth,mc_run=mc,year=year)
  qa <- data.table::data.table(run_name=meta$run_name,regrowth=meta$regrowth,mc_run=mc,year=year,
       actual_model_harvest_tonnes=total_actual,expected_model_harvest_tonnes=total_expected,
       attributed_realised_harvest_tonnes=attributed_total,
       origin_source_reconciliation_residual_tonnes=total_actual-attributed_total,
       attribution_rounding_residual_tonnes=total_residual,absolute_rounding_residual_tonnes=abs_residual,
       negative_component_cells=negative_cells,unmapped_expected_cells=unknown_zone_cells,
       tof_mechanism=if(origin_preserving)"origin_preserving_v13" else "pooled_v12",
       metadata_provenance=if(is.null(meta$provenance))"diagnostic_fixture_no_snapshot" else meta$provenance,
       exact_normalised_pressure_check="passed on country-zone domain",
       forbidden_direct_W_realised_tonnes=matrix[channel=="W" & term=="direct" & source_iso3!=origin_iso3,
                                                 sum(pmax(realised_harvest_tonnes,0))],
       crossborder_pooled_W_realised_tonnes=matrix[channel=="W" & term=="pooled_TOF_redistribution" & source_iso3!=origin_iso3,
                                                  sum(pmax(realised_harvest_tonnes,0))],
       crossborder_origin_preserving_W_realised_tonnes=matrix[channel=="W" & term=="origin_preserving_TOF_redistribution" & source_iso3!=origin_iso3,
                                                              sum(pmax(realised_harvest_tonnes,0))],
       forbidden_direct_V_realised_tonnes=matrix[channel=="V" & !allowed_bilateral_source,
                                                 sum(pmax(realised_harvest_tonnes,0))])
  list(matrix=matrix,demand=demand,qa=qa)
}

.rs_summarize <- function(matrix,demand,periods=NULL) {
  numeric_cols <- c("normalised_pressure_tonnes","clearing_credit_tonnes","requested_harvest_tonnes",
                    "realised_harvest_tonnes","negative_requested_tonnes","negative_realised_tonnes")
  if (!is.null(periods)) {
    pairs <- lapply(seq_len(nrow(periods)),function(k) {
      p <- periods[k]; years <- seq.int(p$period_start,p$period_end)
      # Missing years must never silently make a partial period look complete.
      coverage <- unique(demand[,list(run_name,regrowth,mc_run,year)])
      counts <- coverage[year %in% years,.N,by=c("run_name","regrowth","mc_run")]
      if (any(counts$N!=length(years))) .rs_stop("Incomplete annual capture for period %s",p$period)
      m <- matrix[year %in% years,lapply(.SD,sum),
          by=c("run_name","regrowth","mc_run","origin_iso3","channel","term","source_id","source_iso3","source_name","allowed_bilateral_source"),.SDcols=numeric_cols]
      d <- demand[year %in% years,list(demand_tonnes=sum(demand_tonnes),tof_deficit_tonnes=sum(tof_deficit_tonnes),
          zero_normaliser=any(zero_normaliser)),by=c("run_name","regrowth","mc_run","origin_iso3","channel")]
      m[,`:=`(period=p$period,period_start=p$period_start,period_end=p$period_end)]
      d[,`:=`(period=p$period,period_start=p$period_start,period_end=p$period_end)]
      list(m=m,d=d)
    })
    matrix <- data.table::rbindlist(lapply(pairs,`[[`,"m"))
    demand <- data.table::rbindlist(lapply(pairs,`[[`,"d"))
  }
  time <- if(is.null(periods))"year" else c("period","period_start","period_end")
  combined_keys<-c("run_name","regrowth","mc_run",time,"origin_iso3","term",
                   "source_id","source_iso3","source_name")
  combined_matrix<-matrix[,lapply(.SD,sum),by=combined_keys,.SDcols=numeric_cols]
  combined_matrix[,`:=`(channel="W+V",allowed_bilateral_source=NA)]
  combined_demand<-demand[,list(demand_tonnes=sum(demand_tonnes),tof_deficit_tonnes=sum(tof_deficit_tonnes),
                                zero_normaliser=any(zero_normaliser)),
                          by=c("run_name","regrowth","mc_run",time,"origin_iso3")]
  combined_demand[,channel:="W+V"]
  matrix<-data.table::rbindlist(list(matrix,combined_matrix),fill=TRUE)
  demand<-data.table::rbindlist(list(demand,combined_demand),fill=TRUE)
  keys <- c("run_name","regrowth","mc_run",time,"origin_iso3","channel")
  origin <- matrix[,list(normalised_pressure_tonnes=sum(normalised_pressure_tonnes),
      clearing_credit_tonnes=sum(clearing_credit_tonnes),requested_harvest_tonnes=sum(requested_harvest_tonnes),
      realised_harvest_tonnes=sum(realised_harvest_tonnes),
      domestic_harvest_tonnes=sum(realised_harvest_tonnes[source_iso3==origin_iso3]),
      imported_harvest_tonnes=sum(realised_harvest_tonnes[source_iso3!=origin_iso3]),
      pooled_TOF_harvest_tonnes=sum(realised_harvest_tonnes[term=="pooled_TOF_redistribution"]),
      origin_preserving_TOF_harvest_tonnes=sum(realised_harvest_tonnes[term=="origin_preserving_TOF_redistribution"]),
      negative_realised_tonnes=sum(negative_realised_tonnes),negative_requested_tonnes=sum(negative_requested_tonnes)),by=keys]
  origin <- merge(origin,demand,by=keys,all=TRUE,sort=FALSE)
  origin[,`:=`(signed_adjustments_present=negative_requested_tonnes>0 | negative_realised_tonnes>0,
               unmet_demand_tonnes=demand_tonnes-clearing_credit_tonnes-realised_harvest_tonnes)]
  origin[,domestic_share_pct:=ifelse(!signed_adjustments_present & realised_harvest_tonnes>0,
                                    100*domestic_harvest_tonnes/realised_harvest_tonnes,NA_real_)]
  origin[,import_share_pct:=ifelse(!signed_adjustments_present & realised_harvest_tonnes>0,
                                  100*imported_harvest_tonnes/realised_harvest_tonnes,NA_real_)]
  totals <- origin[,c(keys,"realised_harvest_tonnes","signed_adjustments_present"),with=FALSE]
  data.table::setnames(totals,"realised_harvest_tonnes","origin_total_realised_tonnes")
  matrix <- merge(matrix,totals,by=keys,all.x=TRUE,sort=FALSE)
  matrix[,source_share_pct:=ifelse(!signed_adjustments_present & origin_total_realised_tonnes>0,
                                   100*realised_harvest_tonnes/origin_total_realised_tonnes,NA_real_)]
  list(matrix=matrix,origin=origin)
}
.rs_mc_summary<-function(origin) {
  keys<-c("run_name","regrowth","period","period_start","period_end","origin_iso3","channel")
  metrics<-c("demand_tonnes","realised_harvest_tonnes","domestic_harvest_tonnes","imported_harvest_tonnes",
             "domestic_share_pct","import_share_pct","unmet_demand_tonnes","clearing_credit_tonnes")
  long<-data.table::melt(origin,id.vars=c(keys,"mc_run"),measure.vars=metrics,
                          variable.name="metric",value.name="value")
  long[,{
    valid<-value[is.finite(value)]
    list(mc_count=.N,valid_mc_count=length(valid),mean=if(length(valid))mean(valid)else NA_real_,
         p025=if(length(valid))as.numeric(stats::quantile(valid,.025,names=FALSE))else NA_real_,
         p975=if(length(valid))as.numeric(stats::quantile(valid,.975,names=FALSE))else NA_real_)
  },by=c(keys,"metric")]
}

rs_main <- function(args=commandArgs(trailingOnly=TRUE)) {
  .rs_require()
  cfg <- list(runs=character(),zones=NULL,crosswalk=NULL,output=NULL,mc=integer(),
     periods=c("2020:2030","2030:2040","2040:2050","2020:2050"),block_mb=64,signed_policy="error",overwrite=FALSE)
  for(arg in args) {
    if(arg %in% c("--help","-h")) {
      cat("Usage: Rscript 2post_runtime_sourcing_v1.R --run-dir=PATH [repeat] --zones=RASTER --crosswalk=CSV_OR_GPKG --output-dir=PATH\n",
          "Optional: --periods=2020:2030,2030:2040,2040:2050,2020:2050 --mc-runs=1:3 --block-mb=64 --signed-policy=report|error --overwrite=YES\n",
          "Signed policy defaults to error. Periods have inclusive endpoints. Outputs describe model-implied accounting, not observed trade.\n",sep="")
      return(invisible(NULL))
    }
    key <- sub("=.*$","",arg); value <- sub("^[^=]*=","",arg)
    switch(key,
      "--run-dir"={cfg$runs<-c(cfg$runs,value)},"--scenario-dir"={cfg$runs<-c(cfg$runs,value)},
      "--zones"={cfg$zones<-value},"--crosswalk"={cfg$crosswalk<-value},"--boundaries"={cfg$crosswalk<-value},
      "--output-dir"={cfg$output<-value},"--periods"={cfg$periods<-strsplit(value,",",fixed=TRUE)[[1L]]},
      "--mc-runs"={cfg$mc<-if(grepl(":",value,fixed=TRUE)){p<-as.integer(strsplit(value,":",fixed=TRUE)[[1L]]);seq.int(p[1],p[2])}else as.integer(strsplit(value,",",fixed=TRUE)[[1L]])},
      "--block-mb"={cfg$block_mb<-as.numeric(value)},"--signed-policy"={cfg$signed_policy<-value},
      "--overwrite"={cfg$overwrite<-toupper(value)%in%c("YES","TRUE","1")},.rs_stop("Unknown argument: %s",arg))
  }
  if(!length(cfg$runs) || any(vapply(cfg[c("zones","crosswalk","output")],is.null,logical(1))))
    .rs_stop("run-dir, zones, crosswalk, and output-dir are required")
  if(!cfg$signed_policy %in% c("report","error") || !is.finite(cfg$block_mb) || cfg$block_mb<1)
    .rs_stop("Invalid signed policy or block size")
  periods <- .rs_periods(cfg$periods)
  zones <- terra::rast(cfg$zones); if(terra::nlyr(zones)!=1L).rs_stop("Zones must have one layer")
  cw <- .rs_crosswalk(cfg$crosswalk)
  filenames <- c("annual_sourcing_matrix.csv","annual_origin_balance.csv","period_sourcing_matrix.csv",
                 "period_origin_balance.csv","runtime_sourcing_qa.csv","period_origin_mc_summary.csv","README_runtime_sourcing.txt")
  if(!cfg$overwrite && any(file.exists(file.path(cfg$output,filenames)))) .rs_stop("Output exists; use a new directory or --overwrite=YES")
  results <- list(); counter<-0L
  for(run in unique(cfg$runs)) {
    meta <- .rs_meta(run)
    frozen_cw_path<-file.path(run,"Sourcing","metadata","country_crosswalk.csv")
    if(file.exists(frozen_cw_path)) {
      frozen_cw<-.rs_crosswalk(frozen_cw_path)
      expected<-frozen_cw[order(source_id),list(source_id,source_iso3)]
      supplied<-cw[order(source_id),list(source_id,source_iso3)]
      if(!identical(as.data.frame(expected),as.data.frame(supplied)))
        .rs_stop("Supplied country zones/crosswalk mapping disagrees with the frozen run crosswalk: %s",frozen_cw_path)
    }
    years <- seq.int(min(periods$period_start),max(periods$period_end))
    if(any(years<meta$start | years>meta$end)).rs_stop("Requested period outside run years for %s",meta$run_name)
    mc_runs<-if(length(cfg$mc))cfg$mc else seq_len(meta$mc)
    if(anyNA(mc_runs)||any(mc_runs<1|mc_runs>meta$mc)).rs_stop("MC index outside configured run count")
    indices<-setNames(lapply(c("W","V"),function(ch).rs_index(run,ch)),c("W","V"))
    if(!setequal(indices$W$DemandISO3,cw$source_iso3)||!setequal(indices$V$DemandISO3,cw$source_iso3))
      .rs_stop("Component countries and source-country crosswalk differ")
    for(mc in mc_runs) for(year in years) {
      cat(sprintf("Runtime sourcing: %s MC%d year %d\n",meta$run_name,mc,year))
      counter<-counter+1L
      results[[counter]]<-.rs_year(meta,mc,year,zones,cw,indices,cfg$block_mb,cfg$signed_policy)
    }
  }
  matrix<-data.table::rbindlist(lapply(results,`[[`,"matrix"))
  demand<-data.table::rbindlist(lapply(results,`[[`,"demand"))
  qa<-data.table::rbindlist(lapply(results,`[[`,"qa"))
  annual<-.rs_summarize(matrix,demand); period<-.rs_summarize(matrix,demand,periods)
  dir.create(cfg$output,recursive=TRUE,showWarnings=FALSE)
  outputs<-list(annual$matrix,annual$origin,period$matrix,period$origin,qa,.rs_mc_summary(period$origin))
  for(i in seq_along(outputs))data.table::fwrite(outputs[[i]],file.path(cfg$output,filenames[i]))
  writeLines(c("Model-implied sourcing accounting from recorded runtime origin pressures; not observed trade.",
    "Origin float32 pressures are reconstructed with exact binary64 scalar captures and checked against saved aggregate W/V pressure on mapped country cells.",
    "After reconstructing pressure, clearing reductions and the final common biomass clip are attributed proportionally to origin pressure. This is an explicit accounting rule, not an independently simulated priority among countries.",
    "Clearing credits are allocated in proportion to origin pressure. Their country is the pixel where the credit was APPLIED, not the country where clearing biomass was produced. They are not an observed source of wood.",
    "Pooled TOF redistribution uses each W origin's total TOF deficit share. It is explicit because the legacy model pools that pressure across the region.",
    "When 24-key W captures are present, each v13 origin TOF pressure is reconstructed separately and reported as origin_preserving_TOF_redistribution; it is not pooled.",
    "Signed negative adjustments are retained. Domestic/import/source percentages are withheld when those adjustments occur; negative harvest is accounting, not physical trade.",
    "The CLI defaults to --signed-policy=error. Use --signed-policy=report explicitly only when inspecting legacy signed adjustments.",
    "Each period includes both endpoints. Adjacent decades overlap at 2030 and 2040; use the independent 2020-2050 row for the complete period.",
    "Domestic/import/source percentages use realised Harvest_tot only and exclude clearing credits. They are ratios of summed harvest, never means of annual percentages. Country sourcing may violate intended W/V permissions in the legacy pooled branch; inspect QA.",
    "When frozen input snapshots are present, current component mappings and parameters must match them byte-for-byte before the archived metadata is used.",
    "Unmet demand = demand target - model clearing credit - realised attributed harvest. Values are not silently clamped.",
    "W+V rows are calculated from summed W and V masses. They duplicate the channel detail for convenience and must not be added to W/V rows again.",
    "MC p025/p975 are empirical quantiles across available realizations, not confidence limits; with three MC runs they are descriptive only.",
    "Whole-run source/output equivalence is established separately by the Dinamica comparison suite; this script does not certify it."),
    file.path(cfg$output,filenames[7]))
  cat(sprintf("Saved runtime sourcing tables: %s\n",normalizePath(cfg$output,winslash="/")))
  invisible(list(annual=annual,period=period,qa=qa))
}

if(sys.nframe()==0L) rs_main()
