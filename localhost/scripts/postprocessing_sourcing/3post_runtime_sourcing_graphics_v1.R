#!/usr/bin/env Rscript
# Publication figures from the completed Stage 2 country-sourcing CSVs.
# Sourceable: call sg_main(args), or invoke with Rscript. Never reads rasters.

.sg_stop <- function(...) stop(sprintf(...), call.=FALSE)
.sg_require <- function() {
  if (!requireNamespace("data.table", quietly=TRUE))
    .sg_stop("Package 'data.table' is required")
}
.sg_bool <- function(x) toupper(x) %in% c("YES", "TRUE", "1")
.sg_periods <- function(spec) {
  if (!length(spec) || any(!grepl("^[0-9]{4}:[0-9]{4}$", spec)))
    .sg_stop("Periods must be YYYY:YYYY ranges")
  ends <- strsplit(spec, ":", fixed=TRUE)
  if (any(vapply(ends, function(x) as.integer(x[[1L]])>as.integer(x[[2L]]), logical(1))))
    .sg_stop("Period start must not exceed period end")
  unique(gsub(":", "-", spec, fixed=TRUE))
}
.sg_mc <- function(spec) {
  if (identical(spec, "all")) return(NULL)
  if (length(spec)!=1L || !grepl("^[1-9][0-9]*(:[1-9][0-9]*|(?:,[1-9][0-9]*)*)$", spec, perl=TRUE))
    .sg_stop("MC runs must be all, 1:3, or comma-separated positive integers")
  x <- if (grepl(":", spec, fixed=TRUE)) {
    ends <- as.integer(strsplit(spec, ":", fixed=TRUE)[[1L]])
    if (ends[[1L]]>ends[[2L]]) .sg_stop("MC range must be ascending")
    seq.int(ends[[1L]], ends[[2L]])
  } else as.integer(strsplit(spec, ",", fixed=TRUE)[[1L]])
  unique(x)
}
.sg_near <- function(x, y) abs(x-y)<=pmax(1e-5, 2e-6*pmax(abs(x),abs(y)))
.sg_role <- function(x) {
  y <- tolower(x)
  ifelse(grepl("bau1", y), "BAU1", ifelse(grepl("ics3", y), "ICS3", NA_character_))
}
.sg_inputs <- function(input_dir, periods, selected_mc) {
  .sg_require()
  names <- c("period_origin_balance.csv", "period_sourcing_matrix.csv", "runtime_sourcing_qa.csv")
  paths <- file.path(input_dir, names)
  if (any(!file.exists(paths))) .sg_stop("Missing Stage 2 sourcing input: %s",paths[which(!file.exists(paths))[[1L]]])
  b <- data.table::fread(paths[[1L]],showProgress=FALSE)
  m <- data.table::fread(paths[[2L]],showProgress=FALSE)
  q <- data.table::fread(paths[[3L]],showProgress=FALSE)
  balance_cols <- c("run_name","regrowth","mc_run","period","origin_iso3","channel",
                    "demand_tonnes","realised_harvest_tonnes","domestic_harvest_tonnes",
                    "imported_harvest_tonnes","clearing_credit_tonnes","unmet_demand_tonnes",
                    "signed_adjustments_present")
  matrix_cols <- c("run_name","mc_run","period","origin_iso3","source_iso3","channel",
                   "realised_harvest_tonnes","allowed_bilateral_source")
  qa_cols <- c("run_name","mc_run","year","forbidden_direct_V_realised_tonnes")
  if (!all(balance_cols %in% names(b)) || !all(matrix_cols %in% names(m)) ||
      !all(qa_cols %in% names(q))) .sg_stop("Stage 2 CSV columns do not match the recorded sourcing contract")
  b <- b[channel=="V" & period %in% periods]
  m <- m[channel=="V" & period %in% periods]
  if (!is.null(selected_mc)) {
    b <- b[mc_run %in% selected_mc]
    m <- m[mc_run %in% selected_mc]
    q <- q[mc_run %in% selected_mc]
  }
  if (!nrow(b) || !nrow(m)) .sg_stop("No V sourcing rows match the requested periods and MC runs")
  if (!setequal(unique(b$period), periods) || !setequal(unique(m$period), periods))
    .sg_stop("The requested periods are not all present in both Stage 2 CSVs")
  if (!is.null(selected_mc) && (!setequal(unique(b$mc_run), selected_mc) ||
                                !setequal(unique(m$mc_run), selected_mc)))
    .sg_stop("The requested MC runs are not all present in both Stage 2 CSVs")
  keys <- c("run_name","period","mc_run","origin_iso3")
  if (anyDuplicated(b[,..keys])) .sg_stop("Duplicate V country balance rows")
  if (anyNA(b[,c(keys,"demand_tonnes","realised_harvest_tonnes"),with=FALSE]) ||
      anyNA(m[,c(keys,"source_iso3","realised_harvest_tonnes"),with=FALSE]))
    .sg_stop("Missing V sourcing identifiers or masses")
  if (any(b$signed_adjustments_present) || any(b$demand_tonnes<0) ||
      any(b$realised_harvest_tonnes < -1e-5) || any(m$realised_harvest_tonnes < -1e-5))
    .sg_stop("Signed or negative V accounting cannot be plotted as physical sourcing shares")
  if (any(q$forbidden_direct_V_realised_tonnes>1e-5,na.rm=TRUE))
    .sg_stop("Stage 2 QA contains V harvest outside the allowed bilateral sources")
  if (any(m[realised_harvest_tonnes>1e-5 &
            (is.na(allowed_bilateral_source) | !allowed_bilateral_source)]$realised_harvest_tonnes>1e-5))
    .sg_stop("V matrix has harvest from a disallowed source")
  roles <- .sg_role(b$run_name)
  if (anyNA(roles)) .sg_stop("Cannot identify BAU1/ICS3 scenario from run_name")
  b[,scenario:=roles]
  metadata <- unique(b[,.(run_name,regrowth,scenario)])
  if (anyDuplicated(metadata[,.(regrowth,scenario)]) ||
      anyDuplicated(metadata$run_name) ||
      !setequal(unique(metadata$scenario),c("BAU1","ICS3")))
    .sg_stop("Expected one BAU1 and one ICS3 run for each regrowth setting")
  if (any(metadata[, .N, by=regrowth]$N!=2L))
    .sg_stop("Every regrowth setting must have both BAU1 and ICS3")
  counts <- b[,.(mc_count=data.table::uniqueN(mc_run)),by=.(run_name,period,origin_iso3)]
  expected_mc <- if (is.null(selected_mc)) data.table::uniqueN(b$mc_run) else length(selected_mc)
  if (any(counts$mc_count!=expected_mc) ||
      any(b[,.(n=.N),by=.(run_name,period,mc_run)]$n!=data.table::uniqueN(b$origin_iso3)))
    .sg_stop("The V balance is missing a country or MC for at least one run/period")
  mm <- m[,.(source_tonnes=sum(realised_harvest_tonnes)),
          by=c(keys,"source_iso3")]
  totals <- mm[,.(source_total=sum(source_tonnes),
                   domestic_total=sum(source_tonnes[source_iso3==origin_iso3])),by=keys]
  audit <- merge(b,totals,by=keys,all.x=TRUE,sort=FALSE)
  if (nrow(audit)!=nrow(b) || anyNA(audit$source_total) ||
      any(!.sg_near(audit$source_total,audit$realised_harvest_tonnes)) ||
      any(!.sg_near(audit$domestic_total,audit$domestic_harvest_tonnes)) ||
      any(!.sg_near(audit$source_total-audit$domestic_total,audit$imported_harvest_tonnes)))
    .sg_stop("Country-to-country V matrix does not reconcile with the origin balance")
  if (any(audit$demand_tonnes==0 & audit$source_total>1e-5))
    .sg_stop("Positive V sourcing has zero demand")
  means <- b[,.(demand_tonnes=mean(demand_tonnes),
                domestic_tonnes=mean(domestic_harvest_tonnes),
                imported_tonnes=mean(imported_harvest_tonnes),
                credit_tonnes=mean(clearing_credit_tonnes),
                unmet_tonnes=mean(unmet_demand_tonnes),
                realised_tonnes=mean(realised_harvest_tonnes),mc_count=data.table::uniqueN(mc_run)),
             by=.(run_name,regrowth,scenario,period,origin_iso3)]
  source_means <- mm[,.(source_tonnes=mean(source_tonnes)),
                     by=.(run_name,period,origin_iso3,source_iso3)]
  means <- merge(means,source_means,
                 by=c("run_name","period","origin_iso3"),allow.cartesian=TRUE)
  means[,`:=`(source_share_demand_pct=ifelse(demand_tonnes>0,100*source_tonnes/demand_tonnes,NA_real_),
              domestic_share_demand_pct=ifelse(demand_tonnes>0,100*domestic_tonnes/demand_tonnes,NA_real_),
              imported_share_demand_pct=ifelse(demand_tonnes>0,100*imported_tonnes/demand_tonnes,NA_real_),
              unmet_share_demand_pct=ifelse(demand_tonnes>0,100*unmet_tonnes/demand_tonnes,NA_real_))]
  if (any(means$source_share_demand_pct > 100+1e-4,na.rm=TRUE) ||
      any(means$domestic_share_demand_pct+means$imported_share_demand_pct>100+1e-3,na.rm=TRUE))
    .sg_stop("A V sourcing share materially exceeds 100%% of demand")
  list(plot_data=means,metadata=metadata,origins=sort(unique(b$origin_iso3)),
       periods=periods,mc_count=expected_mc)
}
.sg_import_label <- function(pct, tonnes) {
  pct_text <- if (!is.finite(pct)) "n.a." else if (pct>0 && pct<0.1) "<0.1%" else sprintf("%.1f%%",pct)
  amount <- if (tonnes>=1e6) sprintf("%.2f Mt",tonnes/1e6) else
    if (tonnes>=1e3) sprintf("%.0f kt",tonnes/1e3) else sprintf("%.0f t",tonnes)
  paste0(pct_text,"  (",amount,")")
}
.sg_panel_bars <- function(d, origins, scenario) {
  n <- length(origins)
  graphics::par(mar=c(2.8,4.6,2.2,0.4),xaxs="i",yaxs="i")
  graphics::plot.new()
  graphics::plot.window(xlim=c(0,168),ylim=c(0.4,n+0.6))
  ypos <- setNames(rev(seq_len(n)),origins)
  for (i in seq_along(origins)) {
    origin <- origins[[i]]; y <- ypos[[origin]]
    row <- d[origin_iso3==origin][1L]
    if (nrow(row)!=1L) .sg_stop("Missing plot row for %s / %s",scenario,origin)
    graphics::rect(0,y-0.5,100,y+0.5,col=if(i%%2L)"#FFFFFF" else "#F8F9F9",border=NA)
    graphics::rect(0,y-0.28,100,y+0.28,col="#FFF1CF",border="#E2E5E6",lwd=0.5)
    if (row$demand_tonnes>0) {
      domestic <- max(0,min(100,100*row$domestic_tonnes/row$demand_tonnes))
      imported <- max(0,min(100-domestic,100*row$imported_tonnes/row$demand_tonnes))
      credit <- max(0,min(100-domestic-imported,100*row$credit_tonnes/row$demand_tonnes))
      graphics::rect(0,y-0.28,domestic,y+0.28,col="#B9C5CA",border=NA)
      graphics::rect(domestic,y-0.28,domestic+imported,y+0.28,col="#147D9D",border=NA)
      if (credit>0) graphics::rect(domestic+imported,y-0.28,domestic+imported+credit,y+0.28,
                                  col="#A97554",border=NA)
      graphics::text(103,y,.sg_import_label(row$imported_share_demand_pct,row$imported_tonnes),
                     adj=c(0,0.5),cex=0.74,col="#184052")
    } else graphics::text(103,y,"No V demand",adj=c(0,0.5),cex=0.73,col="#777777")
  }
  graphics::abline(v=c(0,25,50,75,100),col="#E1E5E7",lty=3,lwd=0.7)
  graphics::axis(2,at=unname(ypos[origins]),labels=origins,las=1,tick=FALSE,cex.axis=0.83)
  graphics::axis(1,at=c(0,25,50,75,100),labels=paste0(c(0,25,50,75,100),"%"),
                 tick=FALSE,cex.axis=0.73,col.axis="#465762")
  graphics::title(main=scenario,adj=0,cex.main=1.1,font.main=2,col.main="#173949")
  graphics::mtext("Share of V demand",side=1,line=1.8,cex=0.77,col="#465762")
}
.sg_source_color <- function(pct) {
  if (!is.finite(pct) || pct<=0) return("#FFFFFF")
  if (pct<1) return("#E7F2F6")
  if (pct<5) return("#BDDDE8")
  if (pct<10) return("#82BDD0")
  if (pct<20) return("#4998B3")
  if (pct<40) return("#23738F")
  "#104E68"
}
.sg_panel_matrix <- function(d, origins) {
  n <- length(origins)
  graphics::par(mar=c(4.0,0.8,2.2,0.5),xaxs="i",yaxs="i")
  graphics::plot.new()
  graphics::plot.window(xlim=c(0.5,n+0.5),ylim=c(0.4,n+0.6))
  ypos <- setNames(rev(seq_len(n)),origins)
  xpos <- setNames(seq_len(n),origins)
  for (origin in origins) for (supplier in origins) {
    y <- ypos[[origin]]; x <- xpos[[supplier]]
    if (identical(origin,supplier)) {
      graphics::rect(x-0.49,y-0.48,x+0.49,y+0.48,col="#E7EBED",border="#FFFFFF",lwd=0.4)
      next
    }
    row <- d[origin_iso3==origin & source_iso3==supplier]
    share <- if(nrow(row))row$source_share_demand_pct[[1L]] else 0
    graphics::rect(x-0.49,y-0.48,x+0.49,y+0.48,col=.sg_source_color(share),
                   border="#FFFFFF",lwd=0.4)
    if (is.finite(share) && share>=0.5)
      graphics::text(x,y,sprintf("%.1f",share),cex=if(n>16)0.48 else 0.63,
                     col=if(share>=20)"#FFFFFF" else "#183A4A")
  }
  graphics::axis(1,at=seq_len(n),labels=origins,las=2,tick=FALSE,cex.axis=if(n>16)0.55 else 0.72)
  graphics::title(main="Supplying country",adj=0,cex.main=1.1,font.main=2,col.main="#173949")
  graphics::mtext("Source country (domestic diagonal shaded)",side=1,line=3.1,cex=0.77,col="#465762")
}
.sg_legend <- function() {
  graphics::par(mar=c(0,0,0,0),xaxs="i",yaxs="i")
  graphics::plot.new(); graphics::plot.window(xlim=c(0,1),ylim=c(0,1))
  graphics::text(0.02,0.79,"Bar: portion of V demand",adj=c(0,0.5),cex=0.84,font=2,col="#173949")
  colors <- c("#B9C5CA","#147D9D","#FFF1CF","#A97554")
  labels <- c("Domestic harvest","Imported harvest","Unmet demand","Clearing credit")
  left <- c(0.02,0.17,0.34,0.50)
  for (i in seq_along(colors)) {
    graphics::rect(left[[i]],0.48,left[[i]]+0.017,0.62,col=colors[[i]],border="#CCD3D5",lwd=0.5)
    graphics::text(left[[i]]+0.022,0.55,labels[[i]],adj=c(0,0.5),cex=0.75)
  }
  graphics::text(0.02,0.24,"Matrix: foreign source share of V demand",adj=c(0,0.5),cex=0.84,font=2,col="#173949")
  bands <- c("<1","1-5","5-10","10-20","20-40",">=40%")
  band_colors <- c("#E7F2F6","#BDDDE8","#82BDD0","#4998B3","#23738F","#104E68")
  for (i in seq_along(bands)) {
    x <- 0.39+(i-1L)*0.092
    graphics::rect(x,0.14,x+0.025,0.30,col=band_colors[[i]],border=NA)
    graphics::text(x+0.029,0.22,bands[[i]],adj=c(0,0.5),cex=0.73)
  }
}
.sg_draw <- function(d, period, regrowth, metadata, origins, mc_count) {
  selected_period <- period
  mode <- regrowth
  graphics::layout(matrix(c(1,2,3,4,5,5),nrow=3L,byrow=TRUE),
                   widths=c(0.39,0.61),heights=c(0.44,0.44,0.12))
  graphics::par(oma=c(0,0,3.2,0),family="sans",fg="#173949")
  for (scenario in c("BAU1","ICS3")) {
    role <- scenario
    run <- metadata[metadata$regrowth==mode & metadata$scenario==role]$run_name
    if(length(run)!=1L).sg_stop("Missing %s / %s scenario",scenario,regrowth)
    part <- d[d$run_name==run & d$period==selected_period]
    .sg_panel_bars(part,origins,scenario)
    .sg_panel_matrix(part,origins)
  }
  .sg_legend()
  graphics::mtext(sprintf("Model-implied V sourcing  |  %s  |  %s regrowth",period,regrowth),
                  side=3,outer=TRUE,line=1.5,cex=1.47,font=2,col="#173949")
  graphics::mtext(sprintf("Mean demand and attributed harvest across %d Monte Carlo runs; cumulative period totals",mc_count),
                  side=3,outer=TRUE,line=0.2,cex=0.87,col="#4D626C")
}
.sg_files <- function(periods, regrowth) {
  stem <- unlist(lapply(regrowth,function(mode)paste0("V_sourcing_",periods,"_",mode)),use.names=FALSE)
  c(paste0(rep(stem,each=2L),c(".pdf",".png")),"V_sourcing_plot_data.csv","README_graphics.txt")
}
sg_main <- function(args=commandArgs(trailingOnly=TRUE)) {
  cfg <- list(input=NULL,output=NULL,periods=c("2020:2030","2030:2040","2040:2050","2020:2050"),
              mc="all",overwrite=FALSE,check=FALSE)
  for (arg in args) {
    if (identical(arg,"--check")) {cfg$check<-TRUE; next}
    if (arg %in% c("--help","-h")) {
      cat("Usage: Rscript 3post_runtime_sourcing_graphics_v1.R --input-dir=PATH --output-dir=PATH\n",
          "Optional: --periods=2020:2030,2030:2040,2040:2050,2020:2050 --mc-runs=all|1:3 --overwrite=TRUE --check\n",sep="")
      return(invisible(NULL))
    }
    key <- sub("=.*$","",arg); value <- sub("^[^=]*=","",arg)
    switch(key,
      "--input-dir"={cfg$input<-value},"--output-dir"={cfg$output<-value},
      "--periods"={cfg$periods<-strsplit(value,",",fixed=TRUE)[[1L]]},
      "--mc-runs"={cfg$mc<-value},"--overwrite"={cfg$overwrite<-.sg_bool(value)},
      .sg_stop("Unknown argument: %s",arg))
  }
  if (is.null(cfg$input) || is.null(cfg$output) || !nzchar(cfg$input) || !nzchar(cfg$output))
    .sg_stop("Both --input-dir and --output-dir are required")
  periods <- .sg_periods(cfg$periods)
  selected_mc <- .sg_mc(cfg$mc)
  data <- .sg_inputs(cfg$input,periods,selected_mc)
  modes <- sort(unique(data$metadata$regrowth))
  files <- .sg_files(periods,modes)
  targets <- file.path(cfg$output,files)
  if (cfg$check) {
    cat(sprintf("CHECK OK graphics: %d countries, %d periods, %d regrowth settings, %d MC runs; inputs reconciled.\n",
                length(data$origins),length(periods),length(modes),data$mc_count))
    return(invisible(data))
  }
  if (!cfg$overwrite && any(file.exists(targets)))
    .sg_stop("Figure output already exists: %s. Use a new output folder or --overwrite=TRUE.",
             targets[which(file.exists(targets))[[1L]]])
  if (!dir.exists(cfg$output) && !dir.create(cfg$output,recursive=TRUE,showWarnings=FALSE))
    .sg_stop("Could not create graphics output directory: %s",cfg$output)
  for (mode in modes) for (period in periods) {
    stem <- paste0("V_sourcing_",period,"_",mode)
    pdf_path <- file.path(cfg$output,paste0(stem,".pdf"))
    png_path <- file.path(cfg$output,paste0(stem,".png"))
    grDevices::cairo_pdf(pdf_path,width=16.5,height=10.2,family="sans",onefile=FALSE)
    tryCatch(.sg_draw(data$plot_data,period,mode,data$metadata,data$origins,data$mc_count),
             finally=grDevices::dev.off())
    if (requireNamespace("ragg",quietly=TRUE))
      ragg::agg_png(png_path,width=16.5,height=10.2,units="in",res=300,background="white")
    else grDevices::png(png_path,width=4950,height=3060,res=300,type="cairo-png",bg="white")
    tryCatch(.sg_draw(data$plot_data,period,mode,data$metadata,data$origins,data$mc_count),
             finally=grDevices::dev.off())
    cat(sprintf("Saved %s\n",pdf_path))
  }
  data.table::fwrite(data$plot_data,file.path(cfg$output,"V_sourcing_plot_data.csv"))
  writeLines(c("MoFuSS model-implied sourcing of urban woodfuel and charcoal (V channel).",
    "One two-scenario figure per requested period and regrowth setting; vector PDF and 300-dpi PNG.",
    "Each bar divides mean country V demand into domestic harvest, imported harvest, unmet demand and any clearing credit.",
    "Bar labels give imported share of mean V demand and mean imported tonnes across the selected MC runs.",
    "The matrix shows each foreign supplier's mean attributed harvest divided by the origin's mean V demand.",
    "The shaded diagonal denotes domestic supply, which is included in the bar but omitted from the foreign-source matrix.",
    "Country and source values are model-implied proportional allocations from recorded origin pressures, not observed trade.",
    "The source matrix was reconciled to every selected MC-period country balance before drawing.",
    "Period endpoints are inclusive. Adjacent decade figures overlap at 2030 and 2040; use 2020-2050 for a whole-run total.",
    sprintf("Input: %s",normalizePath(cfg$input,winslash="/")),
    sprintf("Selected MC runs: %s",if(is.null(selected_mc))paste0("all available (",data$mc_count,")") else paste(selected_mc,collapse=","))),
    file.path(cfg$output,"README_graphics.txt"))
  cat(sprintf("SOURCING GRAPHICS COMPLETE: %s\n",normalizePath(cfg$output,winslash="/")))
  invisible(data)
}

if (sys.nframe()==0L) sg_main()
