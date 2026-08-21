## ============================================================================
##  generate_modern_report_v7.R
##  Builds the modern MoFuSS summary report (pdfLaTeX) from current MoFuSS
##  outputs. Auto-detects available data and figures; designed to be sourced
##  from maps_animations_v7.R, replacing the old LaTeX compile block.
##
##  Usage:
##     source(file.path(latex_dir, "generate_modern_report_v7.R"))
##     generate_modern_report(base_dir = rootdir)        # rootdir = run folder
##
##  Optional template overrides (in <latex_dir>): mofuss_report.tex,
##  title_page.tex. Built-in templates are used when these files are absent.
##  Produces: <base_dir>/Summary_Report/MoFuSS_Summary_Report_<code>_scenario.pdf
##            <base_dir>/Summary_Report/assets/<code>_Growth_Harvest_*.mp4
## ============================================================================

generate_modern_report <- function(base_dir,
                                    latex_dir    = file.path(base_dir, "LaTeX"),
                                    output_dir   = NULL,
                                    pdflatex     = NULL,
                                    scenario_ver = NULL,
                                    open_pdf     = FALSE,
                                    keep_build   = FALSE,
                                    mc_threshold = 30L) {

  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = TRUE)
  latex_dir <- normalizePath(latex_dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(latex_dir))
    stop("LaTeX directory not found: ", latex_dir)
  if (length(mc_threshold) != 1L || is.na(mc_threshold) || mc_threshold < 1)
    stop("mc_threshold must be one positive number.")

  ## ---- locate pdflatex (MiKTeX) ------------------------------------------
  if (is.null(pdflatex)) {
    cand <- c(
      Sys.getenv("MOFUSS_PDFLATEX"),
      Sys.which("pdflatex"),
      "C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe",
      file.path(Sys.getenv("LOCALAPPDATA"),
                "Programs/MiKTeX/miktex/bin/x64/pdflatex.exe"))
    cand <- cand[nzchar(cand) & file.exists(cand)]
    if (!length(cand)) stop("pdflatex not found; pass pdflatex = '...path...'.")
    pdflatex <- cand[1]
  }
  message("Using pdflatex: ", pdflatex)

  ## ---- paths --------------------------------------------------------------
  TT        <- file.path(base_dir, "LULCC", "TempTables")
  WIZ       <- file.path(base_dir, "LULCC", "Wizard_imgs")
  BUILD     <- file.path(latex_dir, "build_modern")
  ASSETS    <- file.path(BUILD, "assets")
  SR        <- file.path(base_dir, "Summary_Report")
  SR_ASSETS <- file.path(SR, "assets")

  if (!dir.exists(TT))
    stop("MoFuSS temporary-table directory not found: ", TT,
         "\nRun this script from a completed country simulation folder.")

  if (!is.null(output_dir)) {
    if (length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir))
      stop("output_dir must be one non-empty directory path.")
    output_path <- as.character(output_dir)
    if (!grepl("^[A-Za-z]:[/\\\\]|^/", output_path))
      output_path <- file.path(base_dir, output_path)
    if (!dir.exists(output_path)) stop("MoFuSS output directory not found: ", output_path)
    OUT_DIRS <- normalizePath(output_path, winslash = "/", mustWork = TRUE)
  } else {
    output_paths <- file.path(base_dir, c("Out", "OutBaU", "OutICS"))
    output_paths <- output_paths[dir.exists(output_paths)]
    if (!length(output_paths)) stop("No MoFuSS output directory was found.")
    OUT_DIRS <- normalizePath(output_paths, winslash = "/", mustWork = TRUE)
  }
  message("Using output directory: ", paste(OUT_DIRS, collapse = ", "))

  # build_modern is disposable staging. Recreate it so missing assets or a
  # failed LaTeX run cannot silently reuse a PDF from a previous scenario.
  if (dir.exists(BUILD)) unlink(BUILD, recursive = TRUE, force = TRUE)
  ensure_dir <- function(path) {
    if (!dir.exists(path)) {
      created <- suppressWarnings(dir.create(path, recursive = TRUE,
                                             showWarnings = FALSE))
      if (!isTRUE(created) && !dir.exists(path))
        stop("Could not create report directory: ", path)
    }
  }
  ensure_dir(ASSETS)
  ensure_dir(SR_ASSETS)

  ## ---- helpers ------------------------------------------------------------
  latex_escape <- function(s) {
    if (is.null(s) || length(s) == 0) return("")
    s <- as.character(s); s[is.na(s)] <- ""
    s <- gsub("\\\\", "\\\\textbackslash{}", s)
    s <- gsub("&", "\\\\&",  s); s <- gsub("%", "\\\\%",  s)
    s <- gsub("\\$", "\\\\$", s); s <- gsub("#", "\\\\#",  s)
    s <- gsub("_", "\\\\_",  s); s <- gsub("\\{", "\\\\{", s)
    s <- gsub("\\}", "\\\\}", s); s <- gsub("~", "\\\\textasciitilde{}", s)
    s <- gsub("\\^", "\\\\textasciicircum{}", s)
    s
  }
  read_txt <- function(path, default = "") {
    if (!file.exists(path)) return(default)
    trimws(paste(readLines(path, warn = FALSE), collapse = " "))
  }
  thousands <- function(x) {
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n)) return(latex_escape(as.character(x)))
    if (abs(n - round(n)) < 1e-9)
      formatC(round(n), format = "d", big.mark = ",")
    else
      formatC(n, format = "f", digits = 1, big.mark = ",")
  }
  png_size <- function(path) {
    con <- file(path, "rb"); on.exit(close(con))
    sig <- readBin(con, "raw", n = 24)
    if (length(sig) < 24) return(NULL)
    if (!identical(as.integer(sig[1:8]),
                   c(137L,80L,78L,71L,13L,10L,26L,10L))) return(NULL)
    w <- sum(as.integer(sig[17:20]) * c(16777216, 65536, 256, 1))
    h <- sum(as.integer(sig[21:24]) * c(16777216, 65536, 256, 1))
    c(w, h)
  }
  ## copy first file whose stem matches (case-insensitive), preferring png
  copy_first <- function(stem, dest_name, search_dirs,
                         exts = c(".png", ".jpg", ".jpeg")) {
    for (sd in search_dirs) {
      if (!dir.exists(sd)) next
      fs <- list.files(sd)
      for (ext in exts) {
        hit <- fs[tolower(tools::file_path_sans_ext(fs)) == tolower(stem) &
                  tolower(paste0(".", tools::file_ext(fs))) == ext]
        if (length(hit)) {
          copied <- file.copy(file.path(sd, hit[1]), file.path(ASSETS, dest_name),
                              overwrite = TRUE)
          if (!isTRUE(copied))
            stop("Could not stage report asset: ", file.path(sd, hit[1]))
          return(dest_name)
        }
      }
    }
    NULL
  }

  ## ---- metadata -----------------------------------------------------------
  country  <- read_txt(file.path(TT, "Country.txt"),  "the study area")
  ## ---- display country name(s) from adm0 summary (NAME_0) -----------------
  country_names <- local({
    fs <- file.path(OUT_DIRS, "webmofuss_results", "summary_adm0_fr.csv")
    fs <- fs[file.exists(fs)]
    if (!length(fs)) return("")
    f <- fs[1]
    first <- readLines(f, n = 1, warn = FALSE)
    sep <- if (grepl(";", first) && !grepl(",", first)) ";" else ","
    d <- tryCatch(read.delim(f, sep = sep, stringsAsFactors = FALSE,
                             check.names = FALSE, fileEncoding = "UTF-8-BOM"),
                  error = function(e) NULL)
    if (is.null(d)) return("")
    col <- grep("^NAME_0$", names(d), value = TRUE)[1]
    if (is.na(col)) return("")
    v <- trimws(as.character(d[[col]]))
    v <- unique(v[!is.na(v) & nzchar(v)])
    if (length(v) == 0) return("")
    if (any(grepl("GoogleEarthPoly", v, ignore.case = TRUE)))
      return("a user-defined area of interest")
    if (length(v) == 1) return(v[1])
    paste(paste(v[-length(v)], collapse = ", "), v[length(v)], sep = " and ")
  })
  country_disp <- if (nzchar(country_names)) country_names else country
  scecode  <- read_txt(file.path(TT, "SceCode.txt"),  "scenario")
  userdata <- read_txt(file.path(TT, "UserData.txt"), "the user")

  ## ---- scenario (BaU1..BaU3 / ICS1..ICS3) --------------------------------
  ## Priority: explicit arg -> parameters table (Var=="scenario_ver") ->
  ##           scenario_ver.txt -> SceCode.txt. Maps e.g. "BaU1_v2" to
  ##           code "BaU1" and label "Business as Usual #1".
  read_scenver_from_params <- function() {
    cc <- file.path(TT, "Country.csv")
    if (!file.exists(cc)) return(NULL)
    cdf <- tryCatch(read.csv(cc, stringsAsFactors = FALSE, check.names = FALSE,
                             fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
    if (is.null(cdf) || !nrow(cdf)) return(NULL)
    kcol <- grep("^Key",     names(cdf), value = TRUE)[1]
    ccol <- grep("^Country", names(cdf), value = TRUE)[1]
    if (is.na(kcol) || is.na(ccol)) return(NULL)
    cname <- cdf[[ccol]][which(as.character(cdf[[kcol]]) == "1")[1]]
    if (is.na(cname)) return(NULL)
    pdir <- file.path(base_dir, "LULCC", "DownloadedDatasets",
                      paste0("SourceData", cname))
    pf <- list.files(pdir, pattern = "^parameters.*\\.csv$", full.names = TRUE)
    if (!length(pf)) return(NULL)
    first <- readLines(pf[1], n = 1, warn = FALSE)
    sep <- if (grepl(";", first)) ";" else ","
    pdf <- tryCatch(read.delim(pf[1], sep = sep, stringsAsFactors = FALSE,
                               check.names = FALSE), error = function(e) NULL)
    if (is.null(pdf)) return(NULL)
    vcol <- grep("^Var$",    names(pdf), value = TRUE)[1]
    pcol <- grep("^ParCHR$", names(pdf), value = TRUE)[1]
    if (is.na(vcol) || is.na(pcol)) return(NULL)
    v <- pdf[[pcol]][which(pdf[[vcol]] == "scenario_ver")[1]]
    if (length(v) && !is.na(v) && nzchar(v)) as.character(v) else NULL
  }
  if (is.null(scenario_ver) || !nzchar(scenario_ver))
    scenario_ver <- read_scenver_from_params()
  if (is.null(scenario_ver) || !nzchar(scenario_ver)) {
    sf <- file.path(TT, "scenario_ver.txt")
    if (file.exists(sf)) {
      raw <- paste(readLines(sf, warn = FALSE), collapse = " ")
      m <- regmatches(raw, regexpr("(BaU|ICS)[0-9]+", raw, ignore.case = TRUE))
      if (length(m) && nzchar(m)) scenario_ver <- m
    }
  }
  sce_code <- NA_character_; sce_label <- NA_character_
  if (!is.null(scenario_ver) && nzchar(scenario_ver)) {
    m <- regmatches(scenario_ver,
                    regexpr("(BaU|ICS)[0-9]+", scenario_ver, ignore.case = TRUE))
    if (length(m) && nzchar(m)) {
      typ <- toupper(sub("[0-9].*$", "", m))
      num <- sub("^[A-Za-z]+", "", m)
      sce_code  <- paste0(if (typ == "ICS") "ICS" else "BaU", num)
      sce_label <- paste0(if (typ == "ICS") "Improved Cookstoves"
                          else "Business as Usual", " #", num)
    }
  }
  if (is.na(sce_code)) {                       # fallback to SceCode.txt
    base_tok  <- sub("_.*", "", scecode)
    sce_code  <- if (nzchar(base_tok)) base_tok else "scenario"
    sce_label <- if (grepl("ICS", base_tok, ignore.case = TRUE))
                   "Improved Cookstoves" else "Business as Usual"
  }
  sce_file_code <- gsub("[^A-Za-z0-9._-]+", "_", sce_code)
  sce_file_code <- gsub("^_+|_+$", "", sce_file_code)
  if (!nzchar(sce_file_code)) sce_file_code <- "scenario"
  scenario_display <- if (is.null(scenario_ver) || !nzchar(scenario_ver))
    "<not explicitly set>" else scenario_ver
  message("Scenario: ", scenario_display, "  ->  code=", sce_code,
          "  label=", sce_label)
  ## ---- credit line from country_parameters (nameuser/ads/ads_ctry) -------
  read_param_value <- function(var, default = "") {
    cc <- file.path(TT, "Country.csv"); cname <- NA_character_
    if (file.exists(cc)) {
      cdf <- tryCatch(read.csv(cc, stringsAsFactors = FALSE, check.names = FALSE,
                               fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
      if (!is.null(cdf) && nrow(cdf)) {
        kcol <- grep("^Key", names(cdf), value = TRUE)[1]
        ccol <- grep("^Country", names(cdf), value = TRUE)[1]
        if (!is.na(kcol) && !is.na(ccol))
          cname <- cdf[[ccol]][which(as.character(cdf[[kcol]]) == "1")[1]]
      }
    }
    dd <- file.path(base_dir, "LULCC", "DownloadedDatasets"); pf <- character(0)
    if (!is.na(cname)) {
      pdir <- file.path(dd, paste0("SourceData", cname))
      if (dir.exists(pdir))
        pf <- list.files(pdir, pattern = "^parameters.*\\.csv$", full.names = TRUE)
    }
    if (!length(pf) && dir.exists(dd))
      pf <- list.files(dd, pattern = "^parameters.*\\.csv$", full.names = TRUE,
                       recursive = TRUE)
    if (!length(pf)) return(default)
    first <- readLines(pf[1], n = 1, warn = FALSE)
    sep <- if (grepl(";", first)) ";" else ","
    pdf <- tryCatch(read.delim(pf[1], sep = sep, stringsAsFactors = FALSE,
                               check.names = FALSE, fileEncoding = "UTF-8-BOM"),
                    error = function(e) NULL)
    if (is.null(pdf)) return(default)
    vcol <- grep("^Var$", names(pdf), value = TRUE)[1]
    pcol <- grep("^ParCHR$", names(pdf), value = TRUE)[1]
    if (is.na(vcol) || is.na(pcol)) return(default)
    hit <- which(pdf[[vcol]] == var)
    if (!length(hit)) return(default)
    val <- as.character(pdf[[pcol]][hit[1]])
    if (is.na(val) || !nzchar(trimws(val))) default else trimws(val)
  }
  name_user <- read_param_value("nameuser", "")
  ads_aff   <- read_param_value("ads", "")
  ads_ctry  <- read_param_value("ads_ctry", "")
  run_parts <- character(0)
  if (nzchar(name_user)) run_parts <- c(run_parts, paste0("run by ", name_user))
  if (nzchar(ads_aff))   run_parts <- c(run_parts, paste0("from ", ads_aff))
  if (nzchar(ads_ctry))  run_parts <- c(run_parts, paste0("in ", ads_ctry))
  run_by <- if (length(run_parts)) paste(run_parts, collapse = ", ") else paste0("run by ", if (nzchar(userdata)) sub("[,[:space:]]+$", "", userdata) else "the user")
  message("Credit line: ", run_by)

  ## ---- input parameters ---------------------------------------------------
  input_para <- file.path(TT, "InputPara.csv")
  if (!file.exists(input_para))
    stop("Required report input is missing: ", input_para)
  ip <- tryCatch(
    read.csv(input_para, check.names = FALSE, stringsAsFactors = FALSE,
             fileEncoding = "UTF-8-BOM"),
    error = function(e) stop("Could not read ", input_para, ": ",
                             conditionMessage(e), call. = FALSE))
  if (ncol(ip) < 2L)
    stop("InputPara.csv must contain at least a Parameter and a Value column.")
  hdr <- colnames(ip)
  params <- ip[nzchar(trimws(ip[[1]])), , drop = FALSE]
  getp <- function(key) {
    i <- which(params[[1]] == key)
    if (length(i)) as.character(params[[2]][i[1]]) else ""
  }
  start_year <- getp("StartUp year")
  sim_len    <- getp("Simulation Length (SL)")
  mc_runs    <- getp("Number of MC realizations")
  resolution <- getp("Spatial resolution")
  sce_type   <- getp("Type of scenario")
  parse_first_number <- function(x) {
    hit <- regmatches(as.character(x), regexpr("[0-9]+", as.character(x)))
    if (!length(hit) || !nzchar(hit)) return(NA_real_)
    suppressWarnings(as.numeric(hit))
  }
  mc_n <- parse_first_number(mc_runs)

  ## ---- summary table (auto-detect) ---------------------------------------
  cands <- c("summary_adm0_fr.csv", "summary_adm0_frcompl.csv",
             "summary_ecoregions_fr.csv")
  sdirs <- c(file.path(OUT_DIRS, "webmofuss_results"), TT)
  summ <- NA
  for (sd in sdirs) {
    hit <- cands[file.exists(file.path(sd, cands))]
    if (length(hit)) { summ <- file.path(sd, hit[1]); break }
  }
  nrb_rows <- list(); unit_name <- ""
  if (!is.na(summ)) {
    d <- read.csv(summ, check.names = FALSE,
                  stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
    if (nrow(d)) {
      cn <- colnames(d)
      if ("MC_n" %in% cn) {
        summary_mc_n <- suppressWarnings(as.numeric(d[["MC_n"]]))
        summary_mc_n <- summary_mc_n[is.finite(summary_mc_n)]
        if (length(summary_mc_n)) mc_n <- min(summary_mc_n)
      }
      for (c0 in c("NAME_0", "ECO_NAME", "NAME"))
        if (c0 %in% cn) { unit_name <- as.character(d[[c0]][1]); break }
      pat <- "^(NRB|Harv|fNRB)_([0-9]{4})_([0-9]{4})_(mean|sd|se)$"
      stat_cols <- cn[grepl(pat, cn)]
      per <- list()
      for (col in stat_cols) {
        mm <- regmatches(col, regexec(pat, col))[[1]]
        metric <- mm[2]; key <- paste(mm[3], mm[4], sep = "_"); stat <- mm[5]
        if (is.null(per[[key]])) per[[key]] <- list()
        values <- suppressWarnings(as.numeric(d[[col]]))
        values <- values[is.finite(values)]
        # Means are additive across units. Already-summarized SD/SE values are
        # used only when the source has one row; otherwise covariance between
        # units would be required for a correct aggregate uncertainty.
        value <- if (!length(values)) {
          NA_real_
        } else if (stat == "mean") {
          sum(values)
        } else if (length(values) == 1L) {
          values[1]
        } else {
          NA_real_
        }
        per[[key]][[paste(metric, stat, sep = "_")]] <- value
      }
      if (length(per)) {
        starts <- as.integer(sub("_.*", "", names(per)))
        ends   <- as.integer(sub(".*_", "", names(per)))
        full_key <- paste(min(starts), max(ends), sep = "_")
        ord <- names(per)[order(starts, ends)]
        ord <- c(ord[ord != full_key], if (full_key %in% names(per)) full_key)
        for (k in ord) {
          dd <- per[[k]]; yy <- strsplit(k, "_")[[1]]
          get_stat <- function(metric, stat)
            dd[[paste(metric, stat, sep = "_")]]
          nrb_mean <- get_stat("NRB", "mean")
          harv_mean <- get_stat("Harv", "mean")
          valid_harv <- length(harv_mean) == 1L && is.finite(harv_mean) &&
            harv_mean > 0
          fnrb_mean <- if (nrow(d) > 1 && valid_harv)
            round(nrb_mean / harv_mean * 100) else get_stat("fNRB", "mean")
          table_value <- function(x) {
            if (is.null(x) || !length(x) || !is.finite(x)) return("")
            thousands(x)
          }
          nrb_rows[[length(nrb_rows) + 1]] <- list(
            period = paste0(yy[1], "\\textendash{}", yy[2]),
            nrb     = table_value(nrb_mean),
            nrb_sd  = table_value(get_stat("NRB", "sd")),
            nrb_se  = table_value(get_stat("NRB", "se")),
            harv    = table_value(harv_mean),
            harv_sd = table_value(get_stat("Harv", "sd")),
            harv_se = table_value(get_stat("Harv", "se")),
            fnrb    = table_value(fnrb_mean),
            fnrb_sd = table_value(get_stat("fNRB", "sd")),
            fnrb_se = table_value(get_stat("fNRB", "se")),
            full = identical(k, full_key))
        }
      }
    }
  }
  uncertainty_fields <- c("nrb_sd", "nrb_se", "harv_sd", "harv_se",
                          "fnrb_sd", "fnrb_se")
  uncertainty_available <- length(nrb_rows) && all(vapply(
    nrb_rows, function(row) all(nzchar(unlist(row[uncertainty_fields]))),
    logical(1)))
  show_uncertainty <- is.finite(mc_n) && mc_n >= mc_threshold &&
    uncertainty_available
  if (is.finite(mc_n) && mc_n >= mc_threshold && !uncertainty_available)
    warning("MC >= ", mc_threshold,
            ", but aggregate SD/SE columns were unavailable; Table 2 will show means only.")

  ## ---- title-page assets --------------------------------------------------
  png_dirs <- file.path(OUT_DIRS, "png")
  copy_first("Area_of_Interest", "Area_of_Interest.png", png_dirs, ".png")
  copy_first("sponsors_banner",  "sponsors_banner.png",  WIZ, ".png")
  copy_first("UNAM",             "UNAM.png",             WIZ, ".png")
  copy_first("SEI",              "SEI.png",              WIZ, ".png")
  copy_first("mofuss_366",       "mofuss_366.png",       WIZ, ".png")

  ## ---- body figures (auto-detect, size-aware) ----------------------------
  FIG_SPEC <- list(
    list("AGB_NRB_fNRB_+10",
      paste("Trajectories of aboveground biomass (AGB), non-renewable biomass",
            "(NRB), fraction of non-renewable biomass (fNRB) and total fuelwood",
            "use over the simulation period. The red line uses mean user-defined",
            "parameters; light grey lines show individual Monte Carlo",
            "realizations."), "temporal"),
    list("Map_AGB",
      paste("Spatial distribution of aboveground biomass (AGB) for the first",
            "Monte Carlo realization, over the full simulation period."),
      "spatial"),
    list("Localities_of_Interest",
      "Sampled localities of interest within the area of analysis.", "spatial"),
    list("Boxplots_+10",
      paste("Box-and-whisker plots of the Monte Carlo distribution for AGB,",
            "NRB, fNRB and total fuelwood use. The dark line marks the median,",
            "the box the inter-quartile range (IQR), whiskers the range, and",
            "circles outliers (1.5\\textendash{}3 IQR)."), "boxplot"))
  find_and_copy <- function(stem) {
    for (sd in png_dirs) {
      if (!dir.exists(sd)) next
      fs <- list.files(sd)
      for (ext in c(".png", ".jpg", ".jpeg")) {
        hit <- fs[tolower(tools::file_path_sans_ext(fs)) == tolower(stem) &
                  tolower(paste0(".", tools::file_ext(fs))) == ext]
        if (length(hit)) {
          dest <- paste0(stem, ext)
          copied <- file.copy(file.path(sd, hit[1]), file.path(ASSETS, dest),
                              overwrite = TRUE)
          if (!isTRUE(copied))
            stop("Could not stage report figure: ", file.path(sd, hit[1]))
          return(dest)
        }
      }
    }
    NULL
  }
  fig_lines <- list()
  for (spec in FIG_SPEC) {
    stem <- spec[[1]]; cap <- spec[[2]]; role <- spec[[3]]
    dest <- find_and_copy(stem)
    if (is.null(dest)) next
    sizing <- "width=\\linewidth,keepaspectratio"
    sz <- if (grepl("\\.png$", dest, ignore.case = TRUE))
            png_size(file.path(ASSETS, dest)) else NULL
    if (!is.null(sz)) {
      ar <- sz[2] / sz[1]
      if (ar >= 1.7) sizing <- "height=0.82\\textheight,keepaspectratio"
      else if (ar >= 0.85 && ar <= 1.25)
        sizing <- "width=0.86\\linewidth,keepaspectratio"
    }
    fig_lines[[length(fig_lines) + 1]] <-
      list(dest = dest, cap = cap, sizing = sizing, role = role)
  }

  ## ---- animation (optional) ----------------------------------------------
  mp4_src <- NULL
  mp4_dest <- NULL
  mp4_cands <- unlist(lapply(OUT_DIRS, function(out_dir) {
    list.files(out_dir, pattern = "^Growth_Harvest_Ani.*\\.mp4$",
               full.names = TRUE, ignore.case = TRUE)
  }), use.names = FALSE)
  mp4_cands <- mp4_cands[file.exists(mp4_cands)]
  if (length(mp4_cands)) {
    mp4_src <- mp4_cands[1]
    mp4_dest <- paste0(sce_file_code, "_", basename(mp4_src))
  }

  ## ---- write fragments ----------------------------------------------------
  wf <- function(name, text)
    writeLines(text, file.path(BUILD, name), useBytes = TRUE)

  macro <- function(n, v) sprintf("\\newcommand{\\%s}{%s\\xspace}", n, v)
  meta <- c(
    macro("mfCountry",     latex_escape(country_disp)),
    macro("mfScenario",    latex_escape(sce_label)),
    macro("mfScenarioCode",latex_escape(sce_code)),
    macro("mfScenarioRaw", latex_escape(scecode)),
    macro("mfUser",        latex_escape(if (nzchar(name_user)) name_user else userdata)),
    macro("mfAds",         latex_escape(ads_aff)),
    macro("mfAdsCtry",     latex_escape(ads_ctry)),
    macro("mfRunBy",       latex_escape(run_by)),
    macro("mfStartYear",   latex_escape(start_year)),
    macro("mfSimLen",      latex_escape(sim_len)),
    macro("mfMCruns",      latex_escape(mc_runs)),
    macro("mfMCthreshold", latex_escape(mc_threshold)),
    macro("mfResolution",  latex_escape(resolution)),
    macro("mfSceType",     latex_escape(sce_type)),
    macro("mfUnitName",    latex_escape(if (nzchar(country_names)) country_names else if (nzchar(unit_name)) unit_name else country)))
  wf("_meta.tex", meta)

  ## NOTE: params table uses {@{}l l@{}} (NOT >{\raggedright\arraybackslash}p{}).
  ## On some MiKTeX installs colortbl's \rowcolor breaks p-columns
  ## (\insert@pcolumn / \do@row@strut undefined). l-columns avoid this.
  h1 <- latex_escape(hdr[1]); h2 <- latex_escape(if (length(hdr) > 1) hdr[2] else "Value")
  pt <- c("\\begin{tabular}{@{}l l@{}}", "\\toprule",
          sprintf("\\rowcolor{mfgreen!12}\\textbf{%s} & \\textbf{%s}\\\\", h1, h2),
          "\\midrule")
  for (i in seq_len(nrow(params))) {
    sh <- if (i %% 2 == 0) "\\rowcolor{mfgreen!4}" else ""
    pt <- c(pt, sprintf("%s%s & %s\\\\", sh,
                        latex_escape(params[[1]][i]), latex_escape(params[[2]][i])))
  }
  pt <- c(pt, "\\bottomrule", "\\end{tabular}")
  wf("_params_table.tex", pt)

  if (length(nrb_rows)) {
    if (show_uncertainty) {
      nt <- c(
        "\\resizebox{\\linewidth}{!}{%",
        "\\begin{tabular}{@{}l r r r r r r r r r@{}}", "\\toprule",
        paste0("\\rowcolor{mfgreen!12} & ",
               "\\multicolumn{3}{c}{\\textbf{NRB (t)}} & ",
               "\\multicolumn{3}{c}{\\textbf{Total harvest (t)}} & ",
               "\\multicolumn{3}{c}{\\textbf{fNRB (\\%)}}\\\\"),
        "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}",
        paste0("\\rowcolor{mfgreen!12}\\textbf{Period} & ",
               "\\textbf{Mean} & \\textbf{SD} & \\textbf{SE} & ",
               "\\textbf{Mean} & \\textbf{SD} & \\textbf{SE} & ",
               "\\textbf{Mean} & \\textbf{SD} & \\textbf{SE}\\\\"),
        "\\midrule")
      for (i in seq_along(nrb_rows)) {
        r <- nrb_rows[[i]]
        values <- unlist(r[c("period", "nrb", "nrb_sd", "nrb_se",
                             "harv", "harv_sd", "harv_se",
                             "fnrb", "fnrb_sd", "fnrb_se")])
        if (isTRUE(r$full)) {
          values <- paste0("\\textbf{", values, "}")
          if (i > 1L) nt <- c(nt, "\\midrule")
          nt <- c(nt, paste0("\\rowcolor{mfgreen!18}",
                             paste(values, collapse = " & "), "\\\\"))
        } else {
          sh <- if (i %% 2 == 0) "\\rowcolor{mfgreen!4}" else ""
          nt <- c(nt, paste0(sh, paste(values, collapse = " & "), "\\\\"))
        }
      }
      nt <- c(nt, "\\bottomrule", "\\end{tabular}%", "}")
    } else {
      nt <- c("\\begin{tabular}{@{}l r r r@{}}", "\\toprule",
              "\\rowcolor{mfgreen!12}\\textbf{Period} & \\textbf{NRB (t)} & \\textbf{Total harvest (t)} & \\textbf{fNRB (\\%)}\\\\",
              "\\midrule")
      for (i in seq_along(nrb_rows)) {
        r <- nrb_rows[[i]]
        if (isTRUE(r$full)) {
          if (i > 1L) nt <- c(nt, "\\midrule")
          nt <- c(nt,
                  sprintf("\\rowcolor{mfgreen!18}\\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s}\\\\",
                          r$period, r$nrb, r$harv, r$fnrb))
        } else {
          sh <- if (i %% 2 == 0) "\\rowcolor{mfgreen!4}" else ""
          nt <- c(nt, sprintf("%s%s & %s & %s & %s\\\\", sh, r$period, r$nrb, r$harv, r$fnrb))
        }
      }
      nt <- c(nt, "\\bottomrule", "\\end{tabular}")
    }
    wf("_nrb_table.tex", nt)
  } else {
    wf("_nrb_table.tex", "\\emph{No summary table was found for this run.}")
  }

  role_title <- c(temporal = "Temporal outcomes", spatial = "Spatial outcomes",
                  boxplot = "Distributional outcomes (boxplots)")
  fo <- character(0)
  for (role in c("temporal", "spatial", "boxplot")) {
    grp <- Filter(function(x) x$role == role, fig_lines)
    if (!length(grp)) next
    fo <- c(fo, "\\clearpage", sprintf("\\section{%s}", role_title[[role]]))
    for (g in grp) {
      fo <- c(fo, "\\begin{figure}[H]\\centering",
              sprintf("  \\includegraphics[%s]{assets/%s}", g$sizing, g$dest),
              sprintf("  \\caption{%s}", g$cap),
              "\\end{figure}", "\\FloatBarrier")
    }
  }
  wf("_figures.tex", fo)

  if (!is.null(mp4_dest)) {
    anim <- c("\\clearpage", "\\section{Animation of fuelwood harvest and AGB}",
      "\\begin{mfnote}{Interactive content}",
      sprintf(paste0("A spatio-temporal animation of fuelwood harvest and ",
        "aboveground biomass is available for this run: ",
        "\\href{run:assets/%s}{\\texttt{%s}}. Open this PDF in a viewer that ",
        "allows launching local files, or play the file directly from the ",
        "report folder."), mp4_dest, latex_escape(mp4_dest)),
      "\\end{mfnote}")
    wf("_animation.tex", anim)
  } else wf("_animation.tex", "")

  ## ---- master templates ---------------------------------------------------
## Keep defaults here so generate_modern_report_v7.R can be deployed on its
  ## own. A file with the same name in latex_dir remains an optional override.
  default_master <- r"(
%==============================================================================
% MoFuSS modern summary report; content is supplied by generated fragments.
%==============================================================================
\documentclass[11pt,a4paper]{article}

\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\IfFileExists{lmodern.sty}{\usepackage{lmodern}}{}
\usepackage[english]{babel}
\usepackage[protrusion=true,expansion=false]{microtype}
\usepackage{xspace}

\usepackage[table,svgnames]{xcolor}
\usepackage{graphicx}
\usepackage{booktabs}
\usepackage{array}
\usepackage{caption}
\usepackage{subcaption}
\usepackage{float}
\usepackage{placeins}
\usepackage{enumitem}
\usepackage{titlesec}
\usepackage{fancyhdr}
\usepackage{lastpage}
\usepackage{ragged2e}
\usepackage[most]{tcolorbox}
\usepackage[hidelinks]{hyperref}

\usepackage[inner=2.4cm,outer=2.0cm,top=2.4cm,bottom=2.4cm]{geometry}
\setlength{\headheight}{22pt}\addtolength{\topmargin}{-10pt}

\definecolor{mfgreen}{HTML}{1F7A5C}
\definecolor{mfdark}{HTML}{12352A}
\definecolor{mfaccent}{HTML}{C77F2A}
\definecolor{mfgrey}{HTML}{5B6770}
\definecolor{mfrule}{HTML}{D9E2DD}

\hypersetup{colorlinks=true,linkcolor=mfdark,urlcolor=mfgreen,citecolor=mfgreen}

\titleformat{\section}
  {\normalfont\sffamily\Large\bfseries\color{mfdark}}
  {\thesection}{0.6em}{}[{\color{mfrule}\titlerule[1pt]}]
\titleformat{\subsection}
  {\normalfont\sffamily\large\bfseries\color{mfgreen}}
  {\thesubsection}{0.6em}{}
\titlespacing*{\section}{0pt}{2.2ex plus 1ex minus .2ex}{1.2ex plus .2ex}

\pagestyle{fancy}
\fancyhf{}
\renewcommand{\headrulewidth}{0.4pt}
\renewcommand{\footrulewidth}{0pt}
\renewcommand{\headrule}{\hbox to\headwidth{\color{mfrule}\leaders\hrule height \headrulewidth\hfill}}
\fancyhead[L]{\footnotesize\sffamily\color{mfgrey}MoFuSS Summary Report}
\fancyhead[R]{\footnotesize\sffamily\color{mfgrey}\nouppercase{\leftmark}}
\fancyfoot[C]{\footnotesize\sffamily\color{mfgrey}\thepage\ / \pageref*{LastPage}}
\renewcommand{\sectionmark}[1]{\markboth{#1}{}}

\captionsetup{font={small},labelfont={sf,bf,color=mfdark},
  labelsep=period,justification=justified,singlelinecheck=false,skip=6pt}

\newtcolorbox{mfnote}[1]{enhanced,breakable,
  colback=mfgreen!5,colframe=mfgreen!55,boxrule=0.4pt,arc=2pt,
  left=8pt,right=8pt,top=6pt,bottom=6pt,
  fonttitle=\sffamily\bfseries\color{mfdark},title={#1},
  attach title to upper=\quad}

\newcommand{\HRule}{{\color{mfrule}\rule{\linewidth}{1pt}}}
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.6ex}

\input{_meta.tex}

\begin{document}
\input{title_page.tex}

\clearpage
\pagenumbering{roman}
\renewcommand{\contentsname}{\sffamily\color{mfdark}Contents}
\tableofcontents
\clearpage
\pagenumbering{arabic}

\section{About this report}
MoFuSS (Modeling Fuelwood Savings Scenarios) is a dynamic, spatially explicit
model that simulates the effect of fuelwood harvesting on vegetation and
estimates non-renewable woody biomass (NRB) at landscape level. This report
summarizes the main outputs of a MoFuSS run for the \textbf{\mfScenario} over
\textbf{\mfUnitName}, at a spatial resolution of \textbf{\mfResolution},
starting in \textbf{\mfStartYear} and spanning \textbf{\mfSimLen}, using
\textbf{\mfMCruns} of the Monte Carlo (MC) module.

\begin{mfnote}{What you will find here}
A summary of the user-defined parameters, headline NRB / fNRB / harvest figures
by period, and the temporal, spatial and distributional outputs generated for
this run. Only outputs available for this run are included.
\end{mfnote}

\section{Input parameters set by the user}
The table below lists the basic parameters set through the MoFuSS Wizard for
this run. It does not include the full set of built-in options.

\begin{table}[H]\centering
\caption{Input parameters set by the user for the \mfScenario.}
\input{_params_table.tex}
\end{table}

\begin{description}[leftmargin=1.2em,style=nextline,font=\sffamily\bfseries\color{mfdark}]
  \item[Annual fuelwood savings] Fraction of yearly fuelwood use saved in the
        following year, e.g.\ due to a cookstove or fuel-substitution project.
  \item[Number of MC realizations] Number of homologous simulations run under
        randomly varying parameters to account for uncertainty and sensitivity.
  \item[Accounting for fuelwood from deforestation] Whether forest loss/gain
        events interacting with fuelwood supply and demand were simulated.
\end{description}

\clearpage
\section{NRB, fNRB and harvest summary}
The headline results below summarize non-renewable biomass (NRB), total
fuelwood harvest, and the fraction of non-renewable biomass (fNRB) for
\textbf{\mfUnitName}, broken down by decade and for the full simulation period.

\begin{table}[H]\centering
\caption{Summary outputs for the \mfScenario over \mfUnitName.}
\input{_nrb_table.tex}
\end{table}

\begin{mfnote}{Reading these figures}
\textbf{NRB} and \textbf{harvest} are expressed in metric tonnes (t) accumulated
over each period; \textbf{fNRB} is the fraction of harvested fuelwood that is
non-renewable, expressed as a percentage. Values are means across
\textbf{\mfMCruns} of the Monte Carlo module. When at least
\textbf{\mfMCthreshold} realizations are available, the table also reports the
absolute standard deviation (SD) and standard error
($\mathrm{SE}=\mathrm{SD}/\sqrt{n}$); SD and SE are omitted below that threshold.
For each realization, fNRB is calculated as NRB divided by total harvest, and
its mean, SD and SE are summarized directly from those realization-level ratios.
\end{mfnote}

\input{_figures.tex}
\input{_animation.tex}

\section{How to use this report}
These outputs help identify communities with the highest fuelwood use lying
within or near high-NRB areas. Selecting key localities that contribute most to
NRB can be done manually or via an optimization procedure that maximizes NRB
reductions through targeted intervention in space and time.

\vfill
\begin{center}\footnotesize\sffamily\color{mfgrey}
Generated automatically by MoFuSS on \today\ \textemdash{} \mfScenario, \mfUnitName.
\end{center}

\end{document}
)"

  default_title <- r"(
\begin{titlepage}
\thispagestyle{empty}
\centering

\IfFileExists{assets/mofuss_366.png}{%
  \includegraphics[width=0.40\linewidth,keepaspectratio]{assets/mofuss_366.png}
}{}

\vspace{0.9cm}

{\sffamily\bfseries\color{mfdark}\Huge Summary Report}\\[0.35cm]
{\sffamily\large\color{mfgrey}\mfScenario}

\vspace{0.5cm}

\begin{minipage}{0.88\linewidth}
{\small This is an automated report generated by
\href{https://www.mofuss.unam.mx}{MoFuSS}. It summarizes the main results of the
model for \mfUnitName. MoFuSS was \mfRunBy\ on \today.}
\end{minipage}

\vspace{0.5cm}
\HRule
\vspace{0.5cm}

\IfFileExists{assets/Area_of_Interest.png}{%
  \includegraphics[width=0.9\linewidth,height=0.50\textheight,keepaspectratio]{assets/Area_of_Interest.png}
}{}

\vfill

\IfFileExists{assets/sponsors_banner.png}{%
  {\footnotesize\sffamily\color{mfgrey}Project funded by:}\\[4pt]
  \includegraphics[width=0.62\linewidth,keepaspectratio]{assets/sponsors_banner.png}
}{}

\end{titlepage}

\thispagestyle{empty}
\begin{flushleft}\small
A.~Ghilardi, R.~Bailis. \textbf{Summary Report for \mfCountry}
\textemdash{} Spatiotemporal modeling of fuelwood environmental impacts.
\the\year. CIGA-UNAM and SEI-US.\par
\vspace{0.8em}

Centro de Investigaciones en Geograf\'ia Ambiental,
Universidad Nacional Aut\'onoma de M\'exico.
Antigua carretera a P\'atzcuaro 8701, Col.\ Exhacienda de San Jos\'e de la
Huerta, Morelia, Michoac\'an, C.P.\ 58190, Mexico.
\href{http://www.ciga.unam.mx}{www.ciga.unam.mx}\par
\vspace{0.6em}

Stockholm Environment Institute -- US Centre.
11 Curtis Ave, Somerville, MA 02144, United States.
\href{http://www.sei-us.org}{www.sei-us.org}\par
\vspace{0.6em}

Author contact: Adrian Ghilardi,
\href{mailto:aghilardi@ciga.unam.mx}{aghilardi@ciga.unam.mx}.\par
\vspace{1.2em}

This publication may be reproduced in whole or in part and in any form for
educational or non-profit purposes, without special permission from the
copyright holder(s) provided acknowledgement of the source is made. No use of
this publication may be made for resale or other commercial purpose without
written permission of the copyright holder(s).\par
\vspace{0.8em}

Copyright \textcopyright\ \today\enspace by
Universidad Nacional Aut\'onoma de M\'exico and Stockholm Environment
Institute -- US Centre.\par
\vspace{0.8em}

\IfFileExists{assets/UNAM.png}{\includegraphics[height=1.6cm,keepaspectratio]{assets/UNAM.png}\hspace{1.2cm}}{}%
\IfFileExists{assets/SEI.png}{\raisebox{0.2cm}{\includegraphics[height=1.1cm,keepaspectratio]{assets/SEI.png}}}{}
\end{flushleft}
)"

  template_defaults <- list(
    "mofuss_report.tex" = default_master,
    "title_page.tex" = default_title)
  for (tf in names(template_defaults)) {
    src <- file.path(latex_dir, tf)
    dest <- file.path(BUILD, tf)
    if (file.exists(src)) {
      if (!isTRUE(file.copy(src, dest, overwrite = TRUE)))
        stop("Could not copy report template: ", src)
    } else {
      writeLines(template_defaults[[tf]], dest, useBytes = TRUE)
    }
    if (!file.exists(dest)) stop("Could not create report template: ", dest)
  }

  ## ---- compile ------------------------------------------------------------
  oldwd <- getwd(); setwd(BUILD); on.exit(setwd(oldwd), add = TRUE)
  latex_args <- c("-interaction=nonstopmode", "-halt-on-error",
                  "-file-line-error")
  # MiKTeX can install a missing package during a non-interactive build.
  if (grepl("miktex", pdflatex, ignore.case = TRUE))
    latex_args <- c(latex_args, "--enable-installer")
  for (pass in 1:2) {
    out <- suppressWarnings(system2(
      pdflatex, args = c(latex_args, "mofuss_report.tex"),
      stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    if (is.null(status)) status <- 0L
    if (!identical(as.integer(status), 0L)) {
      excerpt <- tail(out, 40)
      stop("LaTeX compilation failed on pass ", pass, " (exit status ",
           status, ").\n", paste(excerpt, collapse = "\n"), call. = FALSE)
    }
  }
  log <- file.path(BUILD, "mofuss_report.log")
  if (file.exists(log)) {
    lg <- readLines(log, warn = FALSE)
    errs <- grep("^!", lg, value = TRUE)
    if (length(errs))
      warning("LaTeX reported errors:\n", paste(errs, collapse = "\n"))
  }

  pdf_built <- file.path(BUILD, "mofuss_report.pdf")
  if (!file.exists(pdf_built) || file.info(pdf_built)$size <= 0)
    stop("Compilation failed: no valid PDF was produced.")

  # Update deliverables only after LaTeX succeeds. Keep one scenario-specific
  # animation beside the PDF so the relative link remains valid.
  old_scenario_mp4 <- list.files(
    SR_ASSETS,
    pattern = paste0("^", sce_file_code, "_Growth_Harvest_Ani.*\\.mp4$"),
    full.names = TRUE, ignore.case = TRUE)
  if (!is.null(mp4_src)) {
    mp4_out <- file.path(SR_ASSETS, mp4_dest)
    if (!isTRUE(file.copy(mp4_src, mp4_out, overwrite = TRUE)))
      stop("Could not copy the report animation to: ", mp4_out)
    old_scenario_mp4 <- setdiff(old_scenario_mp4, mp4_out)
  }
  if (length(old_scenario_mp4)) unlink(old_scenario_mp4, force = TRUE)

  out_pdf <- file.path(
    SR, sprintf("MoFuSS_Summary_Report_%s_scenario.pdf", sce_file_code))
  if (!isTRUE(file.copy(pdf_built, out_pdf, overwrite = TRUE)))
    stop("Could not copy the compiled report to: ", out_pdf)

  if (!isTRUE(keep_build)) {
    setwd(oldwd)
    cleanup_status <- unlink(BUILD, recursive = TRUE, force = TRUE)
    if (!identical(as.integer(cleanup_status), 0L))
      warning("Report succeeded, but staging could not be removed: ", BUILD)
  }
  message("Report written: ", out_pdf)
  if (isTRUE(open_pdf)) try(utils::browseURL(out_pdf), silent = TRUE)
  invisible(out_pdf)
}
