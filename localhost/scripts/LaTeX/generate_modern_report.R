## ============================================================================
##  generate_modern_report.R
##  Builds the modern MoFuSS summary report (pdfLaTeX) from current MoFuSS
##  outputs. Auto-detects available data and figures; designed to be sourced
##  from maps_animations7.R, replacing the old LaTeX compile block.
##
##  Usage:
##     source(file.path(latex_dir, "generate_modern_report.R"))
##     generate_modern_report(base_dir = rootdir)        # rootdir = run folder
##
##  Requires (in <latex_dir>): mofuss_report.tex, title_page.tex
##  Produces:  <base_dir>/Summary_Report/Mofuss_Summary_Report_<SceCode>.pdf
## ============================================================================

generate_modern_report <- function(base_dir,
                                    latex_dir   = file.path(base_dir, "LaTeX"),
                                    pdflatex    = NULL,
                                    open_pdf    = FALSE) {

  ## ---- locate pdflatex (MiKTeX) ------------------------------------------
  if (is.null(pdflatex)) {
    cand <- c(
      Sys.getenv("MOFUSS_PDFLATEX"),
      "C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe",
      file.path(Sys.getenv("LOCALAPPDATA"),
                "Programs/MiKTeX/miktex/bin/x64/pdflatex.exe"),
      Sys.which("pdflatex"))
    cand <- cand[nzchar(cand) & file.exists(cand)]
    if (!length(cand)) stop("pdflatex not found; pass pdflatex = '...path...'.")
    pdflatex <- cand[1]
  }
  message("Using pdflatex: ", pdflatex)

  ## ---- paths --------------------------------------------------------------
  TT     <- file.path(base_dir, "LULCC", "TempTables")
  WIZ    <- file.path(base_dir, "LULCC", "Wizard_imgs")
  BUILD  <- file.path(latex_dir, "build_modern")
  ASSETS <- file.path(BUILD, "assets")
  SR     <- file.path(base_dir, "Summary_Report")
  outdirs <- c("OutBaU", "OutICS")
  outdirs <- outdirs[dir.exists(file.path(base_dir, outdirs))]
  dir.create(ASSETS, recursive = TRUE, showWarnings = FALSE)
  dir.create(SR,     recursive = TRUE, showWarnings = FALSE)

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
          file.copy(file.path(sd, hit[1]), file.path(ASSETS, dest_name),
                    overwrite = TRUE)
          return(dest_name)
        }
      }
    }
    NULL
  }

  ## ---- metadata -----------------------------------------------------------
  country  <- read_txt(file.path(TT, "Country.txt"),  "the study area")
  scecode  <- read_txt(file.path(TT, "SceCode.txt"),  "scenario")
  userdata <- read_txt(file.path(TT, "UserData.txt"), "the user")

  ## ---- input parameters ---------------------------------------------------
  ip <- read.csv(file.path(TT, "InputPara.csv"), check.names = FALSE,
                 stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  if (!ncol(ip)) ip <- data.frame(Parameter = character(), Value = character())
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

  ## ---- summary table (auto-detect) ---------------------------------------
  cands <- c("summary_adm0_fr.csv", "summary_adm0_frcompl.csv",
             "summary_ecoregions_fr.csv")
  summ <- cands[file.exists(file.path(TT, cands))]
  summ <- if (length(summ)) summ[1] else NA
  nrb_rows <- list(); unit_name <- ""
  if (!is.na(summ)) {
    d <- read.csv(file.path(TT, summ), check.names = FALSE,
                  stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
    if (nrow(d)) {
      cn <- colnames(d)
      for (c0 in c("NAME_0", "ECO_NAME", "NAME"))
        if (c0 %in% cn) { unit_name <- as.character(d[[c0]][1]); break }
      pat <- "^(NRB|Harv|fNRB)_([0-9]{4})_([0-9]{4})_mean$"
      mcols <- cn[grepl(pat, cn)]
      per <- list()
      for (col in mcols) {
        mm <- regmatches(col, regexec(pat, col))[[1]]
        metric <- mm[2]; key <- paste(mm[3], mm[4], sep = "_")
        if (is.null(per[[key]])) per[[key]] <- list()
        per[[key]][[metric]] <- d[[col]][1]
      }
      if (length(per)) {
        starts <- as.integer(sub("_.*", "", names(per)))
        ends   <- as.integer(sub(".*_", "", names(per)))
        full_key <- paste(min(starts), max(ends), sep = "_")
        ord <- names(per)[order(starts, ends)]
        ord <- c(ord[ord != full_key], if (full_key %in% names(per)) full_key)
        for (k in ord) {
          dd <- per[[k]]; yy <- strsplit(k, "_")[[1]]
          nrb_rows[[length(nrb_rows) + 1]] <- list(
            period = paste0(yy[1], "\\textendash{}", yy[2]),
            nrb  = thousands(dd[["NRB"]]),
            harv = thousands(dd[["Harv"]]),
            fnrb = thousands(dd[["fNRB"]]),
            full = identical(k, full_key))
        }
      }
    }
  }

  ## ---- title-page assets --------------------------------------------------
  png_dirs <- file.path(base_dir, outdirs, "png")
  copy_first("Area_of_Interest", "Area_of_Interest.png", png_dirs)
  copy_first("sponsors_banner",  "sponsors_banner.png",  WIZ)
  copy_first("UNAM",             "UNAM.png",             WIZ)
  copy_first("SEI",              "SEI.png",              WIZ)

  ## ---- body figures (auto-detect, size-aware) ----------------------------
  FIG_SPEC <- list(
    list("AGB_NRB_fNRB",
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
    list("Boxplots",
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
          file.copy(file.path(sd, hit[1]), file.path(ASSETS, dest),
                    overwrite = TRUE)
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
  mp4_dest <- NULL
  for (cand in c(file.path(latex_dir, "Growth_Harvest_AniOutBaU.mp4"),
                 file.path(base_dir, "OutBaU", "Growth_Harvest_AniOutBaU.mp4"))) {
    if (file.exists(cand)) {
      mp4_dest <- basename(cand)
      file.copy(cand, file.path(ASSETS, mp4_dest), overwrite = TRUE); break
    }
  }

  ## ---- write fragments ----------------------------------------------------
  wf <- function(name, text)
    writeLines(text, file.path(BUILD, name), useBytes = TRUE)

  macro <- function(n, v) sprintf("\\newcommand{\\%s}{%s\\xspace}", n, v)
  meta <- c(
    macro("mfCountry",     latex_escape(country)),
    macro("mfScenario",    latex_escape(gsub("_", " ", scecode))),
    macro("mfScenarioRaw", latex_escape(scecode)),
    macro("mfUser",        latex_escape(sub("[,\\s]+$", "", userdata))),
    macro("mfStartYear",   latex_escape(start_year)),
    macro("mfSimLen",      latex_escape(sim_len)),
    macro("mfMCruns",      latex_escape(mc_runs)),
    macro("mfResolution",  latex_escape(resolution)),
    macro("mfSceType",     latex_escape(sce_type)),
    macro("mfUnitName",    latex_escape(if (nzchar(unit_name)) unit_name else country)))
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
    nt <- c("\\begin{tabular}{@{}l r r r@{}}", "\\toprule",
            "\\rowcolor{mfgreen!12}\\textbf{Period} & \\textbf{NRB (t)} & \\textbf{Total harvest (t)} & \\textbf{fNRB (\\%)}\\\\",
            "\\midrule")
    for (i in seq_along(nrb_rows)) {
      r <- nrb_rows[[i]]
      if (isTRUE(r$full)) {
        nt <- c(nt, "\\midrule",
                sprintf("\\rowcolor{mfgreen!18}\\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s}\\\\",
                        r$period, r$nrb, r$harv, r$fnrb))
      } else {
        sh <- if (i %% 2 == 0) "\\rowcolor{mfgreen!4}" else ""
        nt <- c(nt, sprintf("%s%s & %s & %s & %s\\\\", sh, r$period, r$nrb, r$harv, r$fnrb))
      }
    }
    nt <- c(nt, "\\bottomrule", "\\end{tabular}")
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
    fo <- c(fo, sprintf("\\section{%s}", role_title[[role]]))
    for (g in grp) {
      fo <- c(fo, "\\begin{figure}[H]\\centering",
              sprintf("  \\includegraphics[%s]{assets/%s}", g$sizing, g$dest),
              sprintf("  \\caption{%s}", g$cap),
              "\\end{figure}", "\\FloatBarrier")
    }
  }
  wf("_figures.tex", fo)

  if (!is.null(mp4_dest)) {
    anim <- c("\\section{Animation of fuelwood harvest and AGB}",
      "\\begin{mfnote}{Interactive content}",
      sprintf(paste0("A spatio-temporal animation of fuelwood harvest and ",
        "aboveground biomass is available for this run: ",
        "\\href{run:assets/%s}{\\texttt{%s}}. Open this PDF in a viewer that ",
        "allows launching local files, or play the file directly from the ",
        "report folder."), mp4_dest, latex_escape(mp4_dest)),
      "\\end{mfnote}")
    wf("_animation.tex", anim)
  } else wf("_animation.tex", "")

  ## ---- ensure master templates are in the build dir ----------------------
  for (tf in c("mofuss_report.tex", "title_page.tex")) {
    src <- file.path(latex_dir, tf)
    if (file.exists(src)) file.copy(src, file.path(BUILD, tf), overwrite = TRUE)
    if (!file.exists(file.path(BUILD, tf)))
      stop("Missing template: ", tf, " (expected in ", latex_dir, ")")
  }

  ## ---- compile (two passes; --enable-installer avoids the 0xC0000409 ------
  ##      crash when MiKTeX must fetch a package on the fly) -----------------
  oldwd <- getwd(); setwd(BUILD); on.exit(setwd(oldwd), add = TRUE)
  for (pass in 1:2) {
    out <- system2(pdflatex,
      args = c("-interaction=nonstopmode", "--enable-installer", "mofuss_report.tex"),
      stdout = TRUE, stderr = TRUE)
    if (!any(grepl("Rerun to get", out)) && pass >= 1) {
      if (pass == 2 || !any(grepl("Rerun to get", out))) {}
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
  if (!file.exists(pdf_built)) stop("Compilation failed: no PDF produced.")
  out_pdf <- file.path(SR, sprintf("Mofuss_Summary_Report_%s.pdf", scecode))
  file.copy(pdf_built, out_pdf, overwrite = TRUE)
  message("Report written: ", out_pdf)
  if (isTRUE(open_pdf)) try(utils::browseURL(out_pdf), silent = TRUE)
  invisible(out_pdf)
}
