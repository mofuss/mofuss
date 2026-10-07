#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Read-only pixel replay of completed Windows v14 annual-LUC runs.
# Outputs must be explicitly directed outside the canonical run/source tree.
# Replays local growth, harvest caps, post-harvest stock and feedback, using
# recorded harvest requests. It does not certify the spatial allocation of
# those requests; whole-model v13/v14 parity tests cover that separately.
# Supported graph scope: retired deforestation/gain machinery is disabled.
# Corrected rules applied to old outputs are a one-step equation audit using
# recorded prior post-stock, not a forecast of a complete corrected run.

suppressPackageStartupMessages(library(terra))

f32 <- function(x) readBin(writeBin(as.double(x), raw(), size = 4L),
                          "double", n = length(x), size = 4L)

annual_model_domain <- function(luc, tof, initial, rules) {
  # Initial model stock already includes v13's TOF allowance. Raw AGB is not
  # the eligibility mask, and a later CR-parameter failure must not replace it.
  if (rules == "fixed_initial") {
    excluded <- !is.finite(initial)
    luc[excluded] <- NA_real_
    tof[excluded] <- NA_real_
  }
  list(luc = luc, tof = tof)
}

annual_start_state <- function(previous_stock, previous_luc, luc, tof, category_k,
                               transition, rules) {
  out <- previous_stock
  domain <- is.finite(luc) & is.finite(tof) & is.finite(category_k)
  out[!domain] <- NA_real_
  allowance <- domain & tof == 1
  out[allowance] <- category_k[allowance]
  if (rules == "legacy") {
    fill <- domain & tof == 0 & !is.finite(out)
  } else {
    fill <- domain & tof == 0 & !is.finite(previous_luc)
  }
  out[fill] <- 0
  reset <- domain & transition %in% c(1, 2, 4)
  out[reset] <- 0
  out
}

annual_capacity <- function(luc, baseline_luc, baseline_k, category_k, rules) {
  out <- category_k
  if (rules == "legacy") {
    keep <- is.finite(luc) & is.finite(baseline_luc) &
      luc == baseline_luc & is.finite(baseline_k)
    out[keep] <- baseline_k[keep]
  } else {
    keep <- is.finite(luc) & is.finite(baseline_luc) & luc == baseline_luc
    out[keep] <- baseline_k[keep]
  }
  out[!is.finite(luc) | !is.finite(category_k)] <- NA_real_
  out
}

annual_growth <- function(stock, tof, capacity, rmax, cr, transition, rules) {
  out <- stock
  forest <- is.finite(tof) & tof == 0
  out[!is.finite(tof)] <- NA_real_
  out[forest] <- NA_real_
  if (is.null(cr)) {
    out[forest & is.finite(capacity) & capacity <= 0] <- 0
    z <- forest & is.finite(stock) & is.finite(capacity) & capacity > 0 &
      is.finite(rmax)
    raw <- f32(stock[z] + stock[z] * rmax[z] * (1 - stock[z] / capacity[z]))
    out[z] <- pmin(capacity[z], pmax(0, raw))
  } else {
    z <- forest & is.finite(stock) & is.finite(cr$A) & cr$A > 0 &
      is.finite(cr$k) & cr$k > 0 & is.finite(cr$m) & cr$m > 0
    ratio <- stock[z] / cr$A[z]
    ratio <- ifelse(stock[z] <= 0, 0, ifelse(ratio >= 1, .999999, ratio))
    age <- f32(-log1p(-ratio^(1 / cr$m[z])) / cr$k[z])
    target <- cr$A[z] * (1 - exp(-cr$k[z] * (age + 1)))^cr$m[z]
    out[z] <- f32(pmax(stock[z], target))
  }
  if (rules %in% c("corrected", "fixed_initial"))
    out[transition %in% c(1, 2, 4)] <- 0
  out
}

annual_feedback <- function(post, tof, transition) {
  out <- post
  out[is.finite(tof) & tof == 0 & is.finite(post) & post <= 0] <- 2
  out[transition %in% c(1, 2, 4)] <- 0
  out
}

comparison <- function(actual, expected, domain, tol) {
  both <- domain & is.finite(actual) & is.finite(expected)
  error <- abs(actual[both] - expected[both])
  c(compared = sum(both), finite_pattern_mismatches =
      sum(domain & xor(is.finite(actual), is.finite(expected))),
    over_tolerance = sum(error > tol),
    max_abs_error = if (length(error)) max(error) else 0)
}

run_audit <- function(run, output, rules = "legacy", mc_ids = 1:3,
                      last_year = 2050L, tol = .01) {
  if (!is.character(rules) || length(rules) != 1L || is.na(rules) ||
      !rules %in% c("legacy", "corrected", "fixed_initial"))
    stop("rules must be exactly 'legacy', 'corrected' or 'fixed_initial'.")
  whole <- function(x) is.numeric(x) && length(x) > 0L &&
    all(is.finite(x) & x == floor(x))
  if (!whole(mc_ids) || any(mc_ids < 1L) || anyDuplicated(mc_ids))
    stop("mc_ids must contain unique positive integers.")
  if (!whole(last_year) || length(last_year) != 1L)
    stop("last_year must be one finite integer.")
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol < 0)
    stop("tol must be one finite nonnegative number.")
  for (path in list(run, output))
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
      stop("run and output must each be one nonempty path.")
  run <- normalizePath(run, winslash = "/", mustWork = TRUE)
  output <- normalizePath(output, winslash = "/", mustWork = FALSE)
  if (startsWith(tolower(paste0(output, "/")), tolower(paste0(run, "/"))))
    stop("Audit outputs must be outside the canonical run.")
  source_root <- getOption("mofuss.audit.source_root")
  if (!is.null(source_root) && startsWith(tolower(paste0(output, "/")),
                                         tolower(paste0(source_root, "/"))))
    stop("Audit outputs must be outside the source repository.")
  raster_dir <- file.path(run, "LULCC/TempRaster")
  pars <- read.csv(file.path(run, "LULCC/TempTables/parameters_dinamica.csv"))
  par <- function(key) as.integer(pars$ParCHR[match(key, pars$Var)])
  first_year <- par("start_year")
  end_year <- par("end_year")
  if (!whole(c(first_year, end_year)) || end_year < first_year || last_year < first_year)
    stop("Invalid run year bounds or last_year precedes the run start.")
  years <- seq.int(first_year, min(last_year, end_year))
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  template <- rast(file.path(raster_dir, "LULCt3_c.tif"))
  vec <- function(path) {
    x <- rast(path)
    if (!compareGeom(x, template, stopOnError = FALSE)) stop("Grid mismatch: ", path)
    as.numeric(values(x, mat = FALSE))
  }
  baseline_luc <- vec(file.path(raster_dir, "LULCt3_c.tif"))
  baseline_tof <- vec(file.path(raster_dir, "TOFvsFOR_mask3_2000.tif"))
  raw_agb <- vec(file.path(raster_dir, "agb3_c.tif"))
  crosswalk <- read.csv(file.path(run, "LULCC/TempTables/woodman_key_crosswalk.csv"))
  forest_keys <- crosswalk$Key[crosswalk$luc_code %in% c(44L, 45L)]
  tof_table <- read.csv(file.path(run, "LULCC/TempTables/TOFvsFOR_Categories3.csv"))
  cr <- if (par("uncapped_regrowth") == 1L) list(
    A = vec(file.path(raster_dir, "A_c.tif")),
    k = vec(file.path(raster_dir, "k_c.tif")),
    m = vec(file.path(raster_dir, "m_c.tif"))) else NULL
  rows <- list()
  nrow_out <- 0L
  for (mc in mc_ids) {
    temp <- file.path(run, "Temp")
    dbg <- file.path(run, sprintf("debugging_%d", mc))
    initial <- vec(file.path(temp, sprintf("2_IniSt%02d.tif", mc)))
    baseline_k <- vec(file.path(temp, sprintf("2_K%02d.tif", mc)))
    ktab <- read.csv(file.path(temp, sprintf("mc_k_%02d.csv", mc)))
    rtab <- read.csv(file.path(temp, sprintf("mc_rmax_%02d.csv", mc)))
    ktab$Value <- f32(ktab$Value)
    rtab$Value <- f32(rtab$Value)
    prior_luc <- baseline_luc
    prior_tof <- baseline_tof
    feedback <- initial
    unchanged <- is.finite(baseline_luc)
    initial_null <- is.finite(baseline_tof) & baseline_tof == 0 & !is.finite(initial)
    initial_zero <- is.finite(baseline_tof) & baseline_tof == 0 &
      is.finite(initial) & initial == 0
    for (year in years) {
      step <- year - par("start_year") + 1L
      map <- function(family) vec(file.path(dbg, sprintf("%s%02d.tif", family, step)))
      luc <- vec(file.path(raster_dir, sprintf("LULCt3_c_%d.tif", year)))
      tof <- vec(file.path(raster_dir, sprintf("TOFvsFOR_mask3_%d.tif", year)))
      model_domain <- annual_model_domain(luc, tof, initial, rules)
      luc <- model_domain$luc
      tof <- model_domain$tof
      transition_raw <- vec(file.path(raster_dir, sprintf("LULCt3_transition_%d.tif", year)))
      transition <- transition_raw
      transition[is.finite(luc) & !is.finite(transition)] <- 0
      transition[!is.finite(luc)] <- NA_real_
      domain <- is.finite(luc) & is.finite(tof)
      same <- domain & is.finite(prior_luc) & luc == prior_luc
      unchanged <- unchanged & same & transition == 0
      unchanged[is.na(unchanged)] <- FALSE
      category_k <- ktab$Value[match(luc, ktab$Key)]
      rate <- rtab$Value[match(luc, rtab$Key)]
      rate[transition %in% c(1, 2, 4)] <- 0
      capacity <- annual_capacity(luc, baseline_luc, baseline_k, category_k, rules)
      stock <- annual_start_state(feedback, prior_luc, luc, tof, category_k,
                                  transition, rules)
      expected_growth <- annual_growth(stock, tof, capacity, rate, cr,
                                       transition, rules)
      growth <- map("Growth")
      harvest <- map("Harvest_tot")
      post <- map("Growth_less_harv")
      requested <- map("Expect_harv_tot")
      expected_harvest <- rep(0, length(growth))
      z <- is.finite(requested) & requested > 0 & is.finite(growth) & growth > 0
      expected_harvest[z] <- pmin(requested[z], growth[z])
      expected_post <- growth
      z <- domain & tof == 0 & is.finite(harvest)
      expected_post[z] <- f32(growth[z] - harvest[z])
      # Independently reconstruct transition codes from consecutive LUC/TOF.
      expected_transition <- rep(NA_real_, length(luc))
      overlap <- domain & is.finite(prior_luc) & is.finite(prior_tof)
      expected_transition[domain] <- 0
      if (step > 1L) {
        was_forest <- prior_luc %in% forest_keys
        is_forest <- luc %in% forest_keys
        expected_transition[overlap & prior_tof == 0 & tof == 1] <- 3
        expected_transition[overlap & prior_tof == 1 & tof == 0] <- 4
        expected_transition[overlap & !was_forest & is_forest] <- 2
        expected_transition[overlap & was_forest & !is_forest] <- 1
      }
      reset <- domain & transition %in% c(1, 2, 4)
      named_comparison <- function(actual, expected, prefix) {
        x <- comparison(actual, expected, domain, tol)
        setNames(x, paste0(prefix, names(x)))
      }
      stats <- c(named_comparison(growth, expected_growth, "growth_"),
                 named_comparison(harvest, expected_harvest, "harvest_"),
                 named_comparison(post, expected_post, "post_"))
      expected_tof <- tof_table[[2L]][match(luc, tof_table[[1L]])]
      nrow_out <- nrow_out + 1L
      rows[[nrow_out]] <- data.frame(run = basename(run), rules = rules, mc = mc,
        year = year, domain_cells = sum(domain),
        luc_tof_support_mismatches = sum(xor(is.finite(luc), is.finite(tof))),
        tof_category_mismatches = sum(domain & tof != expected_tof, na.rm = TRUE),
        luc_missing_tof_lookup_cells = sum(is.finite(luc) & !is.finite(expected_tof)),
        finite_growth_outside_domain = sum(!domain & is.finite(growth)),
        finite_post_outside_domain = sum(!domain & is.finite(post)),
        initial_null_non_tof_cells = sum(initial_null),
        initial_null_finite_growth = sum(!is.finite(initial) & is.finite(growth)),
        initial_null_finite_post = sum(!is.finite(initial) & is.finite(post)),
        initial_null_positive_request = sum(!is.finite(initial) &
          is.finite(requested) & requested > tol),
        initial_null_positive_harvest = sum(!is.finite(initial) &
          is.finite(harvest) & harvest > tol),
        unchanged_initial_null_cells = sum(unchanged & initial_null),
        unchanged_initial_null_finite_growth = sum(unchanged & initial_null & is.finite(growth)),
        unchanged_initial_null_finite_post = sum(unchanged & initial_null & is.finite(post)),
        unchanged_initial_null_post_Mg = sum(post[unchanged & initial_null], na.rm = TRUE),
        initial_zero_non_tof_cells = sum(initial_zero),
        unchanged_zero_K_positive_growth = sum(unchanged & initial_zero &
          is.finite(baseline_k) & baseline_k == 0 & is.finite(growth) & growth > tol),
        # The zero-K invariant applies to capped logistic growth only.
        growth_law = if (is.null(cr)) "logistic" else "chapman-richards",
        transition_code_errors = sum(domain & transition != expected_transition, na.rm = TRUE),
        reset_transition_cells = sum(reset),
        reset_transition_positive_growth = sum(reset & is.finite(growth) & growth > tol),
        reset_transition_positive_harvest = sum(reset & is.finite(harvest) & harvest > tol),
        reset_transition_positive_post = sum(reset & is.finite(post) & post > tol),
        reset_transition_harvest_Mg = sum(harvest[reset], na.rm = TRUE),
        raw_null_finite_post_cells = sum(domain & !is.finite(raw_agb) & is.finite(post)),
        as.list(stats), check.names = FALSE)
      feedback <- annual_feedback(post, tof, transition)
      prior_luc <- luc
      prior_tof <- tof
      if (step %% 10L == 1L || year == tail(years, 1)) {
        write.csv(do.call(rbind, rows), file.path(output, "annual_pixel_replay.csv"), row.names = FALSE)
        message(basename(run), " MC", mc, " year ", year,
          "; growth errors=", stats[["growth_over_tolerance"]],
          "; growth mask errors=", stats[["growth_finite_pattern_mismatches"]])
      }
    }
  }
  result <- do.call(rbind, rows)
  write.csv(result, file.path(output, "annual_pixel_replay.csv"), row.names = FALSE)
  result
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  script_arg <- commandArgs()[startsWith(commandArgs(), "--file=")]
  if (length(script_arg)) options(mofuss.audit.source_root = normalizePath(
    file.path(dirname(sub("^--file=", "", script_arg[[1L]])), ".."),
    winslash = "/", mustWork = TRUE))
  getarg <- function(name, default = NULL) {
    a <- args[startsWith(args, paste0("--", name, "="))]
    if (!length(a)) return(default)
    sub(paste0("^--", name, "="), "", a[[1L]])
  }
  run <- getarg("run")
  output <- getarg("output")
  rules <- getarg("rules", "legacy")
  if (is.null(run) || is.null(output) ||
      !rules %in% c("legacy", "corrected", "fixed_initial"))
    stop("Usage: --run=DIR --output=DIR [--rules=legacy|corrected|fixed_initial] [--mc=1,2,3] [--last-year=2050]")
  run_audit(run, output, rules,
            as.integer(strsplit(getarg("mc", "1,2,3"), ",", fixed = TRUE)[[1L]]),
            as.integer(getarg("last-year", "2050")))
}

if (sys.nframe() == 0L) invisible(main())
