# Shared NRB accounting for maps, tables and emissions stage 1.
# GrowthNN is preharvest STOCK, not the annual biomass increment.
MOFUSS_NRB_CONTRACT <- "woodfuel_attributed_signed_balance_v1"

mofuss_nrb_context <- function(run_dir, luc_mode = NULL, expected_steps = NULL,
                               mc = 1L, model_path = NULL) {
  if (!requireNamespace("raster", quietly = TRUE)) stop("NRB accounting requires raster.")
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  if (grepl("^debugging_[0-9]+$", basename(run_dir))) {
    mc <- as.integer(sub("^debugging_", "", basename(run_dir)))
    run_dir <- dirname(run_dir)
  }
  if (length(mc) != 1L || is.na(mc) || mc < 1L || mc != as.integer(mc)) stop("Invalid MC index.")
  evidence <- integer()
  add_mode <- function(value, source) {
    numeric_value <- suppressWarnings(as.numeric(value))
    if (length(numeric_value) != 1L || !is.finite(numeric_value) ||
        numeric_value != as.integer(numeric_value)) stop("Invalid LUC selector in ", source)
    evidence[source] <<- as.integer(numeric_value)
  }
  if (!is.null(luc_mode)) add_mode(luc_mode, "argument")
  for (rel in c("Temp/mc_batch_ready.csv", "LULCC/TempTables/mc_batch_ready.csv")) {
    path <- file.path(run_dir, rel)
    if (file.exists(path)) {
      ready <- read.csv(path, stringsAsFactors = FALSE)
      if ("lulc_version" %in% names(ready)) {
        if ("status" %in% names(ready) &&
            (anyNA(ready$status) || any(ready$status != "ready"))) {
          stop("MC batch is not ready: ", path)
        }
        values <- unique(ready$lulc_version)
        add_mode(values, rel)
      }
    }
  }
  prep_path <- file.path(run_dir, "windows_performance_preparation.json")
  if (file.exists(prep_path)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Reading run provenance requires jsonlite.")
    prep <- jsonlite::fromJSON(prep_path)
    if (!is.null(prep$luc)) add_mode(prep$luc, "preparation manifest")
    if (is.null(model_path) && !is.null(prep$model)) model_path <- file.path(run_dir, basename(prep$model))
  }
  debug_dir <- file.path(run_dir, paste0("debugging_", mc))
  ledger_present <- length(list.files(debug_dir, pattern = "^Woodfuel_balance[0-9]+[.]tif$")) > 0L
  if (is.null(model_path)) {
    candidates <- list.files(run_dir, pattern = "^10_dyn_.*[.]egoml$", full.names = TRUE)
    if (length(candidates) == 1L) {
      model_path <- candidates[[1L]]
    } else if (length(candidates) > 1L && (ledger_present || any(evidence == 3L))) {
      # Standard scenario bundles contain v13, v13-Linux and v14. Runtime
      # batch metadata chooses the LUC channel; persisted wizard defaults do
      # not identify which channel was actually executed.
      if (!requireNamespace("xml2", quietly = TRUE)) stop("Reading model provenance requires xml2.")
      matches <- vapply(candidates, function(path) {
        doc <- xml2::read_xml(path)
        prop <- xml2::xml_find_first(doc, "./property[@key='mofuss.nrb.attribution.contract']")
        !inherits(prop, "xml_missing") &&
          identical(xml2::xml_attr(prop, "value"), MOFUSS_NRB_CONTRACT)
      }, logical(1))
      if (sum(matches) != 1L) {
        stop("Cannot identify one corrected NRB model in the scenario bundle; provide verified model_path.")
      }
      model_path <- candidates[matches][[1L]]
    }
  }
  contract <- NULL
  if (!is.null(model_path)) {
    if (!file.exists(model_path)) stop("Selected run model is missing: ", model_path)
    if (!requireNamespace("xml2", quietly = TRUE)) stop("Reading model provenance requires xml2.")
    model <- xml2::read_xml(model_path)
    selector <- xml2::xml_find_all(model, ".//functor[property[@key='wizard.constant.input' and @value='Int_constant_4']]/inputport[@name='constant' and not(@peerid)]")
    # Use saved wizard values only when actual runtime/preparation evidence is
    # unavailable. Users can select a different channel without saving XML.
    if (!length(evidence) && length(selector)) add_mode(unique(xml2::xml_text(selector)), "selected model")
    prop <- xml2::xml_find_first(model, "./property[@key='mofuss.nrb.attribution.contract']")
    if (!inherits(prop, "xml_missing")) contract <- xml2::xml_attr(prop, "value")
  }
  if (!length(evidence)) stop("Unknown LUC mode; provide verified luc_mode or run provenance. Annual files alone do not establish the active LUC channel.")
  if (length(unique(evidence)) != 1L) stop("Conflicting LUC provenance: ", paste(names(evidence), evidence, sep = "=", collapse = ", "))
  mode <- unname(evidence[[1L]])
  if (!mode %in% c(1L, 3L)) stop("Unsupported LUC mode for NRB attribution: ", mode)
  growth_files <- list.files(debug_dir, pattern = "^Growth[0-9]+[.]tif$")
  steps <- sort(as.integer(sub("^Growth([0-9]+)[.]tif$", "\\1", growth_files)))
  if (is.null(expected_steps)) expected_steps <- if (length(steps)) max(steps) else 0L
  if (length(expected_steps) != 1L || !is.finite(expected_steps) || expected_steps < 1L ||
      expected_steps != as.integer(expected_steps)) stop("Invalid expected annual NRB step count in ", debug_dir)
  expected_steps <- as.integer(expected_steps)
  required <- seq_len(expected_steps)
  if (!identical(steps, required)) stop("Incomplete or stale annual sequence in ", debug_dir)
  paths <- function(stem) file.path(debug_dir, sprintf("%s%02d.tif", stem, required))
  for (stem in c("Growth", "Growth_less_harv", "Harvest_tot")) {
    missing <- paths(stem)[!file.exists(paths(stem))]
    if (length(missing)) stop("Missing NRB input: ", missing[[1L]])
  }
  ledger <- paths("Woodfuel_balance")
  any_ledger <- any(file.exists(ledger))
  corrected <- identical(contract, MOFUSS_NRB_CONTRACT)
  if (mode == 3L || any_ledger || corrected) {
    if (!corrected || !all(file.exists(ledger))) {
      stop("Corrected NRB requires a model with contract ", MOFUSS_NRB_CONTRACT,
           " and every annual Woodfuel_balance raster. Rerun the corrected model in a new run directory; raw AGB differences cannot attribute dynamic LUC losses.")
    }
    method <- MOFUSS_NRB_CONTRACT
  } else {
    method <- "legacy_fixed_luc_stock_difference"
  }
  structure(list(run_dir = run_dir, debug_dir = debug_dir, mc = as.integer(mc),
                 luc_mode = mode, expected_steps = expected_steps, method = method,
                 model_path = model_path, provenance = evidence), class = "mofuss_nrb_context")
}

mofuss_period_nrb <- function(context, start_step, end_step,
                              baseline = c("preharvest", "previous_postharvest")) {
  baseline <- match.arg(baseline)
  if (!inherits(context, "mofuss_nrb_context")) stop("Use mofuss_nrb_context first.")
  indices <- c(start_step, end_step)
  if (anyNA(indices) || any(indices != as.integer(indices)) || start_step < 1L ||
      end_step < start_step || end_step > context$expected_steps) stop("Invalid NRB period.")
  read_map <- function(stem, step) raster::raster(file.path(context$debug_dir, sprintf("%s%02d.tif", stem, step)))
  harvest_maps <- lapply(seq.int(start_step, end_step), function(step) read_map("Harvest_tot", step))
  harvest <- if (length(harvest_maps) == 1L) harvest_maps[[1L]] else
    raster::calc(raster::stack(harvest_maps), sum, na.rm = FALSE)
  post_end <- read_map("Growth_less_harv", end_step)
  if (identical(context$method, MOFUSS_NRB_CONTRACT)) {
    balance_end <- read_map("Woodfuel_balance", end_step)
    if (baseline == "preharvest") {
      start_depletion <- raster::overlay(
        read_map("Growth", start_step), read_map("Growth_less_harv", start_step),
        read_map("Harvest_tot", start_step), fun = function(b, p, h) {
          # The engine carries its ledger across a temporary growth-domain
          # gap with no harvest. Missing B/P must not erase that valid balance.
          valid <- is.finite(b) & is.finite(p)
          ifelse(valid, b - p, ifelse(is.finite(h) & h <= 0, 0, NA_real_))
        }
      )
      signed <- balance_end - read_map("Woodfuel_balance", start_step) +
        start_depletion
    } else if (start_step > 1L) {
      signed <- balance_end - read_map("Woodfuel_balance", start_step - 1L)
    } else {
      signed <- balance_end
    }
  } else {
    if (baseline == "preharvest") {
      initial <- read_map("Growth", start_step)
    } else if (start_step > 1L) {
      initial <- read_map("Growth_less_harv", start_step - 1L)
    } else {
      path <- file.path(context$run_dir, "Temp", sprintf("2_IniSt%02d.tif", context$mc))
      if (!file.exists(path)) stop("Initial stock required for a first-step end-previous-year baseline: ", path)
      initial <- raster::raster(path)
    }
    signed <- initial - post_end
  }
  # Preserve signed growth offsets until the period is selected. Clipping each
  # year before summing would inflate period NRB by discarding later recovery.
  nrb <- raster::overlay(signed, harvest, fun = function(x, h) {
    ifelse(!is.finite(x) | !is.finite(h), NA_real_, pmax(0, pmin(h, x)))
  })
  fnrb <- raster::overlay(nrb, harvest, fun = function(n, h) {
    ifelse(!is.finite(n) | !is.finite(h) | h <= 0, NA_real_, 100 * n / h)
  })
  list(nrb = nrb, harvest = harvest, fnrb = fnrb, method = context$method,
       baseline = baseline, start_step = start_step, end_step = end_step,
       ratio_units = "percent", zero_harvest_ratio = "undefined_NA")
}
