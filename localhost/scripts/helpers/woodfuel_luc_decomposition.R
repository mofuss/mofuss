# SPDX-License-Identifier: Apache-2.0
# Observational v14 process accounting. Stocks and ledgers are Mg dry biomass
# per original grid cell. No carbon factor, area multiplier or model mutation.
MOFUSS_LUC_DECOMPOSITION_CONTRACT <- "paired_signed_woodfuel_direct_luc_v1"
MOFUSS_WOODMAN_FREEZE_CONTRACT <- "woodman_freeze_year_v1"

.mofuss_luc_stop <- function(...) stop(..., call. = FALSE)
.mofuss_luc_f32 <- function(x) readBin(writeBin(as.double(x), raw(), size = 4L),
                                     "double", n = length(x), size = 4L)
.mofuss_luc_equal <- function(a, b) {
  all((!is.finite(a) & !is.finite(b)) |
        (is.finite(a) & is.finite(b) & a == b))
}
.mofuss_luc_feedback <- function(p, tof, transition) {
  e <- p
  e[is.finite(tof) & tof == 0 & is.finite(p) & p <= 0] <- 2
  e[transition %in% c(1, 2, 4)] <- 0
  e
}
.mofuss_luc_start <- function(previous, previous_luc, luc, tof, category_k, transition) {
  s <- previous
  domain <- is.finite(luc) & is.finite(tof) & is.finite(category_k) & is.finite(transition)
  s[!domain] <- NA_real_
  s[domain & tof == 1] <- category_k[domain & tof == 1]
  s[domain & tof == 0 & !is.finite(previous_luc)] <- 0
  s[domain & transition %in% c(1, 2, 4)] <- 0
  s
}

# The same increment is used by process attribution and the shared NRB reader.
# Deriving it from stocks avoids the historical signed-ledger NoData=-9999
# collision, including a collision propagated through subsequent engine years.
# This is double-precision reconstruction; existing float32 ledgers are an
# independent validation target, not the source of repaired values.
.mofuss_luc_increment <- function(previous, current, initial_support) {
  ep <- if (previous$step == 0L) previous$p else
    .mofuss_luc_feedback(previous$p, previous$tof, previous$tr)
  s <- .mofuss_luc_start(ep, previous$lc, current$lc, current$tof, current$k, current$tr)
  e <- .mofuss_luc_feedback(current$p, current$tof, current$tr)
  valid <- initial_support & is.finite(s) & is.finite(current$b) &
    is.finite(current$p) & is.finite(e) & is.finite(current$h) & current$h >= 0
  seed <- rep(0, length(ep))
  if (previous$step > 0L) {
    ps <- is.finite(ep) & is.finite(previous$p) & is.finite(previous$b)
    if (!is.null(previous$state_valid)) ps <- ps & previous$state_valid
    seed[ps] <- ep[ps] - previous$p[ps]
  }
  delta <- rep(NA_real_, length(ep))
  delta[valid] <- pmin(s[valid], current$b[valid]) - current$p[valid] - seed[valid]
  gap <- initial_support & !valid & is.finite(current$h) & current$h <= 0
  delta[gap] <- -seed[gap]
  list(delta = delta, start = s, previous_end = ep, valid = valid,
       carried_gap = gap, previous_seed = seed)
}

# A staged model/parameter update is not evidence that existing annual outputs
# used the new freeze setting. The native model writes this observer beside the
# physical outputs for each MC realization; legacy models have no freeze branch.
.mofuss_luc_freeze_provenance <- function(run, mc, doc, mode, first = NULL) {
  marker <- xml2::xml_attr(xml2::xml_find_first(doc,
    "./property[@key='mofuss.woodman.freeze.contract']"), "value")
  path <- file.path(run, sprintf("debugging_%d", mc), "woodman_luc_execution.csv")
  enabled <- !is.na(marker)
  if (enabled && !identical(marker, MOFUSS_WOODMAN_FREEZE_CONTRACT))
    .mofuss_luc_stop("Unsupported Woodman freeze contract in ", run)
  if (!enabled) {
    if (file.exists(path)) .mofuss_luc_stop("Freeze execution/model contract conflict in ", run)
    return(list(year = 2050L, contract = "legacy_annual_history_no_freeze_parameter",
                evidence = "legacy_model", paths = character()))
  }
  if (!file.exists(path)) .mofuss_luc_stop("Missing executed Woodman freeze provenance: ", path,
    ". A staged model does not establish the settings used by existing outputs; finish the rerun first.")
  tab <- utils::read.csv(path, check.names = FALSE)
  # Native SaveLookupTable uses `Key*, Value,` (with an optional empty
  # trailing column), whereas R fixture/provenance writers use Key,Value.
  names(tab) <- sub("[*]$", "", trimws(names(tab)))
  if (!all(c("Key", "Value") %in% names(tab)) || anyDuplicated(tab$Key))
    .mofuss_luc_stop("Invalid Woodman freeze execution table: ", path)
  read_key <- function(key) {
    z <- suppressWarnings(as.numeric(tab$Value[tab$Key == key]))
    if (length(z) != 1L || !is.finite(z) || z != as.integer(z))
      .mofuss_luc_stop("Invalid Woodman execution key ", key, " in ", path)
    as.integer(z)
  }
  executed_mode <- read_key(1L); freeze <- read_key(2L)
  if (read_key(3L) != 1L || executed_mode != mode || !freeze %in% 2000:2050 ||
      (!is.null(first) && read_key(4L) != first))
    .mofuss_luc_stop("Conflicting executed Woodman freeze/LUC provenance in ", path)
  evidence_paths <- path
  for (rel in c("Temp/mc_batch_ready.csv", "LULCC/TempTables/mc_batch_ready.csv")) {
    ready_path <- file.path(run, rel)
    if (!file.exists(ready_path)) next
    ready <- utils::read.csv(ready_path, stringsAsFactors = FALSE)
    recorded <- if ("woodman_luc_freeze_year" %in% names(ready))
      unique(suppressWarnings(as.numeric(ready$woodman_luc_freeze_year))) else 2050
    if (mode == 3L && (length(recorded) != 1L || !is.finite(recorded) || recorded != freeze))
      .mofuss_luc_stop("Executed Woodman freeze year conflicts with MC batch: ", ready_path)
    evidence_paths <- c(evidence_paths, ready_path)
  }
  list(year = freeze, contract = marker, evidence = "native_per_mc_execution_table",
       paths = evidence_paths)
}

.mofuss_luc_context <- function(run, mc) {
  run <- normalizePath(run, winslash = "/", mustWork = TRUE)
  for (pkg in c("terra", "jsonlite", "xml2"))
    if (!requireNamespace(pkg, quietly = TRUE)) .mofuss_luc_stop("LUC decomposition requires ", pkg)
  prep <- file.path(run, "windows_performance_preparation.json")
  manifest <- if (file.exists(prep)) jsonlite::fromJSON(prep) else list()
  model <- if (!is.null(manifest$model)) file.path(run, basename(manifest$model)) else {
    candidates <- list.files(run, "^10_dyn_.*[.]egoml$", full.names = TRUE)
    corrected <- candidates[vapply(candidates, function(x) {
      doc <- xml2::read_xml(x)
      identical(xml2::xml_attr(xml2::xml_find_first(doc,
        "./property[@key='mofuss.nrb.attribution.contract']"), "value"),
        "woodfuel_attributed_signed_balance_v1")
    }, logical(1))]
    if (length(corrected) == 1L) corrected else if (length(candidates) == 1L) candidates else NA_character_
  }
  if (length(model) != 1L || is.na(model) || !file.exists(model))
    .mofuss_luc_stop("Cannot identify executed model in ", run)
  doc <- xml2::read_xml(model)
  contract <- xml2::xml_attr(xml2::xml_find_first(doc,
    "./property[@key='mofuss.nrb.attribution.contract']"), "value")
  corrected <- identical(contract, "woodfuel_attributed_signed_balance_v1")
  dbg <- file.path(run, sprintf("debugging_%d", mc))
  if (!corrected) {
    if (length(list.files(dbg, "^Woodfuel_balance[0-9]+[.]tif$")))
      .mofuss_luc_stop("Ledger/model contract conflict in ", run)
    return(list(run = run, available = FALSE, model = model))
  }
  def <- xml2::xml_find_first(doc, ".//functor[outputport[@id='v253']]/inputport[@name='constant']")
  if (!identical(trimws(xml2::xml_text(def)), ".no"))
    .mofuss_luc_stop("Unsupported legacy deforestation branch in ", model)
  params_path <- file.path(run, "LULCC/TempTables/parameters_dinamica.csv")
  p <- utils::read.csv(params_path, stringsAsFactors = FALSE)
  integer_par <- function(key) {
    z <- suppressWarnings(as.numeric(p$ParCHR[p$Var == key]))
    if (length(z) != 1L || !is.finite(z) || z != as.integer(z))
      .mofuss_luc_stop("Invalid ", key, " in ", params_path)
    as.integer(z)
  }
  mode <- integer()
  if (!is.null(manifest$luc)) mode <- c(mode, as.integer(manifest$luc))
  for (rel in c("Temp/mc_batch_ready.csv", "LULCC/TempTables/mc_batch_ready.csv")) {
    path <- file.path(run, rel)
    if (file.exists(path)) {
      ready <- utils::read.csv(path, stringsAsFactors = FALSE)
      if ("status" %in% names(ready) && any(is.na(ready$status) | ready$status != "ready"))
        .mofuss_luc_stop("Unready MC batch in ", path)
      if ("lulc_version" %in% names(ready)) mode <- c(mode, unique(as.integer(ready$lulc_version)))
    }
  }
  if (!length(mode)) mode <- as.integer(xml2::xml_text(xml2::xml_find_first(doc,
    ".//functor[outputport[@id='v302']]/inputport[@name='constant']")))
  if (anyNA(mode) || length(unique(mode)) != 1L || !mode[1] %in% c(1L, 3L))
    .mofuss_luc_stop("Conflicting/unsupported active LUC provenance in ", run)
  first <- integer_par("start_year"); last <- integer_par("end_year")
  freeze <- .mofuss_luc_freeze_provenance(run, mc, doc, mode[1], first)
  count <- integer_par("monte_carlo_runs"); uncapped <- integer_par("uncapped_regrowth")
  if (last < first || mc < 1L || mc > count || !uncapped %in% 0:1)
    .mofuss_luc_stop("Invalid run bounds/MC/growth mode in ", run)
  kpath <- file.path(run, "Temp", sprintf("mc_k_%02d.csv", mc))
  if (!file.exists(kpath)) .mofuss_luc_stop("Missing category K table: ", kpath)
  k <- utils::read.csv(kpath)
  if (!all(c("Key", "Value") %in% names(k)) || anyDuplicated(k$Key) || any(!is.finite(k$Key)))
    .mofuss_luc_stop("Invalid category K table: ", kpath)
  list(run = run, available = TRUE, model = model, mode = mode[1],
       freeze_year = freeze$year, freeze_contract = freeze$contract,
       freeze_evidence = freeze$evidence,
       first = first, last = last, count = count, uncapped = uncapped,
       k = setNames(.mofuss_luc_f32(k$Value), as.character(k$Key)), mc = mc,
       auxiliary_paths = c(model, params_path, kpath, freeze$paths, if (file.exists(prep)) prep))
}

# Read-only preflight. Corrected-but-incomplete inputs fail; legacy output is
# explicitly unavailable rather than being reinterpreted as signed accounting.
mofuss_luc_pair_status <- function(bau_dir, ics_dir, mc, start_year, end_year) {
  whole <- c(mc, start_year, end_year)
  if (length(whole) != 3L || any(!is.finite(whole)) || any(whole != as.integer(whole)))
    .mofuss_luc_stop("MC and period bounds must be finite integers.")
  b <- .mofuss_luc_context(bau_dir, mc); i <- .mofuss_luc_context(ics_dir, mc)
  if (!b$available || !i$available) return(list(available = FALSE,
    status = "legacy_unavailable", method = MOFUSS_LUC_DECOMPOSITION_CONTRACT,
    reason = "Both scenarios require corrected v14 signed-ledger output."))
  for (key in c("mode", "first", "last", "count", "uncapped"))
    if (!identical(b[[key]], i[[key]])) .mofuss_luc_stop("BAU/ICS mismatch: ", key)
  if (b$mode == 3L && !identical(b$freeze_year, i$freeze_year))
    .mofuss_luc_stop("BAU/ICS mismatch: woodman_luc_freeze_year")
  if (!identical(b$k, i$k)) .mofuss_luc_stop("BAU/ICS category K draws differ.")
  if (start_year < b$first || end_year > b$last || end_year < start_year)
    .mofuss_luc_stop("LUC decomposition period is outside the model horizon.")
  paths <- character()
  add <- function(path) {
    pos <- match(path, paths)
    if (is.na(pos)) { paths <<- c(paths, path); pos <- length(paths) }
    pos
  }
  baseline <- start_year - b$first
  steps <- seq.int(baseline, end_year - b$first + 1L)
  spec <- function(ctx) {
    root <- ctx$run; tr <- file.path(root, "LULCC/TempRaster")
    dbg <- file.path(root, sprintf("debugging_%d", mc))
    initial <- add(file.path(root, "Temp", sprintf("2_IniSt%02d.tif", mc)))
    lcbase <- add(file.path(tr, sprintf("LULCt%d_c.tif", ctx$mode)))
    tofbase <- add(file.path(root, "Temp", sprintf("2_TOFvsFOR%02d.tif", mc)))
    raw <- add(file.path(tr, "agb3_c.tif"))
    one <- function(step) {
      year <- ctx$first + step - 1L
      cover_year <- min(year, ctx$freeze_year)
      lc <- if (step == 0L || ctx$mode == 1L) lcbase else
        add(file.path(tr, sprintf("LULCt3_c_%d.tif", cover_year)))
      tof <- if (step == 0L || ctx$mode == 1L) tofbase else
        add(file.path(tr, sprintf("TOFvsFOR_mask3_%d.tif", cover_year)))
      transition <- if (step == 0L || ctx$mode == 1L || year > ctx$freeze_year) 0L else
        add(file.path(tr, sprintf("LULCt3_transition_%d.tif", year)))
      list(step = step, year = year, land_cover_year = cover_year,
        lc = lc, tof = tof, transition = transition,
        p = if (step == 0L) initial else add(file.path(dbg, sprintf("Growth_less_harv%02d.tif", step))),
        c = if (step == 0L) 0L else add(file.path(dbg, sprintf("Woodfuel_balance%02d.tif", step))),
        b = if (step == 0L) 0L else add(file.path(dbg, sprintf("Growth%02d.tif", step))),
        h = if (step == 0L) 0L else add(file.path(dbg, sprintf("Harvest_tot%02d.tif", step))))
    }
    years <- lapply(steps, one)
    before <- if (baseline > 0L) one(baseline - 1L) else NULL
    list(raw = raw, initial = initial, lcbase = lcbase, years = years, before = before)
  }
  bs <- spec(b); is <- spec(i)
  absent <- paths[!file.exists(paths)]
  if (length(absent)) .mofuss_luc_stop("Missing corrected LUC input: ", absent[1])
  geometry <- terra::rast(paths[1])
  for (path in paths[-1])
    if (!terra::compareGeom(geometry, terra::rast(path), stopOnError = FALSE))
      .mofuss_luc_stop("LUC input grid mismatch: ", path)
  list(available = TRUE, status = "available", method = MOFUSS_LUC_DECOMPOSITION_CONTRACT,
       bau = b, ics = i, bs = bs, is = is, paths = paths, steps = steps,
       auxiliary_paths = unique(c(b$auxiliary_paths, i$auxiliary_paths)),
       years = seq.int(start_year, end_year), start_year = start_year, end_year = end_year)
}

.mofuss_luc_map_fields <- c(
  "raw_signed_woodfuel_effect", "net_stock_benefit", "direct_luc_net_effect",
  "direct_luc_loss", "direct_luc_gain", "tof_allowance_net_effect",
  "capacity_clamp_net_effect", "capacity_class_changed_this_year",
  "capacity_class_changed_from_baseline", "capacity_baseline_class",
  "other_net_effect", "clipped_nrb_saving", "closure_residual",
  "gross_positive_woodfuel_creation", "opening_saved_stock",
  "luc_reversal_of_positive_gap", "luc_transition_exposed_saved_stock",
  "forest_to_tof_reversal", "support_gap_stock_benefit",
  "support_gap_algebraic_adjustment", "direct_forest_clearing_net_effect",
  "direct_new_forest_net_effect", "direct_tof_loss_net_effect",
  "net_stock_benefit_on_ledger_support", "unattributed_stock_benefit",
  "bau_signed_depletion", "ics_signed_depletion", "bau_harvest", "ics_harvest",
  "bau_signed_preharvest")

# One original-grid block. Exported for focused fixture tests, not a second
# physical simulator. The seed is reconstructed solely to locate LUC resets;
# its carbon effect already belongs to the signed woodfuel ledger.
.mofuss_luc_block <- function(x, plan) {
  n <- nrow(x); ny <- length(plan$years); out <- matrix(0, n, length(.mofuss_luc_map_fields),
    dimnames = list(NULL, .mofuss_luc_map_fields))
  v <- function(k) if (k == 0L) rep(0, n) else x[, k]
  b <- plan$bs; i <- plan$is
  if (!.mofuss_luc_equal(v(b$raw), v(i$raw))) .mofuss_luc_stop("BAU/ICS original AGB references differ.")
  if (!.mofuss_luc_equal(v(b$initial), v(i$initial))) .mofuss_luc_stop("BAU/ICS initial model stocks differ.")
  if (!.mofuss_luc_equal(v(b$lcbase), v(i$lcbase))) .mofuss_luc_stop("BAU/ICS baseline land cover differs.")
  state <- function(s, ctx, j = NULL, z = if (is.null(j)) s$before else s$years[[j]]) {
    lc <- v(z$lc); tof <- v(z$tof)
    initial_support <- is.finite(v(s$initial))
    lc[!initial_support] <- NA_real_; tof[!initial_support] <- NA_real_
    tr <- v(z$transition); tr[is.finite(lc) & !is.finite(tr)] <- 0
    tr[!is.finite(lc)] <- NA_real_
    list(p = v(z$p), c = v(z$c), b = v(z$b), h = v(z$h), lc = lc, tof = tof, tr = tr,
         # mc_k's Key is already the land-cover category. The engine's +1
         # indexes a wide MC table including its leading realization column;
         # applying that offset to this exported long table would be wrong.
         k = unname(ctx$k[as.character(lc)]), step = z$step)
  }
  bb <- state(b, plan$bau, 1); ib <- state(i, plan$ics, 1)
  if (bb$step > 0L) {
    bb$state_valid <- .mofuss_luc_increment(state(b, plan$bau), bb,
                                            is.finite(v(b$initial)))$valid
    ib$state_valid <- .mofuss_luc_increment(state(i, plan$ics), ib,
                                            is.finite(v(i$initial)))$valid
  }
  be <- state(b, plan$bau, ny + 1L); ie <- state(i, plan$ics, ny + 1L)
  endpoint <- if (isTRUE(plan$model_support)) is.finite(v(b$initial)) & is.finite(v(i$initial)) else
    is.finite(v(b$raw)) & is.finite(v(i$raw)) &
    is.finite(bb$p) & is.finite(ib$p) & is.finite(be$p) & is.finite(ie$p)
  exported_support <- endpoint & is.finite(bb$c) & is.finite(ib$c) &
    is.finite(be$c) & is.finite(ie$c)
  N <- (ie$p - be$p) - (ib$p - bb$p)
  process <- endpoint; harvest_valid <- endpoint
  if (bb$step > 0L) process <- process & bb$state_valid & ib$state_valid
  recovered_b <- recovered_i <- rep(0, n)
  bp <- bb; ip <- ib
  raw_max_error <- 0; raw_compared <- 0L; raw_missing <- rep(FALSE, n)
  # First pass determines one complete annual support mask. No yearly NA->0
  # coercion may manufacture a physical attribution at a missing-domain pixel.
  for (j in seq_len(ny + 1L)) {
    bz <- if (j == 1L) bb else state(b, plan$bau, j)
    iz <- if (j == 1L) ib else state(i, plan$ics, j)
    for (key in c("lc", "tof", "tr"))
      if (!.mofuss_luc_equal(bz[[key]], iz[[key]]))
        .mofuss_luc_stop("BAU/ICS prescribed land-cover inputs differ in year ", b$years[[j]]$year)
    for (z in list(bz, iz)) {
      process <- process & is.finite(z$p) & is.finite(z$lc) &
        is.finite(z$tof) & is.finite(z$tr) & is.finite(z$k)
      if (j > 1L) {
        process <- process & is.finite(z$b) & is.finite(z$h)
        harvest_valid <- harvest_valid & is.finite(z$h) & z$h >= 0
      }
    }
    if (j > 1L) {
      rb <- .mofuss_luc_increment(bp, bz, is.finite(v(b$initial)))
      ri <- .mofuss_luc_increment(ip, iz, is.finite(v(i$initial)))
      process <- process & rb$valid & ri$valid
      recovered_b <- recovered_b + rb$delta
      recovered_i <- recovered_i + ri$delta
      bz$state_valid <- rb$valid; iz$state_valid <- ri$valid
      for (pair in list(list(previous = bp, current = bz, reconstructed = rb),
                        list(previous = ip, current = iz, reconstructed = ri))) {
        z <- pair$current; p <- pair$previous; r <- pair$reconstructed
        compare <- endpoint & is.finite(z$c) & is.finite(p$c) & is.finite(r$delta)
        err <- abs((z$c - p$c) - r$delta)
        tolerance <- 8 * 2^-23 * pmax(1, abs(z$c), abs(p$c), abs(z$p), abs(p$p), na.rm = TRUE) + 1e-6
        if (any(compare & err > tolerance, na.rm = TRUE)) {
          q <- which(compare & err > tolerance)[1]
          .mofuss_luc_stop("Physical signed increment disagrees with a readable exported ledger. year=",
            plan$years[j-1L], " block_row=", plan$block_row, " block_cell=", q,
            " observed=", z$c[q]-p$c[q], " reconstructed=", r$delta[q],
            " previous_P=", p$p[q], " P=", z$p[q], " B=", z$b[q],
            " S=", r$start[q], " previous_E=", r$previous_end[q],
            " class=", z$lc[q], " TOF=", z$tof[q], " transition=", z$tr[q],
            " K=", z$k[q], " previous_state_valid=", p$state_valid[q])
        }
        if (any(compare)) raw_max_error <- max(raw_max_error, err[compare])
        raw_compared <- raw_compared + sum(compare)
        raw_missing <- raw_missing | (endpoint & (!is.finite(z$c) | !is.finite(p$c)))
      }
    }
    bp <- bz; ip <- iz
  }
  ledger_support <- endpoint & is.finite(recovered_b) & is.finite(recovered_i)
  W <- recovered_b - recovered_i
  process <- process & harvest_valid & ledger_support
  pidx <- which(process)
  annual <- matrix(0, ny, length(.mofuss_luc_map_fields),
                   dimnames = list(NULL, .mofuss_luc_map_fields))
  hb <- hi <- rep(0, n); max_error <- 0; error_count <- 0L
  first_b_delta <- first_b_harvest <- rep(NA_real_, n)
  out[, "raw_signed_woodfuel_effect"] <- W
  out[, "net_stock_benefit"] <- N
  out[, "opening_saved_stock"] <- pmax(0, ib$p - bb$p)
  out[, "support_gap_stock_benefit"] <- ifelse(endpoint & !process, N, 0)
  out[, "support_gap_algebraic_adjustment"] <- ifelse(endpoint & !ledger_support, N,
    ifelse(endpoint & !process, N - W, 0))
  out[, "other_net_effect"] <- out[, "support_gap_algebraic_adjustment"]
  out[, "net_stock_benefit_on_ledger_support"] <- ifelse(ledger_support, N, NA_real_)
  out[, "unattributed_stock_benefit"] <- ifelse(endpoint & !ledger_support, N, 0)
  bp <- bb; ip <- ib
  for (j in seq_len(ny)) {
    bz <- state(b, plan$bau, j + 1L); iz <- state(i, plan$ics, j + 1L)
    rb <- .mofuss_luc_increment(bp, bz, is.finite(v(b$initial)))
    ri <- .mofuss_luc_increment(ip, iz, is.finite(v(i$initial)))
    bz$state_valid <- rb$valid; iz$state_valid <- ri$valid
    eb <- rb$previous_end; ei <- ri$previous_end
    sb <- rb$start; si <- ri$start
    if (j == 1L) {
      first_b_delta <- rb$delta
      first_b_harvest <- ifelse(is.finite(bz$b) & is.finite(bz$p), bz$b - bz$p,
        ifelse(is.finite(bz$h) & bz$h <= 0, 0, NA_real_))
    }
    valid <- process & is.finite(sb) & is.finite(si) & is.finite(eb) & is.finite(ei)
    if (any(process & !valid)) .mofuss_luc_stop("Unexpected invalid reconstructed state.")
    reset <- bz$tr %in% c(1, 2, 4) & plan$bau$mode == 3L
    allowance <- !reset & bz$tof == 1
    db <- sb - eb; di <- si - ei
    direct <- ifelse(reset, di - db, 0)
    tof <- ifelse(allowance, di - db, 0)
    remaining_reset <- ifelse(!reset & !allowance, di - db, 0)
    cb <- pmin(0, bz$b - sb); ci <- pmin(0, iz$b - si)
    clamp <- ci - cb
    changed <- is.finite(bz$lc) & is.finite(bp$lc) & bz$lc != bp$lc
    continuing <- !changed & is.finite(bz$lc) & is.finite(v(b$lcbase)) & bz$lc != v(b$lcbase)
    w <- rb$delta - ri$delta
    net <- (iz$p - bz$p) - (ip$p - bp$p)
    exposed <- ifelse(reset, pmax(0, ei - eb), 0)
    reversal <- ifelse(reset, pmax(0, pmax(0, ei - eb) - pmax(0, si - sb)), 0)
    residual <- net - w - direct - tof - clamp - remaining_reset
    # Independent single-scenario closure catches cancellation of equal errors.
    errb <- (bz$p - bp$p) + rb$delta - db - cb
    erri <- (iz$p - ip$p) + ri$delta - di - ci
    scale <- pmax(1, abs(bz$p), abs(bp$p), abs(iz$p), abs(ip$p), abs(eb), abs(ei))
    tol <- 8 * 2^-23 * scale + 1e-6
    err <- pmax(abs(errb), abs(erri), abs(residual))
    if (length(pidx)) max_error <- max(max_error, err[pidx])
    error_count <- error_count + sum(err[pidx] > tol[pidx])
    if (plan$bau$uncapped == 1L && any((cb[pidx] < -tol[pidx]) | (ci[pidx] < -tol[pidx]))) {
      q <- pidx[which((cb[pidx] < -tol[pidx]) | (ci[pidx] < -tol[pidx]))[1]]
      .mofuss_luc_stop("Uncapped preharvest decline contradicts supported v14 mechanics. year=",
        plan$years[j], " block_row=", plan$block_row, " block_cell=", q,
        " S_BAU=", sb[q], " S_ICS=", si[q], " B_BAU=", bz$b[q], " B_ICS=", iz$b[q],
        " class=", bz$lc[q], " TOF=", bz$tof[q], " transition=", bz$tr[q])
    }
    fields <- list(raw_signed_woodfuel_effect = w, net_stock_benefit = net,
      direct_luc_net_effect = direct, direct_luc_loss = pmax(0, -direct),
      direct_luc_gain = pmax(0, direct), tof_allowance_net_effect = tof,
      capacity_clamp_net_effect = clamp,
      capacity_class_changed_this_year = ifelse(changed, clamp, 0),
      capacity_class_changed_from_baseline = ifelse(continuing, clamp, 0),
      capacity_baseline_class = ifelse(!changed & !continuing, clamp, 0),
      other_net_effect = remaining_reset, closure_residual = residual,
      gross_positive_woodfuel_creation = pmax(0, w),
      luc_reversal_of_positive_gap = reversal,
      luc_transition_exposed_saved_stock = exposed,
      forest_to_tof_reversal = ifelse(bz$tr == 1 & bz$tof == 1, reversal, 0),
      direct_forest_clearing_net_effect = ifelse(bz$tr == 1, direct, 0),
      direct_new_forest_net_effect = ifelse(bz$tr == 2, direct, 0),
      direct_tof_loss_net_effect = ifelse(bz$tr == 4, direct, 0))
    for (key in names(fields)) {
      value <- fields[[key]]
      annual[j, key] <- sum(value[pidx])
      if (!key %in% c("raw_signed_woodfuel_effect", "net_stock_benefit"))
        out[pidx, key] <- out[pidx, key] + value[pidx]
    }
    hb[harvest_valid] <- hb[harvest_valid] + bz$h[harvest_valid]
    hi[harvest_valid] <- hi[harvest_valid] + iz$h[harvest_valid]
    bp <- bz; ip <- iz
  }
  if (error_count) .mofuss_luc_stop("Signed-ledger/process closure failed at ", error_count, " pixel-years.")
  nb <- pmax(0, pmin(hb, recovered_b)); ni <- pmax(0, pmin(hi, recovered_i))
  out[, "clipped_nrb_saving"] <- nb - ni
  out[!harvest_valid, "clipped_nrb_saving"] <- NA_real_
  out[, "bau_signed_depletion"] <- recovered_b
  out[, "bau_signed_preharvest"] <- recovered_b - first_b_delta + first_b_harvest
  out[, "ics_signed_depletion"] <- recovered_i
  out[, "bau_harvest"] <- hb; out[, "ics_harvest"] <- hi
  out[!harvest_valid, c("bau_harvest", "ics_harvest")] <- NA_real_
  out[!endpoint, ] <- NA_real_
  # Physical process maps are missing at incomplete-support cells. Their
  # aggregate totals use only complete support; the explicit boundary term
  # keeps full endpoint N/W reconciliation visible without inventing emissions.
  process_fields <- setdiff(.mofuss_luc_map_fields, c("raw_signed_woodfuel_effect",
    "net_stock_benefit", "opening_saved_stock", "other_net_effect", "clipped_nrb_saving",
    "support_gap_stock_benefit", "support_gap_algebraic_adjustment",
    "net_stock_benefit_on_ledger_support", "unattributed_stock_benefit",
    "bau_signed_depletion", "ics_signed_depletion", "bau_harvest", "ics_harvest",
    "bau_signed_preharvest"))
  out[endpoint & !process, process_fields] <- NA_real_
  list(values = out, annual = annual, endpoint_pixels = sum(endpoint),
       process_pixels = sum(process), nrb_pixels = sum(harvest_valid & ledger_support),
       ledger_pixels = sum(ledger_support),
       exported_ledger_pixels = sum(exported_support),
       recovered_ledger_pixels = sum(ledger_support & raw_missing),
       ledger_comparison_pixel_years = raw_compared, max_exported_ledger_error_mg = raw_max_error,
       max_closure_error_mg = max_error)
}

mofuss_luc_decompose_pair <- function(bau_dir, ics_dir, mc, start_year, end_year,
                                       output_dir, temp_dir = NULL,
                                       .write_fields = .mofuss_luc_map_fields, .model_support = FALSE) {
  plan <- mofuss_luc_pair_status(bau_dir, ics_dir, mc, start_year, end_year)
  if (!plan$available) .mofuss_luc_stop(plan$reason)
  plan$model_support <- isTRUE(.model_support)
  if (!is.null(output_dir)) {
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    for (run in c(plan$bau$run, plan$ics$run))
      if (startsWith(tolower(paste0(output_dir, "/")), tolower(paste0(run, "/"))))
        .mofuss_luc_stop("Process outputs must be outside canonical simulation folders.")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  old_temp <- terra::terraOptions(print = FALSE)$tempdir
  if (!is.null(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    terra::terraOptions(tempdir = temp_dir)
    on.exit(terra::terraOptions(tempdir = old_temp), add = TRUE)
  }
  input <- terra::rast(plan$paths); template <- input[[1]]
  terra::readStart(input); on.exit(terra::readStop(input), add = TRUE)
  raster_paths <- list(); writers <- list()
  if (!is.null(output_dir)) {
    for (key in .write_fields) {
      path <- file.path(output_dir, paste0(key, ".tif"))
      if (file.exists(path)) .mofuss_luc_stop("Refusing to overwrite process output: ", path)
      rr <- terra::rast(template); names(rr) <- key
      terra::writeStart(rr, path, overwrite = FALSE, datatype = "FLT8S", gdal = "COMPRESS=LZW")
      writers[[key]] <- rr; raster_paths[[key]] <- path
    }
    on.exit(for (rr in writers) try(terra::writeStop(rr), silent = TRUE), add = TRUE)
  }
  total <- setNames(numeric(length(.mofuss_luc_map_fields)), .mofuss_luc_map_fields)
  annual <- matrix(0, length(plan$years), length(total), dimnames = list(NULL, names(total)))
  counts <- c(endpoint = 0, process = 0, nrb = 0, ledger = 0,
              exported_ledger = 0, recovered_ledger = 0, ledger_compared = 0)
  max_error <- 0; max_exported_error <- 0
  # Bound the R matrix to about 32 MiB even for long horizons/large grids.
  rows <- max(1L, min(128L, floor(4e6 / (terra::ncol(input) * terra::nlyr(input)))))
  for (row in seq.int(1L, terra::nrow(input), by = rows)) {
    nr <- min(rows, terra::nrow(input) - row + 1L)
    plan$block_row <- row
    x <- terra::readValues(input, row = row, nrows = nr, mat = TRUE)
    result <- .mofuss_luc_block(x, plan)
    total <- total + colSums(result$values, na.rm = TRUE)
    annual <- annual + result$annual
    counts <- counts + c(result$endpoint_pixels, result$process_pixels, result$nrb_pixels,
      result$ledger_pixels, result$exported_ledger_pixels, result$recovered_ledger_pixels,
      result$ledger_comparison_pixel_years)
    max_error <- max(max_error, result$max_closure_error_mg)
    max_exported_error <- max(max_exported_error, result$max_exported_ledger_error_mg)
    for (key in names(writers)) terra::writeValues(writers[[key]], result$values[, key], row, nr)
  }
  if (!counts["endpoint"]) .mofuss_luc_stop("No common finite original-AGB endpoint support.")
  if (length(writers)) {
    for (key in names(writers)) terra::writeStop(writers[[key]])
    writers <- list()
  }
  reconciliation <- unname(total["net_stock_benefit"] - total["raw_signed_woodfuel_effect"] -
    total["direct_luc_net_effect"] - total["tof_allowance_net_effect"] -
    total["capacity_clamp_net_effect"] - total["other_net_effect"] - total["closure_residual"])
  if (abs(reconciliation) > 1e-6 * max(1, sum(abs(total))))
    .mofuss_luc_stop("Aggregate process accounting failed to reconcile.")
  summary <- as.data.frame(as.list(setNames(total, paste0(names(total), "_mg"))))
  summary$reversal_fraction_denominator_mg <- summary$luc_transition_exposed_saved_stock_mg
  summary$luc_reversal_fraction <- if (summary$reversal_fraction_denominator_mg > 0)
    summary$luc_reversal_of_positive_gap_mg / summary$reversal_fraction_denominator_mg else NA_real_
  summary$endpoint_support_pixels <- unname(counts["endpoint"])
  summary$process_support_pixels <- unname(counts["process"])
  summary$nrb_support_pixels <- unname(counts["nrb"])
  summary$complete_ledger_support_pixels <- unname(counts["ledger"])
  summary$ledger_support_fraction <- summary$complete_ledger_support_pixels / summary$endpoint_support_pixels
  summary$exported_ledger_support_pixels <- unname(counts["exported_ledger"])
  summary$recovered_exported_ledger_gap_pixels <- unname(counts["recovered_ledger"])
  summary$ledger_validation_pixel_years <- unname(counts["ledger_compared"])
  summary$max_exported_ledger_increment_error_mg <- max_exported_error
  summary$process_support_fraction <- summary$process_support_pixels / summary$endpoint_support_pixels
  summary$max_pixel_year_closure_error_mg <- max_error
  summary$aggregate_reconciliation_error_mg <- reconciliation
  summary$mc <- mc; summary$start_year <- start_year; summary$end_year <- end_year
  summary$luc_mode <- plan$bau$mode; summary$uncapped_regrowth <- plan$bau$uncapped
  summary$woodman_luc_freeze_year <- plan$bau$freeze_year
  summary$woodman_luc_freeze_contract <- plan$bau$freeze_contract
  summary$method <- MOFUSS_LUC_DECOMPOSITION_CONTRACT
  summary$status <- if (counts["process"] == counts["endpoint"]) "complete_annual_support" else
    "partial_annual_support_quality_terms_not_emissions"
  annual <- data.frame(year = plan$years, annual, check.names = FALSE)
  names(annual)[-1] <- paste0(names(annual)[-1], "_mg")
  annual$process_support_pixels <- unname(counts["process"])
  annual$mc <- mc
  annual$land_cover_year <- if (plan$bau$mode == 3L)
    pmin(plan$years, plan$bau$freeze_year) else NA_integer_
  annual$woodman_transitions_enabled <- plan$bau$mode == 3L &
    plan$years <= plan$bau$freeze_year
  # Annual clipped NRB is intentionally not reported: summing annual clips is
  # not the clipped-period diagnostic. Opening/support terms are period-only.
  annual <- annual[, !names(annual) %in% paste0(c("clipped_nrb_saving", "opening_saved_stock",
    "support_gap_stock_benefit", "support_gap_algebraic_adjustment",
    "net_stock_benefit_on_ledger_support", "unattributed_stock_benefit",
    "bau_signed_depletion", "ics_signed_depletion", "bau_harvest", "ics_harvest",
    "bau_signed_preharvest"), "_mg"), drop = FALSE]
  metadata <- list(method = MOFUSS_LUC_DECOMPOSITION_CONTRACT,
    luc_mode = plan$bau$mode, woodman_luc_freeze_year = plan$bau$freeze_year,
    woodman_luc_freeze_contract = plan$bau$freeze_contract,
    woodman_luc_freeze_evidence = c(BAU = plan$bau$freeze_evidence, ICS = plan$ics$freeze_evidence),
    woodman_luc_freeze_policy = "LUC3 only: annual history through freeze year inclusive; reuse that cover and TOF afterward, with zero new transitions; biomass and calendar continue",
    units = "Mg_dry_biomass_per_original_grid_cell_no_area_multiplier",
    biomass_support = if (isTRUE(plan$model_support)) "finite_initial_model_stock_support" else
      "finite_original_agb3_c_and_finite_paired_postharvest_endpoints",
    process_support = "endpoint_support_with_complete_valid_annual_state_in_both_scenarios",
    baseline = "end_of_start_minus_one_postharvest; model initial state at first model year",
    closure = "net_stock = signed_woodfuel + direct_LUC_reset + TOF_allowance + negative_preharvest_adjustment + unclassified_support_adjustment + float32_precision_residual",
    direct_luc_definition = "Woodman reset codes 1 forest cleared, 2 new forest, 4 TOF lost; signed ICS reset minus BAU reset",
    reversal_fraction_definition = "sum loss of positive ICS-minus-BAU endpoint gap at direct reset / sum positive gap exposed immediately before those reset events",
    reversal_fraction_is_cohort_survival = FALSE,
    capacity_classification = "association with class change this year / continuing difference from baseline / baseline class; not a no-LUC causal counterfactual",
    tof_note = "Nondegradable renewable availability allowance; not a conserved standing-stock carbon pool or additional sequestration",
    support_gap_note = "Unattributable endpoint stock and residual are quality diagnostics, never ecological emissions",
    seed_note = "Existing 2-Mg forest seed is already included in signed woodfuel ledger; no extra correction",
    raw_signed_woodfuel_note = "Signed depletion/regrowth balance under realized LUC, not clipped NRB and not a no-LUC simulation",
    signed_reconstruction = "double-precision sum[min(post-LUC start,preharvest stock)-postharvest stock-previous seed]; zero-harvest invalid-state ledger carry preserved; validates every readable exported float32 increment",
    recovery_reason = "Original signed-ledger NoData=-9999 can collide with legitimate negative balances or GDAL approximate NoData matching; reconstruction also recovers subsequent propagated collision history without altering outputs",
    before_direct_luc_note = "net_stock_benefit - direct_luc_net_effect is an accounting add-back, not an alternative simulated landscape",
    input_files = plan$paths, model_files = c(plan$bau$model, plan$ics$model),
    auxiliary_files = plan$auxiliary_paths,
    auxiliary_md5 = as.list(tools::md5sum(plan$auxiliary_paths)),
    model_md5 = unname(tools::md5sum(c(plan$bau$model, plan$ics$model))))
  paths <- list(rasters = raster_paths)
  if (!is.null(output_dir)) {
    paths$summary <- file.path(output_dir, "process_summary.csv")
    paths$annual <- file.path(output_dir, "process_annual.csv")
    paths$metadata <- file.path(output_dir, "process_metadata.json")
    utils::write.csv(summary, paths$summary, row.names = FALSE)
    utils::write.csv(annual, paths$annual, row.names = FALSE)
    jsonlite::write_json(metadata, paths$metadata, auto_unbox = TRUE, pretty = TRUE, na = "null")
  }
  list(summary = summary, annual = annual, paths = paths, metadata = metadata)
}

# Shared NRB recovery API. Returned file-backed terra rasters remain valid for
# the caller; their temporary directory is reported in diagnostics for cleanup
# after raster consumers finish. The support is immutable MODEL initial stock,
# deliberately wider than the emissions pipeline's raw AGB reference footprint.
mofuss_reconstruct_signed_period <- function(run_dir, mc, start_step, end_step,
                                              baseline = c("preharvest", "previous_postharvest"),
                                              temp_dir = NULL) {
  baseline <- match.arg(baseline)
  ctx <- .mofuss_luc_context(run_dir, mc)
  if (!ctx$available) .mofuss_luc_stop("Signed reconstruction requires corrected v14.")
  if (length(start_step) != 1L || length(end_step) != 1L ||
      any(!is.finite(c(start_step, end_step))) ||
      any(c(start_step, end_step) != as.integer(c(start_step, end_step))) ||
      start_step < 1L || end_step < start_step || end_step > ctx$last - ctx$first + 1L)
    .mofuss_luc_stop("Invalid signed reconstruction period.")
  if (is.null(temp_dir)) temp_dir <- terra::terraOptions(print = FALSE)$tempdir
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  output <- tempfile("woodfuel_signed_reconstruction_", tmpdir = temp_dir)
  field <- if (baseline == "preharvest") "bau_signed_preharvest" else "bau_signed_depletion"
  result <- mofuss_luc_decompose_pair(run_dir, run_dir, mc,
    ctx$first + start_step - 1L, ctx$first + end_step - 1L,
    output_dir = output, temp_dir = temp_dir, .write_fields = c(field, "bau_harvest"),
    .model_support = TRUE)
  list(signed = terra::rast(result$paths$rasters[[field]]),
       harvest = terra::rast(result$paths$rasters$bau_harvest),
       diagnostics = list(method = "physical_signed_period_reconstruction_v1",
         baseline = baseline, start_step = start_step, end_step = end_step,
         temporary_output_dir = output, summary = result$summary,
         metadata = result$metadata))
}
