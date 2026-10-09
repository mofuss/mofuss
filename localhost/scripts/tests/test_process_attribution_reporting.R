# Exercise the actual Stage 3/4 process handoff without reading a country run
# or replacing production outputs. The loss is already inside the net result.
post <- file.path(getwd(), "localhost/scripts/postprocessing_emissions")
MOFUSS_CONFIG_ONLY <- TRUE
s3 <- new.env(parent = globalenv())
sys.source(file.path(post, "3post_agb_decomposition_v6.R"), envir = s3)
code <- parse(file.path(post, "4post_manuscript_outputs_v3.R"))
env <- new.env(parent = globalenv())
for (name in c("stopf", "require_columns", "v3_process_products", "v3_write_process_figure",
               "write_table_png", "TABLE_PNG_DPI", "TABLE_PNG_WIDTH_IN")) {
  found <- which(vapply(code, function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
    identical(x[[2L]], as.name(name)), logical(1)))
  stopifnot(length(found) == 1L)
  eval(code[[found]], env)
}
labels <- c(capped = "Capped", uncapped = "Uncapped")
identity_row <- s3$v6_process_identity(list(label = "MDG_capped", regrowth_mode = "capped",
  ics_params = list(scenario_ver = "ICS3_v2"), reference_md5 = paste(rep("a", 32L), collapse = "")),
  run_id = 1L, period = list(start = 2026L, end = 2050L))
stopifnot(nrow(identity_row) == 1L, identity_row$display_label == "capped_ics3_v2")
stock <- data.frame(regrowth_mode = names(labels), run_id = 1L,
                    period_start_year = 2026L, period_end_year = 2050L,
                    period_delta_agb_mg = c(20, 10),
                    agb_reference_md5 = paste(rep("a", 32L), collapse = ""))
process <- transform(stock, process_attribution_status = "available",
  process_attribution_method = "paired_signed_woodfuel_direct_luc_v1",
  raw_signed_woodfuel_effect_mg = c(50, 10), net_stock_benefit_mg = c(20, 10),
  direct_luc_net_effect_mg = c(-30, 0), direct_luc_loss_mg = c(30, 0), direct_luc_gain_mg = 0,
  tof_allowance_net_effect_mg = 0, capacity_clamp_net_effect_mg = 0, other_net_effect_mg = 0,
  clipped_nrb_saving_mg = c(17, 5), luc_reversal_of_positive_gap_mg = c(30, 0),
  luc_transition_exposed_saved_stock_mg = c(40, 0), luc_reversal_fraction = c(0.75, NA),
  closure_residual_mg = 0, endpoint_support_pixels = 100L, process_support_pixels = 100L,
  support_gap_stock_benefit_mg = 0, unattributed_stock_benefit_mg = 0,
  complete_ledger_support_pixels = 100L, stage2_reconciliation_ok = TRUE)
validated <- s3$v6_validate_process_summary(process[1, ], 20)
stopifnot(validated$stage2_reconciliation_ok,
          validated$stage2_reconciliation_residual_mg == 0,
          validated$net_before_direct_luc_mg == 50)
result <- env$v3_process_products(process, stock, labels)
net <- result$mc1_table$Metric == "Net retained AGB benefit - Stage 2 (Mg)"
frac <- result$mc1_table$Metric == "Exposed saved AGB reversed (%)"
co2_before <- result$mc1_table$Metric == "Before direct LUC - stock equivalent (tCO2e)"
co2_net <- result$mc1_table$Metric == "Net retained stock equivalent (tCO2e)"
stopifnot(result$any_available, as.numeric(result$mc1_table$Capped[net]) == 20,
          as.numeric(result$mc1_table$Capped[frac]) == 75,
          abs(as.numeric(result$mc1_table$Capped[co2_before]) - 50 * 0.47 * 44 / 12) < 1e-7,
          abs(as.numeric(result$mc1_table$Capped[co2_net]) - 20 * 0.47 * 44 / 12) < 1e-7,
          result$mc1_table$Uncapped[frac] == "Undefined")
# Neither the 30 Mg loss nor the distinct 17 Mg clipped NRB metric changes net.
stopifnot(process$net_stock_benefit_mg[[1]] == 20,
          process$clipped_nrb_saving_mg[[1]] != process$raw_signed_woodfuel_effect_mg[[1]])
legacy <- env$v3_process_products(NULL, stock, labels)
stopifnot(!legacy$any_available,
          all(legacy$rows$process_attribution_status == "unavailable_legacy_stage3"))
mixed <- process
mixed$process_attribution_status[[2L]] <- "legacy_unavailable"
stopifnot(env$v3_process_products(mixed, stock, labels)$any_available)
partial <- process
partial$process_support_pixels[[1L]] <- 99L
partial$support_gap_stock_benefit_mg[[1L]] <- 1
partial$other_net_effect_mg[[1L]] <- 1
partial$net_stock_benefit_mg[[1L]] <- 21
partial_stock <- stock; partial_stock$period_delta_agb_mg[[1L]] <- 21
stopifnot(env$v3_process_products(partial, partial_stock, labels)$any_available)
expect_error <- function(expr, pattern) {
  error <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
}
expect_error(s3$v6_validate_process_summary(process[1, ], 19), "differs from Stage 2")
bad <- process; bad$net_stock_benefit_mg[[1]] <- 19
expect_error(env$v3_process_products(bad, stock, labels), "does not reconcile")
bad <- process; bad$direct_luc_net_effect_mg[[1]] <- -29
expect_error(env$v3_process_products(bad, stock, labels), "do not close")
bad <- process; bad$luc_reversal_fraction[[1]] <- 0.7
expect_error(env$v3_process_products(bad, stock, labels), "reversal fraction")
bad <- process; bad$luc_reversal_fraction[[2]] <- 0
expect_error(env$v3_process_products(bad, stock, labels), "reversal fraction")
bad <- process; bad$process_attribution_status[[1]] <- "available_preflight_only"
expect_error(env$v3_process_products(bad, stock, labels), "incompatible status")
expect_error(env$v3_process_products(rbind(process, process[1, ]), stock, labels), "keys")
bad <- process; bad$clipped_nrb_saving_mg <- NULL
expect_error(env$v3_process_products(bad, stock, labels), "missing columns")
bad <- process; bad$process_attribution_method <- "stock_difference_only"
expect_error(env$v3_process_products(bad, stock, labels), "method is incompatible")
bad <- process; bad$agb_reference_md5 <- paste(rep("b", 32L), collapse = "")
expect_error(env$v3_process_products(bad, stock, labels), "reference differs")
scratch <- Sys.getenv("MOFUSS_TEST_SCRATCH", "E:/MoFuSS_Active/MDG_LUC_attribution_postprocessing_2026-10-08/tests")
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
fixture <- tempfile("process_reporting_", tmpdir = scratch)
dir.create(fixture)
env$v3_write_process_figure(process, file.path(fixture, "process_figure.png"), labels,
                             "Biomass process account: accounting fixture")
env$write_table_png(result$mc1_table, file.path(fixture, "process_table.png"),
                    "Biomass process account: accounting fixture", "MC1; signed components and LUC reversal")
stopifnot(all(file.info(file.path(fixture, c("process_figure.png", "process_table.png")))$size > 1000))
cat("PROCESS_REPORTING_FIXTURE=", normalizePath(fixture, winslash = "/"), "\n", sep = "")
cat("PASS: process/stock closure, no double subtraction, separate clipped NRB, legacy availability, event denominator and invalid handoffs.\n")
