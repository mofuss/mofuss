# Exercise the current Stage 3 -> Stage 4 table handoff and Stage 4 estimand
# gates. This deliberately runs actual production table-building expressions,
# without rendering maps or creating/replacing a manuscript output package.
post <- file.path(getwd(), "localhost/scripts/postprocessing_emissions")
MOFUSS_CONFIG_ONLY <- TRUE
s3 <- new.env(parent = globalenv())
s3$MOFUSS_CONFIG_ONLY <- TRUE
sys.source(file.path(post, "3post_agb_decomposition_v6.R"), envir = s3)
code <- parse(file.path(post, "4post_manuscript_outputs_v3.R"))
assigned <- function(expr, name) is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
  identical(expr[[2L]], as.name(name))
index <- function(name) {
  found <- which(vapply(code, assigned, logical(1), name = name))
  stopifnot(length(found) == 1L)
  found
}
scratch <- Sys.getenv("MOFUSS_TEST_SCRATCH", "E:/MoFuSS_Active/MDG_NRB_attribution_fix_2026-10-07/emissions")
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
fixture <- tempfile("stage4_contract_", tmpdir = scratch)
dir.create(fixture)
env <- new.env(parent = globalenv())
for (name in c("stopf", "require_columns", "read_csv_required", "same_number",
               "BIOMASS_ESTIMAND", "CONFIGURATION_ORDER", "COMPONENT_LABELS")) {
  eval(code[[index(name)]], env)
}
env$period_end <- 2050L
env$reporting_years <- 25L
env$uncertainty_adequate <- FALSE
env$display_labels <- c(capped = "Capped", uncapped = "Uncapped")
rows <- data.frame(
  display_label = c("Capped", "Uncapped"), regrowth_mode = c("capped", "uncapped"),
  run_id = 1L, period_start_year = 2026L, period_end_year = 2050L, baseline_year = 2025L,
  bau_end_agb_mg = c(70, 80), ics_end_agb_mg = c(90, 120), baseline_delta_agb_mg = c(2, 3),
  end_delta_agb_mg = c(20, 40), period_delta_agb_mg = c(18, 37),
  period_avoided_loss_mg = c(10, 20), period_regrowth_mg = c(8, 17),
  period_avoided_loss_tco2e = c(17.2333333333, 34.4666666667),
  period_regrowth_tco2e = c(13.7866666667, 29.2966666667),
  agb_avoided_stage2_tco2e = c(31.02, 63.7633333333),
  enduse_avoided_tco2e = 15, total_avoided_tco2e = c(46.02, 78.7633333333),
  n_decomposition_period_common = 6L,
  biomass_estimand = "net_retained_stock_under_prescribed_luc_v1"
)
env$per_run <- rows
env$country_per_run <- rows
table <- s3$make_comparison_table(rows)
env$agb_mc1_path <- file.path(fixture, "comparison_table.csv")
write.csv(table, env$agb_mc1_path, row.names = FALSE)
table_code <- code[index("metric_fields"):(index("footnotes") - 1L)]
for (expr in table_code) eval(expr, env)
stopifnot(env$mc1_table$Capped[env$mc1_table$Metric == "Net biomass carbon benefit - Stage 2 (tCO2e/period)"] == 31,
          env$mc1_table$Uncapped[env$mc1_table$Metric == "Stock-gain component (Mg/period)"] == 17,
          identical(unname(env$COMPONENT_LABELS[["harvest"]]), "Net biomass carbon"),
          all(env$source_mc1_labels[!is.na(env$source_mc1_labels)] %in% table$Metric))
expect_error <- function(expr, pattern) {
  err <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}
# Changing an upstream Stage 3 value must still be caught at the handoff.
bad_table <- table
bad_table$Capped[bad_table$Metric == "Net biomass carbon - stage 2 (tCO2e)"] <- 99
write.csv(bad_table, env$agb_mc1_path, row.names = FALSE)
expect_error(for (expr in table_code) eval(expr, env), "does not reconcile")

gate_code <- code[vapply(code, function(expr) {
  if (!is.call(expr) || !(identical(expr[[1L]], as.name("require_columns")) ||
                         identical(expr[[1L]], as.name("if")))) return(FALSE)
  symbols <- all.names(expr)
  "biomass_estimand" %in% symbols ||
    (identical(expr[[1L]], as.name("require_columns")) &&
       length(expr) >= 3L && identical(expr[[3L]], "biomass_estimand"))
}, logical(1))]
stopifnot(length(gate_code) == 4L)
for (expr in gate_code) eval(expr, env)
env$per_run$biomass_estimand <- "harvest_only_nrb"
expect_error(for (expr in gate_code) eval(expr, env), "biomass estimand")
env$per_run <- rows
env$country_per_run$biomass_estimand <- "harvest_only_nrb"
expect_error(for (expr in gate_code) eval(expr, env), "estimands disagree")
env$country_per_run <- rows
env$per_run$biomass_estimand <- NULL
expect_error(for (expr in gate_code) eval(expr, env), "biomass_estimand")
cat("PASS: current Stage 3/4 table labels and values reconcile; altered values and wrong/missing regional/country estimands are rejected.\n")
