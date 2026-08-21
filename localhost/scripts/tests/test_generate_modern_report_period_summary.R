# Verify that the V8 report consumes the current period-level *_fr table. All
# generated files are confined to a temporary fixture.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
test_args <- commandArgs(trailingOnly = TRUE)
report_name <- if (length(test_args)) {
  test_args[[1L]]
} else {
  "generate_modern_report.R"
}
report_script <- file.path(
  repo_root, "localhost", "scripts", "LaTeX", report_name
)
latex_dir <- dirname(report_script)
source_root <- "D:/ken_1km_bau1_2030_v3_ng"
stopifnot(file.exists(report_script), dir.exists(source_root))

fixture <- tempfile("modern_report_period_summary_")
dir.create(file.path(fixture, "LULCC", "TempTables"), recursive = TRUE)
dir.create(file.path(fixture, "Out", "webmofuss_results"), recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

for (name in c("Country.csv", "InputPara.csv")) {
  stopifnot(file.copy(
    file.path(source_root, "LULCC", "TempTables", name),
    file.path(fixture, "LULCC", "TempTables", name)
  ))
}
period_table <- file.path(
  source_root, "Out", "webmofuss_results", "summary_adm0_fr.csv"
)
stopifnot(file.exists(period_table))
stopifnot(file.copy(
  period_table,
  file.path(fixture, "Out", "webmofuss_results", "summary_adm0_fr.csv")
))

source(report_script)
pdf <- generate_modern_report(
  base_dir = fixture,
  latex_dir = latex_dir,
  output_dir = "Out",
  mc_threshold = 30
)
stopifnot(file.exists(pdf), file.info(pdf)$size > 0)
cat("MODERN_REPORT_PERIOD_SUMMARY_FIXTURE_OK:", report_name, "\n")
