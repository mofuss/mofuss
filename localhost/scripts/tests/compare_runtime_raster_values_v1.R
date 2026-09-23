# Quantify pixel-level differences in already isolated regression outputs.
suppressPackageStartupMessages(library(terra))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
root <- "E:/MoFuSS_Active/ecsa_sourcing_and_speed_v1/runtime_regression"
paths <- file.path(root, args)
stopifnot(all(dir.exists(paths)))
files <- list.files(paths[1], pattern = "^Ex_agr_harv[0-9]+[.]tif$", recursive = TRUE)
result <- do.call(rbind, lapply(files, function(file) {
  a <- rast(file.path(paths[1], file)); b <- rast(file.path(paths[2], file))
  stopifnot(compareGeom(a, b, stopOnError = FALSE))
  av <- values(a, mat = FALSE); bv <- values(b, mat = FALSE)
  common <- is.finite(av) & is.finite(bv)
  delta <- bv[common] - av[common]
  data.frame(file = file, common_finite_cells = sum(common),
    changed_finite_cells = sum(delta != 0),
    null_pattern_different_cells = sum(is.na(av) != is.na(bv)),
    baseline_null_candidate_zero = sum(is.na(av) & !is.na(bv) & bv == 0),
    candidate_null_baseline_zero = sum(is.na(bv) & !is.na(av) & av == 0),
    max_abs_delta = max(abs(delta), 0), sum_delta = sum(delta),
    baseline_sum = sum(av, na.rm = TRUE), candidate_sum = sum(bv, na.rm = TRUE))
}))
write.csv(result, file.path(root, paste0("pixel_comparison_", args[1], "_vs_", args[2], ".csv")), row.names = FALSE)
print(result, row.names = FALSE)
