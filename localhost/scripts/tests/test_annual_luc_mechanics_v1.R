# Regression checks for the independent annual-LUC raster replay.
# Scalar reference values below were also checked with native Windows Dinamica.
args <- commandArgs()
script <- normalizePath(sub("^--file=", "", args[startsWith(args, "--file=")][[1L]]),
                        winslash = "/", mustWork = TRUE)
source(file.path(dirname(script), "..", "..", "..", "calib_valid_agb",
                 "verify_annual_luc_mechanics.R"))

same <- function(x, y, tolerance = 1e-6) {
  stopifnot(identical(is.finite(x), is.finite(y)))
  z <- is.finite(x)
  stopifnot(all(abs(x[z] - y[z]) <= tolerance))
}

prior <- c(NA_real_, 0, 2, 100, NA_real_)
prior_luc <- c(2, 2, 2, 2, NA_real_)
same(annual_start_state(prior, prior_luc, rep(2, 5), rep(0, 5),
                        rep(500, 5), rep(0, 5), "corrected"),
     c(NA_real_, 0, 2, 100, 0))
same(annual_start_state(prior, prior_luc, rep(2, 5), rep(0, 5),
                        rep(500, 5), rep(0, 5), "legacy"), c(0, 0, 2, 100, 0))
same(annual_capacity(rep(2, 4), rep(2, 4), c(NA_real_, 0, 2, 100),
                     rep(500, 4), "corrected"), c(NA_real_, 0, 2, 100))
same(annual_capacity(c(2, 3, 2), rep(2, 3), rep(0, 3),
                     c(500, 700, 500), "corrected"), c(0, 700, 0))

# Numeric zero with calibrated K=0 never gains available stock from the seed.
zero_growth <- annual_growth(0, 0, 0, .1, NULL, 0, "corrected")
same(zero_growth, 0)
same(annual_feedback(zero_growth, 0, 0), 2)
same(annual_growth(2, 0, 0, .1, NULL, 0, "corrected"), 0)
same(annual_growth(2, 0, 100, .1, NULL, 0, "corrected"), 2.196)

# Actual uncapped Chapman-Richards grows from numeric zero, preserves NoData,
# and must not produce biomass/harvest during a conversion year.
cr <- list(A = c(100, 100), k = c(.05, .05), m = c(2, 2))
same(annual_growth(c(0, NA_real_), c(0, 0), c(100, 100), c(.1, .1),
                   cr, c(0, 0), "corrected"), c(.2378569, NA_real_))
for (transition in c(1, 2, 4)) {
  same(annual_growth(c(0, 0), c(0, 0), c(100, 100), c(0, 0), cr,
                     rep(transition, 2), "corrected"), c(0, 0))
  same(annual_feedback(c(0, 0), c(0, 0), rep(transition, 2)), c(0, 0))
  # Conversion-year zero survives the final clamp even if baseline K is NULL.
  same(annual_growth(0, 0, NA_real_, 0, NULL, transition, "corrected"), 0)
}
same(annual_growth(c(0, 0), c(0, 0), c(100, 100), c(0, 0), cr,
                   c(2, 4), "legacy"), rep(.2378569, 2))

# The fixed domain follows v13 INITIAL MODEL stock, not raw CTrees coverage.
# Exercise every transition: NULL initial non-TOF cannot gain a TOF allowance
# or conversion-year zero; numeric zero and initialized TOF remain eligible.
for (transition in 0:4) {
  initial <- c(NA_real_, 0, 100)
  d <- annual_model_domain(rep(3, 3), rep(1, 3), initial, "fixed_initial")
  same(d$luc, c(NA_real_, 3, 3))
  same(d$tof, c(NA_real_, 1, 1))
  tr <- ifelse(is.finite(d$luc), transition, NA_real_)
  start <- annual_start_state(rep(NA_real_, 3), rep(NA_real_, 3),
    d$luc, d$tof, c(NA_real_, 500, 500), tr, "fixed_initial")
  expected <- if (transition %in% c(1, 2, 4)) 0 else 500
  same(start, c(NA_real_, expected, expected))
  grown <- annual_growth(start, d$tof, rep(500, 3), rep(.1, 3), NULL,
    tr, "fixed_initial")
  same(grown, c(NA_real_, expected, expected))
  same(annual_feedback(grown, d$tof, tr), c(NA_real_, expected, expected))
}
# A finite initialized cell with missing CR parameters retains the existing
# NoData growth behavior; the eligibility rule does not fill those parameters.
d <- annual_model_domain(2, 0, 50, "fixed_initial")
same(annual_growth(50, d$tof, 100, .1,
  list(A = NA_real_, k = .05, m = 2), 0, "fixed_initial"), NA_real_)
same(annual_model_domain(2, 0, NA_real_, "corrected")$luc, 2)
for (invalid in list(list(rules = "typo"), list(rules = NA_character_),
                     list(mc_ids = c(1, 1)), list(mc_ids = 0),
                     list(mc_ids = 1.5), list(mc_ids = NA_integer_),
                     list(last_year = NA_integer_), list(last_year = 2050.5),
                     list(tol = -1), list(tol = Inf))) {
  failure <- tryCatch(do.call(run_audit, c(list(run = "unused", output = "unused"),
                                          invalid)), error = identity)
  stopifnot(inherits(failure, "error"), !grepl("path", conditionMessage(failure)))
}
cat("Annual LUC replay regression checks passed.\n")
