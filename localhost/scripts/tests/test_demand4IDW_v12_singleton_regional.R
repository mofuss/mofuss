# Regression test for Regional configurations whose selected region contains
# exactly one country. The configured scope remains Regional, but demand
# preparation must take the Country path and skip directional HC jobs.

repository_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
script <- file.path(
  repository_root,
  "localhost",
  "scripts",
  "3_demand4IDW_v12.R"
)
expressions <- parse(file = script)

resolver_expression <- NULL
for (expression in expressions) {
  if (
    is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.name(expression[[2L]]) &&
      identical(
        as.character(expression[[2L]]),
        ".resolve_effective_demand_scope"
      )
  ) {
    resolver_expression <- expression
    break
  }
}
stopifnot(!is.null(resolver_expression))
eval(resolver_expression, envir = .GlobalEnv)

directional_expression <- NULL
for (expression in expressions) {
  if (is.call(expression) && identical(expression[[1L]], as.name("if"))) {
    condition_text <- paste(deparse(expression[[2L]]), collapse = " ")
    if (
      grepl(
        'identical(effective_byregion, "Regional")',
        condition_text,
        fixed = TRUE
      )
    ) {
      directional_expression <- expression
      break
    }
  }
}
stopifnot(!is.null(directional_expression))

regions0 <- data.frame(
  GID_0 = c("AAA", "BBB", "CCC"),
  mofuss_reg = c(
    "SSA_adm0_MULTI", "SSA_adm0_MULTI", "SSA_adm0_SINGLE"
  ),
  stringsAsFactors = FALSE
)

singleton <- .resolve_effective_demand_scope(
  byregion = "Regional",
  aoi_poly = "0",
  mofuss_region = "SSA_adm0_SINGLE",
  regions0 = regions0
)
stopifnot(
  identical(singleton$effective_byregion, "Country"),
  identical(singleton$mofuss_region, "CCC"),
  isTRUE(singleton$singleton_regional),
  identical(singleton$country_ids, "CCC")
)

# The directional block itself must be a no-op for the resolved singleton.
effective_byregion <- singleton$effective_byregion
aoi_poly <- 0L
directional_hc_jobs_created <- FALSE
eval(directional_expression, envir = .GlobalEnv)
stopifnot(!directional_hc_jobs_created)

configured_urb_shift_factor <- 1.25
singleton_urb_shift_factor <- configured_urb_shift_factor
if (!identical(singleton$effective_byregion, "Country")) {
  singleton_urb_shift_factor <- 1
}
stopifnot(identical(singleton_urb_shift_factor, configured_urb_shift_factor))

multi_country <- .resolve_effective_demand_scope(
  byregion = "Regional",
  aoi_poly = 0L,
  mofuss_region = "SSA_adm0_MULTI",
  regions0 = regions0
)
stopifnot(
  identical(multi_country$effective_byregion, "Regional"),
  identical(multi_country$mofuss_region, "SSA_adm0_MULTI"),
  !multi_country$singleton_regional,
  identical(multi_country$country_ids, c("AAA", "BBB"))
)

country <- .resolve_effective_demand_scope(
  byregion = "Country",
  aoi_poly = 0L,
  mofuss_region = "AAA",
  regions0 = regions0
)
stopifnot(
  identical(country$effective_byregion, "Country"),
  identical(country$mofuss_region, "AAA"),
  !country$singleton_regional
)

aoi <- .resolve_effective_demand_scope(
  byregion = "Regional",
  aoi_poly = 1L,
  mofuss_region = "SSA_adm0_SINGLE",
  regions0 = regions0
)
stopifnot(
  identical(aoi$effective_byregion, "Regional"),
  identical(aoi$mofuss_region, "SSA_adm0_SINGLE"),
  !aoi$singleton_regional
)

script_text <- paste(readLines(script, warn = FALSE), collapse = "\n")
stopifnot(
  grepl(
    'identical(effective_byregion, "Regional")',
    script_text,
    fixed = TRUE
  ),
  grepl(
    'effective_byregion == "Country" & aoi_poly == 0',
    script_text,
    fixed = TRUE
  ),
  grepl(
    "region2BprocessedCtry_iso <- mofuss_region",
    script_text,
    fixed = TRUE
  ),
  !identical(singleton$effective_byregion, "Regional")
)

cat("DEMAND4IDW_V12_SINGLETON_REGIONAL_OK\n")
