# Regression tests for demand_in cleanup and administrative index deployment.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)

load_definition <- function(script_name, definition_name) {
  expressions <- parse(file = file.path(
    repository_root,
    "localhost",
    "scripts",
    script_name
  ))
  matches <- which(vapply(expressions, function(expression) {
    is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      identical(as.character(expression[[2L]]), definition_name)
  }, logical(1)))
  stopifnot(length(matches) == 1L)
  eval(expressions[[matches]], envir = .GlobalEnv)
}

load_definition("1_erase_all_v1.R", ".clean_demand_input_preserving_wp")
load_definition("2_copy_files_v4.R", ".copy_admin_region_indexes")

fixture <- tempfile("demand_input_refresh_")
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)
demand_input_dir <- file.path(fixture, "working", "demand", "demand_in")
dir.create(file.path(demand_input_dir, "obsolete_folder"), recursive = TRUE)

writeLines("preserve raster", file.path(demand_input_dir, "wp_global.tif"))
writeLines("preserve table", file.path(demand_input_dir, "wp_metadata.csv"))
writeLines("remove demand", file.path(demand_input_dir, "demand_old.csv"))
writeLines("remove region", file.path(demand_input_dir, "mofuss_regions0.gpkg"))
writeLines(
  "remove nested file",
  file.path(demand_input_dir, "obsolete_folder", "contents.txt")
)

.clean_demand_input_preserving_wp(demand_input_dir)
stopifnot(
  file.exists(file.path(demand_input_dir, "wp_global.tif")),
  file.exists(file.path(demand_input_dir, "wp_metadata.csv")),
  !file.exists(file.path(demand_input_dir, "demand_old.csv")),
  !file.exists(file.path(demand_input_dir, "mofuss_regions0.gpkg")),
  !dir.exists(file.path(demand_input_dir, "obsolete_folder"))
)

admin_dir <- file.path(fixture, "admin_regions")
for (level in 0:2) {
  level_dir <- file.path(admin_dir, paste0("regions_adm", level))
  dir.create(level_dir, recursive = TRUE)
  writeLines(
    paste("admin level", level),
    file.path(level_dir, paste0("mofuss_regions", level, ".gpkg"))
  )
}

copied <- .copy_admin_region_indexes(admin_dir, demand_input_dir)
expected <- file.path(
  demand_input_dir,
  paste0("mofuss_regions", 0:2, ".gpkg")
)
stopifnot(
  identical(copied, expected),
  all(file.exists(expected)),
  identical(
    unname(vapply(expected, readLines, character(1))),
    paste("admin level", 0:2)
  ),
  file.exists(file.path(demand_input_dir, "wp_global.tif"))
)

unlink(file.path(admin_dir, "regions_adm2", "mofuss_regions2.gpkg"))
missing_source_error <- tryCatch(
  {
    .copy_admin_region_indexes(admin_dir, demand_input_dir)
    NA_character_
  },
  error = conditionMessage
)
stopifnot(
  !is.na(missing_source_error),
  grepl("Missing required", missing_source_error, fixed = TRUE)
)

cat("DEMAND_INPUT_REFRESH_OK\n")
