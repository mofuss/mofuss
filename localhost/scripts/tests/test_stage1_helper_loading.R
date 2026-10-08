# A sourced config-only Stage 1 can be invoked later from a different cwd.
# Neither commandArgs nor a surviving source frame may be required to find
# the shared attribution helper.
script <- normalizePath(file.path(getwd(), "localhost/scripts/postprocessing_emissions",
                                  "1post_raster_fr_generator_diskmemory_v9.R"), winslash = "/")
expected <- normalizePath(file.path(dirname(script), "../helpers/woodfuel_nrb_attribution.R"), winslash = "/")
scratch <- Sys.getenv("MOFUSS_TEST_SCRATCH", "E:/MoFuSS_Active/MDG_NRB_attribution_fix_2026-10-07/emissions")
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
fixture <- tempfile("stage1_source_", tmpdir = scratch)
dir.create(fixture)
previous <- getwd()
setwd(fixture)
for (loader in c("source", "sys.source")) {
  env <- new.env(parent = globalenv())
  env$MOFUSS_CONFIG_ONLY <- TRUE
  env$commandArgs <- function(...) character()
  if (loader == "source") source(script, local = env) else sys.source(script, envir = env)
  # Loading has returned; its file/ofile frame no longer exists here.
  api <- env$stage1_nrb_api()
  stopifnot(identical(api$source_path, expected),
            is.function(api$mofuss_period_nrb), identical(api, env$stage1_nrb_api()))
}
setwd(previous)
cat("PASS: source/sys.source config-only loading resolves the helper after the source frame closes, from arbitrary cwd without --file.\n")
