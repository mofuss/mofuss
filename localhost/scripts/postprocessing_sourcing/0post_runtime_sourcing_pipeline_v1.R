# MoFuSS sourcing pipeline: run after the completed Dinamica simulations.
# Stage 1: model-implied approximation. Stage 2: recorded runtime sourcing.
# Edit the USER INPUTS block, then Source this file in RStudio, or use Rscript.
# Rscript 0post_runtime_sourcing_pipeline_v1.R --check validates without outputs.

# BEGIN USER INPUTS ----------------------------------------------------------
# AUTO uses the parent of this repository, wherever it is installed.
# Set another drive once here, e.g. "E:/" on Windows or "/mnt/data" on Linux.
SOURCING_WORKING_ROOT <- "AUTO"
# AUTO puts analyses under <batch root>/_mofuss_postprocessing.
# Alternatively set one shared absolute analysis parent on any mounted drive.
SOURCING_ANALYSIS_PARENT <- "AUTO"

# root = "" inherits SOURCING_WORKING_ROOT. Override root per batch if needed.
# analysis_folder is a neutral folder NAME, matching the emissions pipeline.
# Disabled batches are skipped. folders contains the four completed scenarios.
SOURCING_BATCHES <- list(
  AGO = list(
    enabled = FALSE,
    root = "",
    analysis_folder = "AGO_1000m_2050_mc3",
    folders = c(
      "AGO_1000m_bau1_2050_mc3_capped",
      "AGO_1000m_bau1_2050_mc3_uncapped",
      "AGO_1000m_ics3_2050_mc3_capped",
      "AGO_1000m_ics3_2050_mc3_uncapped"
    )
  ),
  ECSA = list(
    enabled = TRUE,
    root = "",
    analysis_folder = "ECSA_1000m_2050_mc30",
    folders = c(
      "ECSA_1000m_bau1_2050_mc30_capped",
      "ECSA_1000m_bau1_2050_mc30_uncapped",
      "ECSA_1000m_ics3_2050_mc30_capped",
      "ECSA_1000m_ics3_2050_mc30_uncapped"
    )
  ),
  GOG = list(
    enabled = FALSE,
    root = "",
    analysis_folder = "GOG_1000m_2050_mc30",
    folders = c(
      "GOG_1000m_bau1_2050_mc30_capped",
      "GOG_1000m_bau1_2050_mc30_uncapped",
      "GOG_1000m_ics3_2050_mc30_capped",
      "GOG_1000m_ics3_2050_mc30_uncapped"
    )
  ),
  MDG = list(
    enabled = FALSE,
    root = "",
    analysis_folder = "MDG_1000m_2050_mc3",
    folders = c(
      "MDG_1000m_bau1_2050_mc3_capped",
      "MDG_1000m_bau1_2050_mc3_uncapped",
      "MDG_1000m_ics3_2050_mc3_capped",
      "MDG_1000m_ics3_2050_mc3_uncapped"
    )
  ),
  LSO = list(
    enabled = FALSE,
    root = "",
    analysis_folder = "LSO_1000m_2050_mc3",
    folders = c(
      "LSO_1000m_bau1_2050_mc3_capped",
      "LSO_1000m_bau1_2050_mc3_uncapped",
      "LSO_1000m_ics3_2050_mc3_capped",
      "LSO_1000m_ics3_2050_mc3_uncapped"
    )
  ),
  MLI = list(
    enabled = FALSE,
    root = "",
    analysis_folder = "MLI_1000m_2050_mc3",
    folders = c(
      "MLI_1000m_bau1_2050_mc3_capped",
      "MLI_1000m_bau1_2050_mc3_uncapped",
      "MLI_1000m_ics3_2050_mc3_capped",
      "MLI_1000m_ics3_2050_mc3_uncapped"
    )
  ),
  GAB = list(
    enabled = FALSE,
    root = "",
    analysis_folder = "GAB_1000m_2050_mc3",
    folders = c(
      "GAB_1000m_bau1_2050_mc3_capped",
      "GAB_1000m_bau1_2050_mc3_uncapped",
      "GAB_1000m_ics3_2050_mc3_capped",
      "GAB_1000m_ics3_2050_mc3_uncapped"
    )
  ),
  GLEA = list(
    enabled = FALSE,
    root = "",
    analysis_folder = "GLEA_1000m_2050_mc3",
    folders = c(
      "GLEA_1000m_bau1_2050_mc3_capped",
      "GLEA_1000m_bau1_2050_mc3_uncapped",
      "GLEA_1000m_ics3_2050_mc3_capped",
      "GLEA_1000m_ics3_2050_mc3_uncapped"
    )
  )
)

# 1 = approximation; 2 = recorded runtime sourcing; 1:2 runs both.
SOURCING_STAGES <- 1:2
# Inclusive endpoints: adjacent decades overlap at 2030 and 2040.
SOURCING_PERIODS <- c("2020:2030", "2030:2040", "2040:2050", "2020:2050")
SOURCING_MC_RUNS <- "all" # or "1:3", "1,3", or c(1L, 3L)
SOURCING_BLOCK_MB <- 64
SOURCING_SIGNED_POLICY <- "error" # "report" permits diagnostic signed accounting
SOURCING_OVERWRITE <- FALSE # TRUE replaces only the analysis output files
SOURCING_CHECK_ONLY <- FALSE # TRUE validates all enabled batches without outputs
SOURCING_TEMP_DIR <- NULL # Raster scratch folder; NULL uses R's temporary directory
# Optional batch fields zones and crosswalk override automatic input discovery.
# Use absolute paths; crosswalk accepts a CSV or a country boundary GPKG.
# END USER INPUTS ------------------------------------------------------------

.sp_stop <- function(...) stop(sprintf(...), call. = FALSE)
.sp_bool <- function(x, label) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) .sp_stop("%s must be TRUE or FALSE", label)
  x
}
.sp_text <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
    .sp_stop("%s must be one nonempty path or value", label)
  trimws(x)
}
.sp_script_path <- function() {
  target <- "0post_runtime_sourcing_pipeline_v1.R"
  frames <- sys.frames()
  candidates <- unlist(lapply(frames, function(frame) {
    # source() uses ofile; sys.source() uses file.
    vapply(c("ofile", "file"), function(name) {
      value <- get0(name, envir=frame, inherits=FALSE, ifnotfound="")
      if (is.character(value) && length(value)==1L && !is.na(value)) value else ""
    }, character(1))
  }), use.names=FALSE)
  args <- grep("^--file=", commandArgs(FALSE), value=TRUE)
  candidates <- c(rev(candidates), sub("^--file=", "", args),
    file.path(getwd(), target),
    file.path(getwd(), "localhost", "scripts", "postprocessing_sourcing", target))
  candidates <- candidates[nzchar(candidates) & basename(candidates)==target & file.exists(candidates)]
  if (!length(candidates)) .sp_stop("Cannot locate %s; Source the saved file or use its full Rscript path.", target)
  normalizePath(candidates[[1L]], winslash="/", mustWork=TRUE)
}
.sp_path <- function(path, base, must_exist=FALSE) {
  path <- chartr("\\", "/", path.expand(.sp_text(path, "Path")))
  if (.Platform$OS.type != "windows" && grepl("^[A-Za-z]:", path))
    .sp_stop("Windows drive path '%s' is not usable on Linux; set its Linux mount path in USER INPUTS.", path)
  if (!grepl("^(/|[A-Za-z]:/)", path)) path <- file.path(base, path)
  normalizePath(path, winslash="/", mustWork=must_exist)
}
.sp_child_name <- function(x, label) {
  x <- .sp_text(x, label)
  if (grepl("[/\\\\]", x) || x %in% c(".", "..")) .sp_stop("%s must be one child-folder NAME", label)
  x
}
.sp_key <- function(path) {
  path <- sub("/+$", "", normalizePath(path, winslash="/", mustWork=FALSE))
  if (.Platform$OS.type == "windows") tolower(path) else path
}
.sp_within <- function(path, parent) {
  path <- .sp_key(path); parent <- .sp_key(parent)
  identical(path, parent) || startsWith(path, paste0(parent, "/"))
}
.sp_rscript <- function() {
  exe <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  candidates <- c(file.path(R.home("bin"), exe), file.path(R.home("bin"), "x64", exe), Sys.which("Rscript"))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) .sp_stop("Cannot locate Rscript for this R installation")
  normalizePath(candidates[[1L]], winslash="/", mustWork=TRUE)
}
.sp_run <- function(script, args, scratch=NULL) {
  if (!is.null(scratch)) {
    dir.create(scratch, recursive=TRUE, showWarnings=FALSE)
    if (!dir.exists(scratch) || file.access(scratch, 2L)!=0L) .sp_stop("Scratch is not writable: %s", scratch)
    # R itself cannot start with a spaced TMPDIR on some systems. Pass a
    # dedicated raster scratch setting after R starts instead.
    vars <- "MOFUSS_SOURCING_TEMP_DIR"
    previous <- Sys.getenv(vars, unset=NA_character_)
    on.exit({
      for (i in seq_along(vars)) {
        if (is.na(previous[[i]])) Sys.unsetenv(vars[[i]]) else
          do.call(Sys.setenv, stats::setNames(list(previous[[i]]), vars[[i]]))
      }
    }, add=TRUE)
    do.call(Sys.setenv, as.list(stats::setNames(rep(scratch, length(vars)), vars)))
  }
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  status <- system2(.sp_rscript(),
    vapply(c("--vanilla", script, args), shQuote, character(1), type=quote_type),
    stdout="", stderr="", wait=TRUE)
  if (!identical(as.integer(status), 0L)) .sp_stop("Sourcing script %s failed with exit status %s", basename(script), status)
  invisible(TRUE)
}
.sp_inputs <- function(runs, entry, runtime) {
  discover <- function(run, kind) {
    candidates <- if (kind=="zones") c(
      "Sourcing/metadata/input_snapshot/LULCC/TempRaster/admin_c.tif", "LULCC/TempRaster/admin_c.tif"
    ) else c("Sourcing/metadata/country_crosswalk.csv",
      "Sourcing/metadata/input_snapshot/LULCC/TempVector/userarea.gpkg", "LULCC/TempVector/userarea.gpkg")
    candidates <- file.path(run, candidates)
    hits <- candidates[file.exists(candidates) & !dir.exists(candidates)]
    if (!length(hits)) .sp_stop("Missing %s for %s; set the batch %s path explicitly.", kind, run, kind)
    hits[[1L]]
  }
  resolve <- function(kind) {
    override <- entry[[kind]]
    if (!is.null(override) && length(override)==1L && !is.na(override) && nzchar(override)) {
      return(rep(.sp_path(override, dirname(runs[[1L]]), TRUE), length(runs)))
    }
    vapply(runs, discover, character(1), kind=kind)
  }
  zones <- resolve("zones"); crosswalks <- resolve("crosswalk")
  standard <- function(path) {
    x <- runtime$.rs_crosswalk(path)
    as.data.frame(x[order(x$source_id), ])
  }
  reference <- standard(crosswalks[[1L]])
  hashes <- unname(tools::md5sum(zones))
  for (i in seq_along(runs)) {
    if (!identical(standard(crosswalks[[i]]), reference))
      .sp_stop("Country crosswalk differs in %s", runs[[i]])
    if (!identical(hashes[[i]], hashes[[1L]])) {
      x <- terra::rast(zones[[1L]]); y <- terra::rast(zones[[i]])
      if (!terra::compareGeom(x, y, stopOnError=FALSE)) .sp_stop("Country-zone geometry differs in %s", runs[[i]])
      different <- terra::ifel(is.na(x)!=is.na(y), 1, terra::ifel(is.na(x), 0, terra::ifel(x==y, 0, 1)))
      if (terra::global(different, "sum", na.rm=TRUE)[1L,1L] != 0)
        .sp_stop("Country-zone values differ in %s", runs[[i]])
    }
  }
  list(zones=zones[[1L]], crosswalk=crosswalks[[1L]])
}
.sp_plan <- function(script_path) {
  script_dir <- dirname(script_path)
  scripts <- file.path(script_dir, c("1post_model_implied_sourcing_v1.R", "2post_runtime_sourcing_v1.R"))
  if (any(!file.exists(scripts))) .sp_stop("Sourcing scripts must be kept together in %s", script_dir)
  stages <- SOURCING_STAGES
  if (!is.numeric(stages) || !length(stages) || anyNA(stages) || any(!stages %in% 1:2) ||
      anyDuplicated(stages) || !identical(as.integer(stages), sort(as.integer(stages))))
    .sp_stop("SOURCING_STAGES must be 1L, 2L, or 1:2")
  overwrite <- .sp_bool(SOURCING_OVERWRITE, "SOURCING_OVERWRITE")
  .sp_bool(SOURCING_CHECK_ONLY, "SOURCING_CHECK_ONLY")
  if (!is.numeric(SOURCING_BLOCK_MB) || length(SOURCING_BLOCK_MB)!=1L ||
      !is.finite(SOURCING_BLOCK_MB) || SOURCING_BLOCK_MB<1) .sp_stop("SOURCING_BLOCK_MB must be at least 1")
  if (length(SOURCING_SIGNED_POLICY)!=1L || is.na(SOURCING_SIGNED_POLICY) ||
      !SOURCING_SIGNED_POLICY %in% c("error", "report")) .sp_stop("SOURCING_SIGNED_POLICY must be error or report")
  periods <- SOURCING_PERIODS
  if (!is.character(periods) || !length(periods) || anyNA(periods) || any(!grepl("^[0-9]{4}:[0-9]{4}$", periods)))
    .sp_stop("SOURCING_PERIODS must contain YYYY:YYYY ranges")
  mc <- SOURCING_MC_RUNS
  if (identical(mc, "all")) {
    mc_arg <- character()
  } else {
    if (is.numeric(mc)) mc <- paste(mc, collapse=",")
    if (!is.character(mc) || length(mc)!=1L || is.na(mc) ||
        !grepl("^[1-9][0-9]*(:[1-9][0-9]*|(?:,[1-9][0-9]*)*)$", mc, perl=TRUE))
      .sp_stop("SOURCING_MC_RUNS must be all, a range such as 1:3, or positive MC indices")
    mc_arg <- paste0("--mc-runs=", mc)
  }
  runtime <- new.env(parent=globalenv()); sys.source(scripts[[2L]], runtime); runtime$.rs_require()
  runtime$.rs_periods(periods)
  repo_parent <- normalizePath(file.path(script_dir, "../../../.."), winslash="/", mustWork=TRUE)
  default_root <- if (identical(SOURCING_WORKING_ROOT, "AUTO")) repo_parent else
    .sp_path(SOURCING_WORKING_ROOT, repo_parent)
  batches <- SOURCING_BATCHES
  if (!is.list(batches) || !length(batches) || is.null(names(batches)) ||
      anyNA(names(batches)) || any(!nzchar(names(batches))) || anyDuplicated(tolower(names(batches))))
    .sp_stop("SOURCING_BATCHES must be a uniquely named list")
  enabled <- list(); disabled <- character()
  for (name in names(batches)) {
    entry <- batches[[name]]
    if (!is.list(entry) || !.sp_bool(entry$enabled, paste0(name, "$enabled"))) {
      if (!is.list(entry)) .sp_stop("Batch %s must be a list", name)
      disabled <- c(disabled, name); next
    }
    root <- if (is.null(entry$root) || identical(entry$root, "")) default_root else .sp_path(entry$root, repo_parent)
    if (!dir.exists(root)) .sp_stop("Batch %s root does not exist: %s", name, root)
    root <- normalizePath(root, winslash="/", mustWork=TRUE)
    folder <- .sp_child_name(entry$analysis_folder, paste0(name, "$analysis_folder"))
    if (!is.character(entry$folders) || length(entry$folders)!=4L || anyNA(entry$folders) || anyDuplicated(entry$folders))
      .sp_stop("Batch %s must list four unique scenario folders", name)
    children <- vapply(entry$folders, .sp_child_name, character(1), label=paste0(name, "$folders"))
    runs <- file.path(root, children)
    missing <- runs[!dir.exists(runs)]
    if (length(missing)) .sp_stop("Batch %s is missing scenario folder: %s", name, missing[[1L]])
    runs <- vapply(runs, normalizePath, character(1), winslash="/", mustWork=TRUE)
    parent <- if (identical(SOURCING_ANALYSIS_PARENT, "AUTO")) file.path(root, "_mofuss_postprocessing") else
      .sp_path(SOURCING_ANALYSIS_PARENT, root)
    analysis <- file.path(parent, folder)
    inputs <- .sp_inputs(runs, entry, runtime)
    outputs <- file.path(analysis, c("model_implied_sourcing", "runtime_sourcing"))
    common <- c(paste0("--zones=", inputs$zones), paste0("--periods=", paste(periods, collapse=",")),
      mc_arg, paste0("--overwrite=", if (overwrite) "TRUE" else "FALSE"))
    args <- list(
      c(paste0("--scenario-dir=", runs), paste0("--boundaries=", inputs$crosswalk),
        paste0("--output-dir=", outputs[[1L]]), common),
      c(paste0("--run-dir=", runs), paste0("--crosswalk=", inputs$crosswalk),
        paste0("--output-dir=", outputs[[2L]]), paste0("--block-mb=", SOURCING_BLOCK_MB),
        paste0("--signed-policy=", SOURCING_SIGNED_POLICY), common)
    )
    enabled[[name]] <- list(name=name, root=root, runs=runs, analysis=analysis, outputs=outputs, args=args)
  }
  if (!length(enabled)) .sp_stop("No sourcing batches are enabled")
  all_runs <- unlist(lapply(enabled, `[[`, "runs"), use.names=FALSE)
  all_outputs <- unlist(lapply(enabled, function(b) b$outputs[stages]), use.names=FALSE)
  if (anyDuplicated(vapply(all_runs, .sp_key, character(1)))) .sp_stop("Enabled batches reuse a scenario folder")
  if (anyDuplicated(vapply(all_outputs, .sp_key, character(1)))) .sp_stop("Enabled batches share an output folder")
  for (output in all_outputs) for (run in all_runs) {
    if (.sp_within(output, run) || .sp_within(run, output)) .sp_stop("Analysis outputs must be outside working folders: %s", output)
  }
  scratch <- if (is.null(SOURCING_TEMP_DIR)) NULL else .sp_path(SOURCING_TEMP_DIR, repo_parent)
  list(batches=enabled, disabled=disabled, stages=as.integer(stages), scripts=scripts, scratch=scratch)
}
run_runtime_sourcing_pipeline <- function(args=character()) {
  unknown <- base::setdiff(args, "--check")
  if (length(unknown)) .sp_stop("Unknown pipeline argument: %s", unknown[[1L]])
  plan <- .sp_plan(.sourcing_pipeline_file)
  check_only <- SOURCING_CHECK_ONLY || "--check" %in% args
  cat("MoFuSS sourcing pipeline: stages ", paste(plan$stages, collapse=" -> "), "\n", sep="")
  cat("Enabled batches: ", paste(names(plan$batches), collapse=", "), "\n", sep="")
  cat("Disabled batches: ", paste(plan$disabled, collapse=", "), "\n", sep="")
  # Validate every selected analysis in every batch before publishing any output.
  for (batch in plan$batches) {
    cat("\nBatch ", batch$name, "\nWorking root: ", batch$root, "\nAnalysis: ", batch$analysis, "\n", sep="")
    for (stage in plan$stages) .sp_run(plan$scripts[[stage]], c(batch$args[[stage]], "--check"))
  }
  if (check_only) {
    cat("\nCHECK COMPLETE: all enabled sourcing inputs are valid; no analysis outputs were written.\n")
    return(invisible(plan))
  }
  for (batch in plan$batches) for (stage in plan$stages) {
    cat(sprintf("\n========== Sourcing %s: Stage %d/2 ==========\n", batch$name, stage))
    .sp_run(plan$scripts[[stage]], batch$args[[stage]], plan$scratch)
  }
  cat("\nSOURCING PIPELINE COMPLETE\n")
  invisible(plan)
}
.sourcing_pipeline_file <- .sp_script_path()
if (!identical(Sys.getenv("MOFUSS_RUNTIME_SOURCING_NO_AUTORUN"), "1") &&
    !isTRUE(get0("MOFUSS_CONFIG_ONLY", envir=environment(), inherits=FALSE, ifnotfound=FALSE))) {
  tryCatch(run_runtime_sourcing_pipeline(commandArgs(trailingOnly=TRUE)), error=function(e) {
    message("SOURCING PIPELINE ERROR: ", conditionMessage(e))
    if (!interactive()) quit(save="no", status=1L, runLast=FALSE)
  })
}
