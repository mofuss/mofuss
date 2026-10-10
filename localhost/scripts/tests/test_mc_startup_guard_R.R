# Exercise the actual terminal R expressions in scratch; never generate MC
# draws or mutate a real run. The native full-graph fixtures test reset/order.
script <- file.path(getwd(), "localhost/scripts/rnorm_v8.R")
expressions <- as.list(parse(file = script))
last <- length(expressions)
stopifnot(identical(expressions[[last]][[1L]], as.name("write.csv")),
          identical(expressions[[last - 1L]], quote(publish_current_mc_batch())),
          grepl("mc_startup_guard.csv", paste(deparse(expressions[[last]]), collapse=""), fixed=TRUE))
find_branch <- function(name) {
  Filter(function(x) is.call(x) && identical(x[[1L]], as.name("if")) &&
           identical(x[[2L]], substitute(VAR == 1L, list(VAR=as.name(name)))), expressions)[[1L]]
}
adopt <- find_branch("PublishExistingBatch")
dry <- find_branch("DryRun")
scratch <- Sys.getenv("MOFUSS_TEST_SCRATCH", tempdir())
fixture <- tempfile("startup_marker_R_", tmpdir = scratch)
dir.create(fixture, recursive=TRUE)
previous <- getwd()
tryCatch({
  setwd(fixture)
  for (mode in c("dry", "adopt", "failed_publish", "success")) {
    write.csv(data.frame(Key=1L,Value=-1L), "mc_startup_guard.csv", row.names=FALSE)
    env <- new.env(parent=globalenv())
    env$PublishExistingBatch <- as.integer(mode=="adopt")
    env$DryRun <- as.integer(mode=="dry")
    env$MC <- 1L
    env$configured_start <- 2000L
    env$configured_end <- 2050L
    env$parameter_value <- function(...) "BaU1_v2"
    env$dir.exists <- function(...) TRUE
    env$list.files <- function(...) "Growth_less_harv51.tif"
    env$quit <- function(...) stop("EXPECTED_EARLY_EXIT", call.=FALSE)
    env$publish_current_mc_batch <- function() {
      if (mode=="failed_publish") stop("EXPECTED_PUBLISH_FAILURE", call.=FALSE)
      invisible(TRUE)
    }
    error <- tryCatch({
      for (expr in list(adopt, dry, expressions[[last-1L]], expressions[[last]])) eval(expr, env)
      NULL
    }, error=identity)
    expected <- if (mode=="success") 1L else -1L
    stopifnot(identical(read.csv("mc_startup_guard.csv")$Value, expected))
    if (mode=="success") stopifnot(is.null(error)) else stopifnot(inherits(error,"error"))
  }
}, finally=setwd(previous))
cat("MC_STARTUP_R_COMPLETION_GUARD_TESTS_OK\n")
