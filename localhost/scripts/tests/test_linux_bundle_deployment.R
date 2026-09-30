# Verify deployment preserves existing science inputs/results and fails early
# for an incomplete repository bundle. Does not invoke the preprocessing reset.
scripts <- normalizePath("localhost/scripts", winslash = "/", mustWork = TRUE)
# Load only the file-copy functions; never execute preprocessing in a test.
for (expression in parse(file.path(scripts, "2_copy_files_v4.R"))) {
  if (is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.symbol(expression[[2L]]) && as.character(expression[[2L]]) %in% c("mofuss_runtime_bundle_files",
        "mofuss_validate_runtime_bundle", "mofuss_copy_runtime_bundle")) {
    eval(expression)
  }
}
fixture <- tempfile("linux_bundle_deployment_")
dir.create(fixture)
for (name in c("parameters.csv", "Out/result.csv", "Temp/mc_batch_ready.csv")) {
  dir.create(dirname(file.path(fixture, name)), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste("preserve", name), file.path(fixture, name))
}
sentinels <- list.files(fixture, recursive = TRUE, full.names = TRUE)
before <- tools::md5sum(sentinels)
mofuss_copy_runtime_bundle(scripts, fixture)
stopifnot(identical(before, tools::md5sum(sentinels)))
files <- mofuss_runtime_bundle_files()
stopifnot(identical(unname(tools::md5sum(file.path(scripts, files))),
                    unname(tools::md5sum(file.path(fixture, files)))))
if (.Platform$OS.type != "windows") {
  stopifnot(all(file.access(file.path(fixture, files[grepl("[.]sh$", files)]), 1L) == 0L))
}
incomplete <- tempfile("incomplete_linux_bundle_")
dir.create(incomplete)
failure <- try(mofuss_copy_runtime_bundle(incomplete, fixture), silent = TRUE)
stopifnot(inherits(failure, "try-error"), identical(before, tools::md5sum(sentinels)))
unlink(c(fixture, incomplete), recursive = TRUE)
cat("LINUX_BUNDLE_DEPLOYMENT_OK\n")
