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
stopifnot("10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml" %in% files,
          "helpers/woodfuel_nrb_attribution.R" %in% files,
          "tools/windows_launcher_v1.R" %in% files)
stopifnot(identical(unname(tools::md5sum(file.path(scripts, files))),
                    unname(tools::md5sum(file.path(fixture, files)))))
if (.Platform$OS.type != "windows") {
  stopifnot(all(file.access(file.path(fixture, files[grepl("[.]sh$", files)]), 1L) == 0L))
}
# Preserve the Woodman branch introduced on Windows while deploying the
# standard Linux support bundle. Validate its selected model before copying.
woodman <- "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml"
mofuss_copy_runtime_bundle(scripts, fixture, active_egoml = woodman)
stopifnot(file.exists(file.path(fixture, woodman)),
          identical(unname(tools::md5sum(file.path(scripts, woodman))),
                    unname(tools::md5sum(file.path(fixture, woodman)))),
          identical(before, tools::md5sum(sentinels)))
# Step 2 configures v14 immediately, without depending on step 10 or changing
# any other model bytes. Historical v13 files remain available and unchanged.
source(file.path(scripts, "tools", "windows_launcher_v1.R"))
for (luc in c(1L, 3L)) {
  target <- file.path(fixture, woodman)
  original <- readBin(target, "raw", n = file.info(target)$size)
  mofuss_configure_model_luc(target, luc)
  configured <- rawToChar(readBin(target, "raw", n = file.info(target)$size))
  stopifnot(identical(configured,
                     .mofuss_windows_constant(rawToChar(original), "Int", "v302", as.character(luc))))
  stopifnot(identical(before, tools::md5sum(sentinels)))
}
failure <- try(mofuss_copy_runtime_bundle(scripts, fixture,
                 active_egoml = "missing_selected_model.egoml"), silent = TRUE)
stopifnot(inherits(failure, "try-error"), identical(before, tools::md5sum(sentinels)))
incomplete <- tempfile("incomplete_linux_bundle_")
dir.create(incomplete)
failure <- try(mofuss_copy_runtime_bundle(incomplete, fixture), silent = TRUE)
stopifnot(inherits(failure, "try-error"), identical(before, tools::md5sum(sentinels)))
unlink(c(fixture, incomplete), recursive = TRUE)
cat("LINUX_BUNDLE_DEPLOYMENT_OK\n")
