library(funchir)

setwd(paste0(gitlabdir,"/linwin/scripts"))
getwd()

pkgname <- "GADM_admin_hsrl.R"
pkgname <-"GADM_admin_wp.R"
pkgname <- "3_demand4IDW_v1.R"
# pkgname <- "5_choose_GADM_AOI_gpkg_v2.R"
# pkgname <- "6_harmonizer.R"
# pkgname <- "7_scenarios.R"
funchir::stale_package_check(pkgname)
