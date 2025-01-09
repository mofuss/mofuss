#.env por persona 
runGADM = 0 #Cuando se actualizan las regiones o que Rob lo pide :p
source("./00_webmofuss.R")

#cat("Sourcing 0_set_directories_and_region_v3.R...\n")
#source(file.path(scriptsmofuss, "/0_set_directories_and_region_v3.R"))
#cat("00_set_directories_and_region_v3.R sourced successfully.\n")
#
#if (runGADM == 1) {
#  cat("Sourcing 1apre_GADM_admin_wp_v5.R...\n")
#  source(file.path(scriptsmofuss, "/preprocessing4globaldatasets/1apre_GADM_admin_wp_v5.R"))
#  cat("1apre_GADM_admin_wp_v5.R sourced successfully.\n")
#}
#
#cat("Sourcing 1_erase_all_v1.R...\n")
#source(file.path(scriptsmofuss, "/1_erase_all_v1.R"))
#cat("1_erase_all_v1.R sourced successfully.\n")
#
#cat("2_copy_files_v1.R...\n")
#source(file.path(scriptsmofuss, "/2_copy_files_v1.R"))
#cat("2_copy_files_v1.R sourced successfully.\n")
#
#cat("Sourcing 3_demand4IDW_v3.R...\n")
#source(paste0(scriptsmofuss,"/3_demand4IDW_v3.R"))
#cat("3_demand4IDW_v3.R sourced successfully.\n")
#
#cat("Sourcing 4_produce_growth_and_stock_csv.R...\n")
#source(paste0(scriptsmofuss,"/4_produce_growth_and_stock_csv.R"))
#cat("4_produce_growth_and_stock_csv.R sourced successfully.\n")

cat("Sourcing _harmonizer_v1.R...\n")
source(paste0(scriptsmofuss,"/5_harmonizer_v1.R"))
cat("_harmonizer_v1.R sourced successfully.\n")

cat("Sourcing 6_scenarios.R...\n")
source(paste0(scriptsmofuss,"/6_scenarios.R"))
cat("6_scenarios.R sourced successfully.\n")