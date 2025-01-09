correr = 0
source("./00_webmofuss.R")
source(paste0(scriptsmofuss,"/0_set_directories_and_region_v3.R"))
if(correr == 1) {
	source(paste0(scriptsmofuss,"/preprocessing4globaldatasets/1apre_GADM_admin_wp_v5.R"))
}
source(paste0(scriptsmofuss,"/1_erase_all_v1.R"))
source(paste0(scriptsmofuss,"/2_copy_files_v1.R"))
#source(paste0(scriptsmofuss,"/3_demand4IDW_v3.R"))