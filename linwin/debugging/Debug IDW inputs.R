# Debug IDW inputs

library(tibble)
library(readr)
library(tidyverse)

# Regional inputs for IDW
# nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2020-2030
colnames20_30v <- c("2021_fw_v","2022_fw_v","2023_fw_v","2024_fw_v",
              "2025_fw_v","2026_fw_v","2027_fw_v","2028_fw_v","2029_fw_v","2030_fw_v")
colnames20_30w <- c("2021_fw_w","2022_fw_w","2023_fw_w","2024_fw_w",
                    "2025_fw_w","2026_fw_w","2027_fw_w","2028_fw_w","2029_fw_w","2030_fw_w")

setwd(countrydir)

vf <- read_csv("In/DemandScenarios/BaU_fwch_v.csv")
names(vf)
vsumf <- vf %>%
  summarise_at(c(colnames20_30v), sum, na.rm = TRUE) %>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(across(where(is.numeric)))) / 1000
vrowf <- nrow(vf)
vrowfr <- rast("In/DemandScenarios/locs_raster_v.tif") %>% 
  global(fun="notNA")

# Calculate number of pixels in locs raster!

wf <- read_csv("In/DemandScenarios/BaU_fwch_w.csv")
wsumf <- wf %>%
  summarise_at(c(colnames20_30w), sum, na.rm = TRUE) %>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(across(where(is.numeric)))) / 1000
wrowf <- nrow(wf)
wrowfr <- rast("In/DemandScenarios/locs_raster_w.tif") %>% 
  global(fun="notNA")

sumf <- vsumf$sum + wsumf$sum

sumf
vrowf
vrowfr
wrowf
wrowfr





# ####
# 
# idwdebuggingdir <- "E:/MoFuSS_Global_Nov2023_test/"
# setwd(idwdebuggingdir)
# getwd()
# 
# central_v <- read_csv("central/BaU_fwch_v.csv")
# names(central_v)
# central_vsum <- central_v %>%
#   summarise_at(c(colnames20_30v), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# central_vrow <- nrow(central_v)
# # Calculate number of pixels in locs raster!
# 
# central_w <- read_csv("central/BaU_fwch_w.csv")
# central_wsum <- central_w %>%
#   summarise_at(c(colnames20_30w), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# central_wrow <- nrow(central_w) 
# 
# central_sum <- central_vsum$sum + central_wsum$sum
# 
# 
# eastern_v <- read_csv("eastern/BaU_fwch_v.csv")
# eastern_vsum <- eastern_v %>%
#   summarise_at(c(colnames20_30v), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# eastern_vrow <- nrow(eastern_v) 
# 
# eastern_w <- read_csv("eastern/BaU_fwch_w.csv")
# eastern_wsum <- eastern_w %>%
#   summarise_at(c(colnames20_30w), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# eastern_wrow <- nrow(eastern_w) 
# 
# eastern_sum <- eastern_vsum$sum + eastern_wsum$sum
# 
# 
# 
# southern_v <- read_csv("southern/BaU_fwch_v.csv")
# southern_vsum <- southern_v %>%
#   summarise_at(c(colnames20_30v), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# southern_vrow <- nrow(southern_v) 
# 
# southern_w <- read_csv("southern/BaU_fwch_w.csv")
# southern_wsum <- southern_w %>%
#   summarise_at(c(colnames20_30w), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# southern_wrow <- nrow(southern_w) 
# 
# southern_sum <- southern_vsum$sum + southern_wsum$sum
# 
# 
# western_v <- read_csv("western/BaU_fwch_v.csv")
# western_vsum <- western_v %>%
#   summarise_at(c(colnames20_30v), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# western_vrow <- nrow(western_v) 
# 
# western_w <- read_csv("western/BaU_fwch_w.csv")
# western_wsum <- western_w %>%
#   summarise_at(c(colnames20_30w), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# western_wrow <- nrow(western_w) 
# 
# western_sum <- western_vsum$sum + western_wsum$sum
# 
# 
# # Continental inputs for IDW
# cont_v <- read_csv("0_cont/BaU_fwch_v.csv")
# cont_vsum <- cont_v %>%
#   summarise_at(c(colnames20_30v), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# cont_vrow <- nrow(cont_v) 
# central_vrow+eastern_vrow+southern_vrow+western_vrow
# 
# 
# cont_w <- read_csv("0_cont/BaU_fwch_w.csv")
# cont_wsum <- cont_w %>%
#   summarise_at(c(colnames20_30w), sum, na.rm = TRUE) %>%
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) / 1000
# cont_wrow <- nrow(cont_w)
# central_wrow+eastern_wrow+southern_wrow+western_wrow
# 
# 
# cont_sum <- cont_vsum$sum + cont_wsum$sum
# central_sum + eastern_sum + southern_sum + western_sum
# 
# print(tibble::as_tibble(central_v), n=100) ####
# 
# 


