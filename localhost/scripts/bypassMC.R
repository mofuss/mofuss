# MoFuSS
# Version 4
# Date: Sep 2025

# 2dolist
# bau_dir <-  "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_bau_1km_subc/Temp/" # Read from din_parameters.csv
# Check list of useless files before reruning
# ---- Directories ----
# paste0(bau_dir,"/Temp")
# paste0(getwd(),"/Temp")

# Internal parameters
bau_dir <-  "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_bau_1km_subc" # Read from din_parameters.csv

# Load libraries ----
library(fs)
library(stringr)
library(purrr)

# Check list of useless files before reruning
# unlink("Summary_Report//Mofuss_Summary_Report.pdf", force=TRUE)
# unlink("LaTeX//Mofuss_Summary_Report.pdf", force=TRUE)
unlink("LaTeX//InputPara.csv", force=TRUE)
unlink("LaTeX//NRBTable.csv", force=TRUE)
unlink("LaTeX//fNRBTable.csv", force=TRUE)
unlink("LaTeX//SumTable.csv", force=TRUE)
unlink("LaTeX//SumTableBaU.csv", force=TRUE)
unlink("LaTeX//Growth_Harvest_AniOutBaU.mp4", force=TRUE)
unlink("HTML_animation_OutBaU//*", recursive = TRUE,force=TRUE)
unlink("OutBaU/*", recursive = TRUE,force=TRUE)
unlink("OutICS/*", recursive = TRUE,force=TRUE)

unlink("Debugging", recursive = TRUE,force=TRUE) 
unlink("Temp", recursive = TRUE,force=TRUE)
if (!dir.exists("Debugging")) {
  dir.create("Debugging")
}
if (!dir.exists("Temp")) {
  dir.create("Temp")
}

# --- Inputs ---
bau_dir  <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_bau_1km_subc"
dest_dir <- getwd()

# 1) Find debugging_* directories in bau_dir
debugging_dirs <- list.dirs(bau_dir, recursive = FALSE, full.names = TRUE)
debugging_dirs <- debugging_dirs[grepl("^debugging_", basename(debugging_dirs))]

cat("Found", length(debugging_dirs), "debugging folders in bau_dir:",
    paste(basename(debugging_dirs), collapse = ", "), "\n")

# 2) For each, create an empty folder with the same name in getwd()
for (src in debugging_dirs) {
  name <- basename(src)
  dst  <- file.path(dest_dir, name)
  
  if (!dir.exists(dst)) dir.create(dst, recursive = TRUE)
  
  # ensure empty (do NOT touch the source folders)
  inside <- list.files(dst, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(inside)) unlink(inside, recursive = TRUE, force = TRUE)
  
  cat("Ready (empty):", dst, "\n")
}

# ---- Exact filename patterns (match on basename only) ----
patterns <- c(
  "^Harvest_pixels_V\\.csv$",
  "^Harvest_pixels_W\\.csv$",
  "^i_st_all\\.csv$",
  "^k_all\\.csv$",
  "^LULC_Categories.\\.csv$",  # exactly ONE variable char where the "?" is
  "^Prune_factor_V\\.csv$",
  "^Prune_factor_W\\.csv$",
  "^rmax_all\\.csv$"
)

# Build one regex that matches any of the above
name_re <- str_c("^(?:", str_c(patterns, collapse = "|"), ")$")

# List files, then keep only those whose BASENAME matches
candidates <- dir_ls(paste0(bau_dir,"/Temp"), type = "file", recurse = FALSE)
files_to_copy <- candidates[str_detect(path_file(candidates), name_re)]

# Copy
walk(files_to_copy, ~ file_copy(.x, paste0(getwd(),"/Temp"), overwrite = TRUE))

# Optional: quick check
stopifnot(all(file_exists(file.path(paste0(getwd(),"/Temp"), path_file(files_to_copy)))))

# Max AGB value for graphs and animation showing last MC run
maxAGB_all<-read.csv("Temp//k_all.csv")
MaxAGB<-(max(maxAGB_all, na.rm=TRUE))
write.csv(MaxAGB,"Temp//MaxAGB.csv")
MaxAGB_lastMC<-(max(maxAGB_all[MC, ], na.rm=TRUE))
write.csv(MaxAGB_lastMC,"Temp//MaxAGB_lastMC.csv")
MaxAGB_firstMC<-(max(maxAGB_all[1, ], na.rm=TRUE))
write.csv(MaxAGB_firstMC,"Temp//MaxAGB_firstMC.csv")
