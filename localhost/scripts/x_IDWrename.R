# Rename 

# Define the directory where your files are located
# dir_path <- "D:/MoFuSS_SEA_1000m_Mar2024/In"
dir_path <- getwd()
#"G:/My Drive/webpages/000_MoFuSSCountryDatasets/0_IDW_gpu/x_deprecated_5datasets_april2024/ASIA_adm0_mongolia_results"

# List all files in the directory
files <- list.files(path = dir_path, full.names = TRUE)

# Optional: Filter the files to only those you want to rename
# This step is useful if there are other files in the directory that you don't want to rename
# Here, I'm assuming all files that start with 'IDWb_' are to be renamed
files_to_rename <- grep("IDWcv_C\\+\\+_(fw_v|fw_w)\\d+", files, value = TRUE) #Adjust following Edgar's names
files_to_rename

# Generate the new file names by removing the 'b'
new_names <- gsub("IDWcv_", "IDW_", files_to_rename)

# Use file.rename to rename the files
for(i in seq_along(files_to_rename)) {
  file.rename(from = files_to_rename[i], to = new_names[i])
}

# Print a message to indicate completion
cat("Files have been renamed successfully.\n")
