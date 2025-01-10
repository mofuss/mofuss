# MoFuSS
# Version 1
# Date: Jan 2025

# Load libraries ----
library(rprojroot)
library(rstudioapi)
library(telegram.bot)

# Internal parameters ----
runGADM <- 0

# Telegram Bot Setup ----
telegram_token <- "7926603565:AAG7Nb56jbBdZbdBrcZBSs688oupZVk9RwU"  # Replace with your bot token
chat_id <- "252483477"  # Replace with your chat ID
bot <- Bot(token = telegram_token)

send_telegram_message <- function(message) {
  bot$sendMessage(chat_id = chat_id, text = message)
}

# Determine the script directory
scriptsmofuss <- dirname(rstudioapi::getSourceEditorContext()$path)
cat("scriptsmofuss set to:", scriptsmofuss, "\n")

# Function to source scripts with progress notifications
source_script <- function(script_path, script_name) {
  tryCatch({
    # Notify script start
    cat("Starting", script_name, "...\n")
    send_telegram_message(paste("Starting", script_name, "⏳"))
    
    # Run the script
    source(script_path)
    
    # Notify script success
    cat(script_name, "sourced successfully.\n")
    send_telegram_message(paste(script_name, "ran successfully! ✅"))
    TRUE
  }, error = function(e) {
    # Notify script error
    cat("Error in", script_name, ":", e$message, "\n")
    send_telegram_message(paste("Error in script:", script_name, "\nMessage:", e$message))
    FALSE
  })
}

# Source files ----
scripts <- list(
  "00_webmofuss.R",
  "0_set_directories_and_region_v3.R",
  if (runGADM == 1) "preprocessing4globaldatasets/1apre_GADM_admin_wp_v5.R",
  "1_erase_all_v1.R",
  "2_copy_files_v1.R",
  "3_demand4IDW_v3.R",
  "4_produce_growth_and_stock_csv.R",
  "5_harmonizer_v1.R",
  "6_scenarios.R"
)

all_successful <- TRUE
for (script in scripts) {
  if (!is.null(script)) {
    script_path <- file.path(scriptsmofuss, script)
    successful <- source_script(script_path, script)
    if (!successful) {
      all_successful <- FALSE
      break
    }
  }
}

# Notify overall completion
if (all_successful) {
  send_telegram_message("All scripts ran successfully! ✅")
  cat("All scripts ran successfully! ✅\n")
} else {
  send_telegram_message("One or more scripts encountered errors. ❌")
  cat("One or more scripts encountered errors. ❌\n")
}