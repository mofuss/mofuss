# MoFuSS
# Version 2
# Date: Apr 2026

# 2dolist ----

# Internal parameters ----
runGADM <- 0
runplantations <- 0
telegram_msgs <- 1
personal_demand <- 1

start_time <- Sys.time()

# Load libraries ----
library(rprojroot)
library(rstudioapi)
library(telegram.bot)

# Get system information
computer_name <- Sys.info()[["nodename"]] # Gets the hostname of the computer
os_name <- Sys.info()[["sysname"]]       # Gets the operating system name

# Determine the script directory
scriptsmofuss <- dirname(rstudioapi::getSourceEditorContext()$path)
cat("scriptsmofuss set to:", scriptsmofuss, "\n")

# Load machine-local secrets. Process-level variables (for example, CI or
# scheduler-injected secrets) take precedence over values in the local .env.
env_file <- file.path(scriptsmofuss, ".env")
telegram_env_names <- c(
  "MOFUSS_TELEGRAM_BOT_TOKEN",
  "MOFUSS_TELEGRAM_CHAT_ID"
)
telegram_process_values <- Sys.getenv(
  telegram_env_names,
  unset = NA_character_
)

if (file.exists(env_file)) {
  readRenviron(env_file)
}

for (i in seq_along(telegram_env_names)) {
  if (!is.na(telegram_process_values[[i]])) {
    do.call(
      Sys.setenv,
      setNames(
        list(telegram_process_values[[i]]),
        telegram_env_names[[i]]
      )
    )
  }
}

# Telegram Bot Setup ----
telegram_token <- Sys.getenv("MOFUSS_TELEGRAM_BOT_TOKEN", unset = "")
chat_id <- Sys.getenv("MOFUSS_TELEGRAM_CHAT_ID", unset = "")
telegram_credentials_present <- nzchar(telegram_token) && nzchar(chat_id)

bot <- NULL
if (telegram_msgs == 1 && telegram_credentials_present) {
  bot <- tryCatch(
    Bot(token = telegram_token),
    error = function(e) {
      message("Telegram setup failed; notifications disabled: ", conditionMessage(e))
      NULL
    }
  )
} else if (telegram_msgs == 1) {
  message(
    "Telegram notifications disabled: set MOFUSS_TELEGRAM_BOT_TOKEN ",
    "and MOFUSS_TELEGRAM_CHAT_ID in the local .env or process environment."
  )
}

send_telegram_message <- function(message) {
  if (telegram_msgs != 1 || is.null(bot)) {
    return(invisible(FALSE))
  }

  tryCatch(
    {
      bot$sendMessage(chat_id = chat_id, text = message)
      invisible(TRUE)
    },
    error = function(e) {
      message("Telegram notification failed; continuing: ", conditionMessage(e))
      invisible(FALSE)
    }
  )
}

# Function to source scripts with progress notifications
source_script <- function(script_path, script_name) {
  tryCatch({
    # Notify script start
    cat("Starting", script_name, "...\n")
    if (telegram_msgs == 1) {
    # send_telegram_message(paste("Starting", script_name, "⏳"))
    send_telegram_message(paste("Starting", script_name, "⏳ -- running on", computer_name, "(", os_name,")"))    
    }
    # Run the script
    source(script_path)
    
    # Notify script success
    cat(script_name, "sourced successfully.\n")
    if (telegram_msgs == 1) {
    send_telegram_message(paste(script_name, "ran successfully! ✅" , "-- running on", computer_name, "(", os_name,")"))   
    }
    TRUE
  }, error = function(e) {
    # Notify script error
    cat("Error in", script_name, ":", e$message, "\n")
    if (telegram_msgs == 1) {
    send_telegram_message(paste("Error in script:", script_name, "\nMessage ❌", e$message, "-- running on", computer_name, "(", os_name,")"))
    }
    FALSE
  })
}

copy_personal_demand_csv <- function() {
  
  if (personal_demand == 1) {
    
    cat("\033[32mPlease choose the demand table (.csv).\033[0m\n")
    
    if (telegram_msgs == 1) {
      send_telegram_message(
        paste(
          "You now need you to upload the demand table 📂",
          "-- running on", computer_name, "(", os_name, ")"
        )
      )
    }
    
    # Opens pop-up file chooser
    input_csv <- utils::choose.files(
      caption = "Please choose the demand table (.csv)",
      filters = matrix(
        c("CSV Files", "*.csv"),
        ncol = 2,
        byrow = TRUE
      )
    )
    
    input_csv <- input_csv[1]
    
    if (!grepl("\\.csv$", input_csv, ignore.case = TRUE)) {
      stop("Selected file is not a .csv file.")
    }
    
    # Extract uploaded filename
    uploaded_name <- basename(input_csv)
    
    demand_dir <- paste0(
      countrydir,
      "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/"
    )
    
    if (!dir.exists(demand_dir)) {
      dir.create(demand_dir, recursive = TRUE)
    }
    
    output_path <- paste0(
      demand_dir,
      uploaded_name
    )
    
    file.copy(
      from = input_csv,
      to = output_path,
      overwrite = TRUE
    )
    
    cat(
      paste0(
        "\033[32mPersonal demand CSV copied successfully as: ",
        uploaded_name,
        "\033[0m\n"
      )
    )
    
    if (telegram_msgs == 1) {
      send_telegram_message(
        paste(
          "Personal demand table uploaded successfully ✅",
          "\nFile:", uploaded_name,
          "-- running on", computer_name, "(", os_name, ")"
        )
      )
    }
  }
}

# Source files ----
scripts <- list(
  "00_webmofuss.R",
  "0_set_directories_and_region_v3.R",
  if (runGADM == 1) "preprocessing4globaldatasets/0apre_GADM_admin_wp_v8.R",
  "1_erase_all_v1.R",
  "2_copy_files_v1.R",
  # "2a_ics_constructor_v7.R",
  if (personal_demand != 1) "2b_oneschema_fix_v5.R",
  "2c_demand_tables_v5.R",
  "3_demand4IDW_v8.R",
  "4_produce_growth_and_stock_csv_v1.R",
  "5_harmonizer_v5.R",
  "6a_scenarios.R",
  # if (runplantations == 1) "6b_plantations_v0.R",
  # "6c_demand_maps_v1.R"
  "6d_parameters_dinamica_v1.R"
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
    
    # After copying files, ask user for personal demand CSV
    if (script == "2_copy_files_v1.R") {
      copy_personal_demand_csv()
    }
  }
}

# Notify overall completion
if (telegram_msgs == 1) {
  if (all_successful) {
    send_telegram_message(paste("All scripts ran successfully ✅," ,"-- running on", computer_name, "(", os_name,"). We now need you to log back into your MoFuSS account 💻  "))
    cat("All scripts ran successfully! ✅\n")
  } else {
    send_telegram_message(paste("One or more scripts encountered errors ❌","-- runnning on", computer_name, "(", os_name,")"))
    cat("One or more scripts encountered errors. ❌\n")
  }  
}

end_time <- Sys.time()
end_time - start_time
