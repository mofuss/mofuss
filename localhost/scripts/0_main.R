#library(telegram.bot)
library(glue)
library(stringi)
library(httr)

options(error = traceback)

reloj <- stri_unescape_unicode(gsub("\\U","\\u", "\\u23F3", fixed=TRUE))
paloma <- stri_unescape_unicode(gsub("\\U","\\u", "\\u2705", fixed=TRUE))
tache <- stri_unescape_unicode(gsub("\\U","\\u", "\\u274C", fixed=TRUE))
compu <- stri_unescape_unicode(gsub("\\U","\\u", "\\u1F4BB", fixed=TRUE))

source("./00_webmofuss.R")

chat_id <- chatId  # Replace with your chat ID
safe_telegram_message <- function(msg){
  tryCatch({
    POST(
      "http://10.99.1.38/api/telegram.php",
      body = list(
        chatid = chat_id,
        message = msg
      ),
      encode = "form",
      timeout(10)
    )
  }, error=function(e){
    invisible(NULL)
  })
}

# Get system information
computer_name <- Sys.info()[["nodename"]] # Gets the hostname of the computer
os_name <- Sys.info()[["sysname"]]       # Gets the operating system name

if(computer_name == "mofuss") {
  computer_name <- "WebMoFuSS_1"
}
if(computer_name == "mofuss2") {
	  computer_name <- "WebMoFuSS_2"
}

runGADM <- 0

cat("scriptsmofuss set to:", scriptsmofuss, "\n")

source_script <- function(script_path, script_name) {
  tryCatch({
    cat("Starting", script_name, "...\n")
    safe_telegram_message(glue("Starting {script_name} {reloj} -- running on {computer_name}({os_name})"))    
	inicio <- Sys.time()
    source(script_path)
	fin <- Sys.time()
    cat(
		script_name,
		"completed in",
		round(as.numeric(fin - inicio, units = "mins"), 2),
		"minutes\n"
		)
    safe_telegram_message(glue("{script_name} ran successfully! {paloma} -- running on {computer_name}({os_name})"))   
    TRUE
  }, error = function(e) {
    cat("Error in", script_name, ":", e$message, "\n")
	print(e)
	cat("\nTraceback:\n")
    traceback(2)
    safe_telegram_message(glue("Error in script: {script_name}\nMessage {tache} {e$message} -- running on {computer_name}({os_name})"))
	FALSE
  })
}

scripts <- list(
  #"00_webmofuss.R",
  "0_set_directories_and_region_v3.R",
  if (runGADM == 1) "preprocessing4globaldatasets/0apre_GADM_admin_wp_v7.R",
  #"1_erase_all_v1.R",
  "2_copy_files_v1.R",
  #"2b_oneschema_fix_v4.R",
  "2c_demand_tables_v5.R",
  "3_demand4IDW_v8.R",
  "4_produce_growth_and_stock_csv_v1.R",
  "5_harmonizer_v5.R",
  "6a_scenarios.R"
)
inicio <- Sys.time()
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
fin <- Sys.time()

if (all_successful) {
  safe_telegram_message(glue("All scripts ran successfully {paloma} -- running on {computer_name} ({os_name}). We now need you to log back into your MoFuSS account {compu}"))
  cat("All scripts ran successfully! ✅\n","completed in",
		round(as.numeric(fin - inicio, units = "mins"), 2),
		"minutes\n")
} else {
  safe_telegram_message(glue("One or more scripts encountered errors {tache} -- runnning on {computer_name} ({os_name})"))
  cat("One or more scripts encountered errors. ❌\n")
  quit(
	save = "no",
	status = 1
  )
}
