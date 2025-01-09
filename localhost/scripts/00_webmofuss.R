# MoFuSS
# Version 1
# Date: Jan 2025

# 2dolist ----

# Internal parameters ----

# Load libraries ----
library(readxl)
library(readr)
library(data.table)

file_path <- ".env"
if (file.exists(file_path)) {
	webmofuss = 1
} else {
	webmofuss = 0
}

if (webmofuss == 1) {
	dframe <- read.table(file='.env',header=FALSE,
						sep='=',col.names=c('key','value'))
	paramsEnv <- data.table(dframe,key='key')
	gitlabdir <- paramsEnv['gitlabdir']$value 
	countrydir <- paramsEnv['countrydir']$value
	demanddir <- paramsEnv['demanddir']$value 
	admindir <- paramsEnv['admindir']$value
	emissionsdir <- paramsEnv['emissionsdir']$value
	rTempdir <- paramsEnv['rTempdir']$value
	parameters_file_path <- paramsEnv['parameters_file_path']$value
	
	parameters_file <- basename(parameters_file_path) # OJO ACA
	
	scriptsmofuss <- paramsEnv['scriptsmofuss']$value
} else if(webmofuss == 0) {
	# ToDoList
  # Load Temporals for webmofuss if needed
} else {
	#Algo salió mal
}