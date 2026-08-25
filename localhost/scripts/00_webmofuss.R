# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autónoma de México
# and Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# https://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS ----
# Script: 00_webmofuss.R
# Version: 1
# Date: Jan 2025
# Execution: Source from RStudio; it is normally sourced by a numbered main script.
#
# Purpose: Detect web/local execution and load machine-specific MoFuSS paths and
# notification identifiers from the local .env configuration.
# Inputs: Optional .env key-value file in the active working directory.
# Outputs: Shared configuration objects including paths, webmofuss and chatId.
# Side effects: Reads local configuration into the current R session.

# 2dolist ----

# Internal parameters ----

# Load libraries ----
library(data.table)
library(readr)
library(readxl)

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
	githubdir <- paramsEnv['githubdir']$value
	countrydir <- paramsEnv['countrydir']$value
	demanddir <- paramsEnv['demanddir']$value 
	admindir <- paramsEnv['admindir']$value
	emissionsdir <- paramsEnv['emissionsdir']$value
	rTempdir <- paramsEnv['rTempdir']$value
	parameters_file_path <- paramsEnv['parameters_file_path']$value
	
	parameters_file <- basename(parameters_file_path) # OJO ACA
	
	scriptsmofuss <- paramsEnv['scriptsmofuss']$value
	chatId <- paramsEnv['chatId']$value
} else if(webmofuss == 0) {
	# ToDoList
  # Load Temporals for webmofuss if needed
} else {
	# Algo salió mal
}
