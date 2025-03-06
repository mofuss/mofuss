## Copyright 2025 Stockholm Environment Institute ----

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS
# Version 1
# Date: Jan 2025

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