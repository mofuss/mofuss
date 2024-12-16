# MoFuSS local installation

Explore global geospatial results and access comprehensive documentation by visiting [MoFuSS's official website](https://www.mofuss.unam.mx/).

For questions or feedback, please contact mofussfreeware@gmail.com ✨

## MoFuSS can be run in two ways:
**1.- Web-Based Interface:** This is the simplest option as it requires no installation. However, it is still under development. To check for updates, visit our MoFuSS group or go to www.mofuss.unam.mx under the "User-Defined Scenarios" or "MoFuSS-US" section.

**2.- Local Installation:** This option allows you to run MoFuSS directly on your local computer. However, it's important to note that at least one module must be executed on a High-Performance Computing (HPC) cluster, as it relies on parallel C++ algorithms. This document provides a step-by-step guide for installing MoFuSS locally.

_For step-by-step instructions on running MoFuSS locally, including guidance on using the HPC module, continue reading below._ 📚

## Install and run MoFuSS Locally

MoFuSS uses Google Earth Engine for data download, R for data preparation and graphs, C++ for harvest probability maps, DINAMICA EGO for geospatial simulations (core scripts), ffmpeg for creating animations, LaTeX for formatting and generating final reports

* Stage 0: Data Download. If you already have all the necessary datasets, you can skip this stage and proceed directly to Stage 1.  
* Stage 1: Local Data Processing. Six sequential scripts are run on your local machine to generate Stage 1 results. 
* Stage 2: A subset of Stage 1 results are then sent to the High-Performance Computing (HPC) cluster, where geospatial simulations are performed. Once the simulations are complete, the results are sent back to your local machine for further processing.
* Stage 3: Final Analysis and Report Generation. Results from Stages 1 and 2 are used on your local machine to generate the final outputs, including summary results and reports formatted using LaTeX.

### Stage 0: Data Download

If you wish to download all or select MoFuSS datasets from scratch, or replace specific MoFuSS datasets with those you've produced, please refer to the MoFuSS repository: [gee2mofuss](https://github.com/mofuss/gee2mofuss). [**Please not that we are still documenting this repository, please contact aghilardi@ciga.unam.mx in case you need support**]

### Stage 1: Local Data Processing
#### Preparing your environment

* Install R and RStudio on your local machine.
* Clone this repository to your local machine.
* Install the `remotes` package in R: `install.packages("remotes")`
* Install the following packages in R:
    * Use pacman if desired:
    ```
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(readr, dplyr, fasterize, glue, igraph, raster, rgl, sf, tictoc, stars, gitlabr, inline, tidyverse, spam, svDialogs, terra, readxl)
    ```
* Obtain the three folders needed to run the R script locally and save them directly in the C or D drive. (THIS COMES FRO STAGE 0)
   * admin_regions
      * Geographical information
    * demand_in
        * Data on the fuel demand
    * world_1000m_yyyymmdd
        * Contains mapping data at specified resolution corresponding to the analysis precision level (1000m here)

#### Running Stage 1
All R scripts are written to run on Windows and Linux, and would need to be modified to run on MacOS.
* The sequence of scripts for this stage is the following:
    * 1. `scripts/0_set_directories_and_region.R`
         * Use the hardwired option in the script to set these paths to the locations you selected for the 3 folders in the above step.
    * 2. `scripts/1_erase_all_win.R`
    * 3. `scripts/3_demand4IDW_v1.R`
    * 4. `scripts/3_demand4IDW_v2.R`
    * 5. `scripts/3debug_demand4IDW_v1_optional.R`
    * 6. `scripts/5_harmonizer.R`
    * 7. `scripts/6_scenarios.R`

### Stage 2: HPC processing...
How to use the tool...



### Stage 3: Final Analysis and Report Generation.

#### Retrieven results

#### Ouputs

#### Etc





