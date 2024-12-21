# MoFuSS local installation

Explore global geospatial results and access comprehensive documentation by visiting [MoFuSS's official website](https://www.mofuss.unam.mx/).

For questions or feedback, please contact mofussfreeware@gmail.com ✨

## MoFuSS can be run in two ways:
**1.- Web-Based Interface:** This is the simplest option as it requires no installation. However, it is still under development. To check for updates, [join our MoFuSS group](groups.google.com/g/mofuss) or visit www.mofuss.unam.mx under the "User-Defined Scenarios" or "MoFuSS-US" section.

**2.- Local Installation:** This option allows you to run MoFuSS directly on your local computer. However, it's important to note that at least one module must be executed on a High-Performance Computing (HPC) cluster, as it relies on parallel C++ algorithms. This document provides a step-by-step guide for installing MoFuSS locally. 📚

## Install and run MoFuSS Locally

MoFuSS uses Google Earth Engine for data download, R for data preparation and graphs, C++ for harvest probability maps, DINAMICA EGO for geospatial simulations (core scripts), ffmpeg for creating animations, LaTeX for formatting and generating final reports

* Stage 0: Data Download. If you already have all the necessary datasets, you can skip this stage and proceed directly to Stage 1.  
* Stage 1: Local Data Processing. Seven sequential scripts are run on your local machine to generate Stage 1 results. 
* Stage 2: A subset of Stage 1 results are then sent to the High-Performance Computing (HPC) cluster, where woodfuel harvest probability maps are built. Once all the geoprocessing is complete, you need to download the results into your local machine for further processing. 
* Stage 3: Final Analysis and Report Generation. Results from Stages 1 and 2 are used on your local machine to generate the final outputs, including summary results and reports formatted using LaTeX.

[**MoFuSS can send progress updates directly to your Telegram account, eliminating the need for constant monitoring. However, we are unsure about the security of this approach.**]

### Stage 0: Data Download

If you wish to download all or some MoFuSS datasets from scratch, or replace specific MoFuSS datasets with those you've produced, please refer to the MoFuSS repository: [gee2mofuss](https://github.com/mofuss/gee2mofuss). [**Note that we are still documenting this repository, please contact aghilardi@ciga.unam.mx in case you need support**]

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
* Download into your local hard drive the three folders needed to run MoFuSS locally, available from our [Zenodo repository.](https://zenodo.org/records/14517562)
* Uncompress demand and admin_regions avoiding to create unnecesarry fodler
* Create local folder were you expect to run MoFuSS, e.g. "MoFuSS_Malawi_1km"; unzippe LULCC.zio inside this folder; once again, preventing of creating uneccesarry subfolders e.g. LULCC/LULCC
*
* and save them directly in the C or D drive. [**Note for Adrian to expand this section, link to Stage 0 and better explain the 5 folders structure**]
* 
   * admin_regions
      * Geographical information
   * demand_in
      * Data on the fuel demand
   * world_1000m_yyyymmdd
      * Contains mapping data at specified resolution corresponding to the analysis precision level (1000m here)
   * emissions
   * rTemp

#### Running Stage 1
All R scripts are written to run on Windows and Linux, and would need to be modified to run on MacOS.
* The sequence of scripts for this stage is the following:
    * 1. `localhost/scripts/0_set_directories_and_region.R`
         * Use the hardwired option in the script to set these paths to the locations you selected for the 5 folders in the above step.
    * 2. `localhost/scripts/1_erase_all_win.R`
    * 3. `localhost/scripts/2_copy_files.R`
    * 4. `localhost/scripts/3_demand4IDW_v3.R`
    * 5. `localhost/scripts/4_produce_growth_and_stock_csv.R`
    * 6. `localhost/scripts/5_harmonizer_v1.R`
    * 7. `localhost/scripts/6_scenarios.R`

### Stage 2: HPC processing...
How to use the tool...

Explain parameters...

### Stage 3: Final Analysis and Report Generation.
#### Dinamica EGO core scripts...

a

b

c


#### Retrieven results

#### Ouputs

#### Etc





