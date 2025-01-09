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

If you prefer to run 1km simulations for nearly any country or region in the Global South using the provided datasets (recommended), proceed directly to **Stage 1.**

Alternatively, if you wish to download all or some MoFuSS datasets from scratch, or replace specific MoFuSS datasets with those you've produced, please refer to the MoFuSS repository: [gee2mofuss](https://github.com/mofuss/gee2mofuss). [**Note that we are still working and documenting this repository, please contact aghilardi@ciga.unam.mx in case you need support**]

### Stage 1: Local Data Processing
#### Preparing your environment: RStudio

* Install R and RStudio on your local machine.
* Clone this repository to your local machine.
* Install the `remotes` package in R: `install.packages("remotes")`
* Install the following packages in R:
    * Use pacman if desired:
    ```
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(dplyr, fasterize, fs, gdata, gitlabr, glue, hacksaw, igraph, inline, mapview, purrr, raster, readr, readxl, rgl, rlang, rmapshaper, rprojroot, rstudioapi, sf, spam, stars, stringr, svDialogs, tcltk, terra, tibble, tictoc, tidyterra, tidyverse)
    ```
    Please note that `rmapshaper` needs [Node.js](https://nodejs.org/) to be installed.
  
#### Preparing your environment: Downloading all datasets

* Download the three folders needed to run MoFuSS locally from our [Zenodo Repository](https://zenodo.org/records/14517562) and save them to your local hard drive.
* Extract the contents of the "demand" and "admin_regions" folders directly onto your local hard drive. Avoid creating unnecessary subfolders, such as demand/demand/...
* Create a local folder where you plan to run MoFuSS, e.g., MoFuSS_Malawi_1km. Extract the LULCC.zip file into this folder, ensuring that no unnecessary subfolders are created (e.g., avoid LULCC/LULCC/...).
* Within your local hard drive, create two empty folders named "emissions" and "rTemp".
* Ideally, if possible, all five folders should be at the same level within your hard drive, similar to the following **example**:
   * 1.- `D:/MoFuSS_Malawi_1km/LULCC/`
   * 2.- `D:/admin_regions`
   * 3.- `D:/demand`
   * 4.- `D:/emissions`
   * 5.- `D:/rTemp`

#### Preparing your environment: Setting up parameters
* Within your local github folder, there are a few parameter tables in .csv and .xlsx: `mofuss/localhost/selected_parameters/`.
* When running 1km Global South simulations, you'll need to check the one named: "_parameters_world1000m_" and adjust all parameters accordingly to your preferences.
* Please pay particular attention to the Area of Interest (AoI) of your choice.

#### Running Stage 1
* All R scripts are written to run on Linux and Windows, and would need to be modified to run on MacOS. The sequence of scripts for this stage is the following:
   * 1.- `mofuss/localhost/scripts/0_set_directories_and_region_v3.R`
   * 2.- `mofuss/localhost/scripts/1_erase_all_v1.R`
   * 3.- `mofuss/localhost/scripts/2_copy_files_v1.R`
   * 4.- `mofuss/localhost/scripts/3_demand4IDW_v3.R`
   * 5.- `mofuss/localhost/scripts/4_produce_growth_and_stock_csv.R`
   * 6.- `mofuss/localhost/scripts/5_harmonizer_v1.R`
   * 7.- `mofuss/localhost/scripts/6_scenarios.R`
 
It is needed to run the following script **just once in each new node** (i.e. new computer): `mofuss/localhost/scripts/preprocessing4globaldatasets/1apre_GADM_admin_wp_v5.R`. It should be run after 1.- `mofuss/localhost/scripts/0_set_directories_and_region_v3.R`, and is not linked to any Area of Interest; it just prepares the administrative layers for any 1000m or 100m simulation to be run. 

### Stage 2: HPC processing
* After successfully running all the R scripts, upload the resulting files to [MoFuSS's IDW online tool](https://www.mofuss.unam.mx/idw/) hosted on our HPC clusters.
* Complete all parameters requested or leave the default ones. Alternatively, if you are operating your own HPC cluster, refer to the [CostDistance repository](https://github.com/mofuss/CostDistance_IDW) for detailed instructions.
* Files to be ingested by the Cost Distance code:
   * 1.- `YourMoFuSSWorkingDirectory/In/fricc_v.tif`
   * 2.- `YourMoFuSSWorkingDirectory/In/fricc_w.tif`
   * 3.- `YourMoFuSSWorkingDirectory/In/locs_c_v.tif`
   * 4.- `YourMoFuSSWorkingDirectory/In/locs_c_w.tif`
   * 5.- `YourMoFuSSWorkingDirectory/In/DemandScenarios/BaU_fwch_v.csv`
   * 6.- `YourMoFuSSWorkingDirectory/In/DemandScenarios/BaU_fwch_w.csv`

### Stage 3: Final Analysis and report generation.
#### Preparing your environment: Setting up LaTeX for final reports
* Download and Install MiKTeX. Visit the [MiKTeX download page](https://miktex.org/download) and download the installer suitable for your operating system.
* Run the Installer. During installation, ensure you select the option to allow MiKTeX to install missing packages automatically. This will enable it to download and install libraries locally when required.
* Verify Installation. Open a terminal or command prompt and type pdflatex --version to confirm MiKTeX is correctly installed.
* Optional: Configure Updates. Open the MiKTeX Console, navigate to the "Updates" section, and ensure that your installation is up to date.

#### Preparing your environment: Setting up Dinamica EGO
* Install Dinamica EGO. It is recommended to use the older version available in the [Zenodo Repository](https://zenodo.org/records/14517562)
* Open Dinamica EGO Wizard
* Open and run the file located at: `YourMoFuSSWorkingDirectory/7_FW_dyn_lulcc_Sc16b_luc1.egoml`, and follow the provided instructions on the HTML interface.

#### Retrieve results
All output files will be saved in: `YourMoFuSSWorkingDirectory/OutBaU/`

### Changing your area of interest
When changing the Area of Interest you might keep working on the same MoFuSS directory in which case all previous results will be overwritten. Or just create a new folder to run the new simulations.
* 1.- You first need to adust the parameters in `YourMoFuSSWorkingDirectory/LULCC/DownloadedDatasets/SourceData*/parameters_*.csv (or xlsx)` accordingly.
* 2.- Re-run from Stage 1 but starting in script # 4.- `mofuss/localhost/scripts/3_demand4IDW_v3.R`

👀 If you have any feedback on this documentation, please feel free to reach out to us at: aghilardi@ciga.unam.mx.
