
******************

### **Welcome to the New MoFuSS GitHub Account!**

As of November 2024, we’ve officially migrated from the MoFuSS GitLab account to this GitHub platform. Please note that the GitLab account will be permanently erased soon to avoid any confusion.

Stay tuned for updates, and thank you for your support as we continue to grow!

MoFuSS uses Google Earth Engine for data download, R for data preparation and graphs, C++ for harvest probabiility maps, DINAMICA EGO for geospatial simulations (core scripts), ffmpeg for animations, and LaTeX for final reports.

* This project uses Google Earth Engine for data download, R for data preparation and graphs, C++ for harvest probabiility maps, DINAMICA EGO for geospatial simulations (core scripts), ffmpeg for animations, and LaTeX for final reports.
* 6 sequential scripts are run on your local machine to generate the stage-1 results.
* Stage-1 results are sent to the HPC cluster for the geospatial simulations and sent back to your local machine.
* Stage-2 results are used on your local machine to generate the final results and reports, formatted in LaTeX.
* As of November 25, 2024, the project does not include a license, effectively making it closed source. While others can view the code, they cannot legally use, modify, or distribute it. We plan to reopen the project after completing the development of the web-MoFuSS interface. This interface will provide the most user-friendly way to run MoFuSS with your own parameters on our HPC clusters—no installation required.


## Resources
* Explore global geospatial results and access comprehensive documentation by visiting [MoFuSS's official website](https://www.mofuss.unam.mx/).
* For questions or feedback, please contact mofussfreeware@gmail.com :sparkles:

## Getting started to use MoFuSS on the UNAM HPC cluster

### Preparing your environment
* All scripts are written to run on Windows, and would need to be modified to run on Linux/MacOS.
* Install R and RStudio on your local machine.
* Clone this repository to your local machine.
* Install the `remotes` package in R: `install.packages("remotes")`
* Install the following packages in R:
    * Use pacman if desired:
    ```
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(readr, dplyr, fasterize, glue, igraph, raster, rgl, sf, tictoc, stars, gitlabr, inline, tidyverse, spam, svDialogs, terra, readxl)
    ```
* Obtain the three folders needed to run the R script locally and save them directly in the C or D drive.
    * admin_regions
        * Geographical information
    * demand_in
        * Data on the fuel demand
    * world_1000m_yyyymmdd
        * Contains mapping data at specified resolution corresponding to the analysis precision level (1000m here)


### Running MoFuSS on the UNAM HPC cluster
* The sequence of scripts to run MoFuSS on the UNAM HPC cluster is the following:
    * 1. `scripts/0_set_directories_and_region.R`
         * Use the hardwired option in the script to set these paths to the locations you selected for the 3 folders in the above step.
    * 2. `scripts/1_erase_all_win.R`
    * 3. `scripts/3_demand4IDW_v1.R`
    * 4. `scripts/3_demand4IDW_v2.R`
    * 5. `scripts/3debug_demand4IDW_v1_optional.R`
    * 6. `scripts/5_harmonizer.R`
    * 7. `scripts/6_scenarios.R`



## Gettings started to contribute
