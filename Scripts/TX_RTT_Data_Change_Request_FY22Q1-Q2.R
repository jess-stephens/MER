# PROJECT:  SA-SI-MER
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TX_RTT Data Change Request
# LICENSE:  MIT
# DATE:     2022-06-07
# UPDATED: 
# NOTE:     


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)

# GLOBAL VARIABLES --------------------------------------------------------

#site adjusted dataset
dsd_folderpath <- "Data/DSD TA"
ndoh_folderpath <- "Data/NDOH Import"
fy22files_folderpath <- "Data/Files to be added to FY22Q2 TX_RTT import"

# msd_source <- source_info()
# curr_pd <- source_info(genie_path, return = "period")
# curr_fy <- source_info(genie_path, return = "fiscal_year")
# curr_qtr <- source_info(genie_path, return = "quarter")

# IMPORT ------------------------------------------------------------------


