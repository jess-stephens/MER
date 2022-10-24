# PROJECT:  SA-SI-MER
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TX_TB & PrEP_CT Data Change Request: FY22Q2
# LICENSE:  MIT
# DATE:     2022-09-09
# UPDATED: 
# NOTE:     


# Remember, PrEP_CT has the weird 14 facility thing


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
library(readxl)
library(tierdrop)

# DIRECTORIES -----------------------------------------------------------

dir_setup()

#set folderpaths
ndoh_folderpath <- 'data-raw/NDOH'
reference_folder <- "data-raw/Reference Files"
msd_folder <- "data-raw/MSD-Genie"
import_folder <- "data-raw/Import Files"
validation_folder <- "data-raw/Validation Files"
dsd_folderpath <- "Data/DSD TA"

ndoh_filepath <- ndoh_folderpath %>% glamr::return_latest()
msd_filepath <- msd_folder %>% glamr::return_latest()

#store some locals - change depending on the quarter
fiscal_quarter <- "FY22Q2"
curr_qtr <- "Q2"
kp <- FALSE

import_vars <- c("mech_uid", "orgUnit_uid",	"dataElement_uid",
                 "categoryOptionCombo_uid",	"value",	"period")

dcr_vars <- c("District", "SubDistrict", "Facility", "mech_code", "dataElement",
              "categoryOptionComboName", "value", "period")


# MFL ---------------------------------------------------------------

mfl_new_df <- googlesheets4::read_sheet(tierdrop::mfl_new_id, sheet = "MFL")

ou5_code_join <-mfl_new_df %>% 
  janitor::clean_names() %>% 
  select(ou5uid, old_ou5code)

# need to use the Q2 MFL

#DSD/TA
facility_list <- dsd_folderpath %>% 
  return_latest("MASTER_FACILITY_LIST_05_25_2022_USAID_supported _with DATIM_UIDs") %>% 
  read_csv()

#Reshape facility list
df_fac <- facility_list %>% 
  filter(!is.na(OU2name)) %>% 
  janitor::clean_names() %>%
  dplyr::select(ou5name, ou5uid, datim_uid, tidyselect::starts_with("fy22q2")) %>%
  mutate(ou5uid = case_when(ou5name == "gp Alexandra 18th Avenue Clinic" ~ "x2Kr2DYkN5G",
                            ou5name == "kz St Margaret's Clinic" ~ "NaExSi8bc5v",
                            ou5name == "kz St Margaret's TB MDR Hospital" ~ "H3fntYVRPyN",
         TRUE ~ ou5uid),
         datim_uid = case_when(ou5name == "gp Alexandra 18th Avenue Clinic" ~ "OjWzDVKaNoh",
                               ou5name == "kz St Margaret's Clinic" ~ "HKkmflgo9FR",
                               ou5name == "kz St Margaret's TB MDR Hospital" ~ "rIl9klCXXwH",
                               #ou5anme == "mp Emthonjeni Clinic (Msukaligwa)", 
                               TRUE ~ datim_uid)) %>% 
  left_join(ou5_code_join, by = c("ou5uid")) %>% 
  mutate(old_ou5code = case_when(ou5name == "kz St Margaret's Clinic" ~ 611251,
                                 ou5name == "kz St Margaret's TB MDR Hospital" ~ 736306,
                                 TRUE ~ old_ou5code),
         old_ou5code = as.character(old_ou5code)) %>% 
 # filter(is.na(old_ou5code))
  tidyr::pivot_longer(cols = tidyselect::starts_with("fy22"), names_to = "period",
                      values_to = "DSD_TA") %>% 
  mutate(DSD_TA = case_when(ou5name == "mp Emthonjeni Clinic (Msukaligwa)" ~ "DSD",
                            TRUE ~ DSD_TA)) %>% 

  #view()
  mutate(period = str_sub(period, start = 1, end = 6) %>% toupper()) %>% 
  rename(usaid_facility = ou5name)

# GENIE -------------------------------------------------------------

#read the most recent MSD from the Genie folder
df_genie <- msd_folder %>%
  glamr::return_latest() %>%
  gophr::read_msd()

# MECH ---------------------------------------------------------------

mechs <- glamr::si_path() %>%
  glamr::return_latest("mechs") %>%
  readr::read_csv()

mechs <- mechs %>%
  dplyr::mutate(mech_code = as.character(mech_code))

#Now, we are going to use the mech data from DATIM, join with facility-level mech-data from the MSD
# to get a list of all the facilities reported most recents and the corresponding mech data

# Sometimes, you'll have some extra mechs to encode manually - for example, in FY22Q3, there were
# two MATCH facilities that were not in DATIM yet, so we had to add these manually. If so, select
# `grab_mech_data(df, extra_mechs = TRUE)` and follow the prompt to ensure the data is saved correctly.
# Note - errors will likely be due to file path issues
mech_df <- grab_mech_data(mech_df = mechs, msd_df = df_genie, extra_mechs = TRUE)

# NDOH processing - PREP -------------------------------------------------------------------

kp = FALSE
#Go step by step here

#Import NDOH file and append all the tabs to indicator names - merge into one dataset
ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = FALSE) %>% 
  mutate(Facility = recode(Facility, "gp Freedom Park" = "gp Freedom Park Clinic")) 
ndoh_all_kp <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = TRUE)

#check for facility alignment between MFL and NDOH - okay if they are all Correctional
validate_ndoh(ndoh_all)
validate_ndoh(ndoh_all_kp)


#clean up the NDOH variable names and tidy df
ndoh_clean <- tidy_ndoh(ndoh_all, kp = FALSE)
ndoh_clean_kp <- tidy_ndoh(ndoh_all_kp, kp = TRUE)

ndoh_prep <- ndoh_clean %>% 
  filter(indicator == "PrEP_CT",
         District != "kz Harry Gwala District Municipality") 

ndoh_prep_kp <- ndoh_clean_kp %>% 
  filter(indicator == "PrEP_CT",
         District != "kz Harry Gwala District Municipality")  

# df_validation <- ndoh_processing(filepath = ndoh_filepath, qtr = "Q3", export_type = "Validation", save= FALSE)
# 
# df_validation %>% 
#   filter(indicator =="PrEP_CT")
#Map in dataElements

ndoh_mapped_prep <- ndoh_post_processing(ndoh_prep, kp = FALSE, export_type = "Validation")
ndoh_mapped_prep_kp <- ndoh_post_processing(ndoh_prep_kp, kp = TRUE, export_type = "Validation")

#bind rows and then filter to just testResult
prep_ct_final_testresult <- bind_rows(ndoh_mapped_prep, ndoh_mapped_prep_kp) %>% 
  filter(dataElement_uid %in% c("Y5zUjJ7a5fK", "mdomazTZUg0")) %>% 
  select(dcr_vars)

today <- lubridate::today()

write_csv(prep_ct_final_testresult, glue::glue("data-raw/Validation Files/FY22Q2_PrEPCT_TestResult_{today}.csv"))

# TX_TB ----------------------------------------------------------------------

#clean up the NDOH variable names and tidy df

ndoh_tb <- ndoh_all  %>% 
  filter(indicator %in% c("TX TB_D", "TX TB_D_TestType", "TX TB_D_Pos", "TX TB_D_TestType"))
  
ndoh_join <- df_fac %>% dplyr::left_join(ndoh_tb, by = c(old_ou5code = "Code"))

#Munge and clean up NDOH names
ndoh_clean_tb <- ndoh_join %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX TB_D" = "TX_TB_D",
                                          "TX TB_D_Pos" = "TX_TB_Pos_D",
                                          "TX TB_D_TestType" = "TX_TB_TestType_D"),
                numeratordenom = ifelse(stringr::str_detect(indicator, "_D"), "D", "N"),
                CoarseAgeGroup = ifelse(indicator != "TX_CURR" & CoarseAgeGroup %in% c("50-54", "55-59", "60-64", "65+"),
                                        "50+", CoarseAgeGroup),
                tb_disagg = case_when(indicator == "TX_TB_D" ~ "Age/Sex/TBScreen",
                                      indicator == "TX_TB_Pos_D" ~ "Specimen Return",
                                      indicator == "TX_TB_TestType_D" ~ "Specimen Sent Total"),
                indicator = dplyr::recode(indicator, "TB_PREV_D" = "TB_PREV",
                                          "TB_PREV_N" = "TB_PREV",
                                          "TB_STAT_N" = "TB_STAT",
                                          "TB_STAT_D" = "TB_STAT",
                                          "TX_PVLS_D" = "TX_PVLS",
                                          "TX_PVLS_N" = "TX_PVLS",
                                          "TX_TB_N" = "TX_TB",
                                          "TX_TB_D" = "TX_TB",
                                          "TX_TB_Pos_D" = "TX_TB",
                                          "TX_TB_TestType_D" ="TX_TB"))

#one missing - sex is missing for facility
tb_age_sex <- ndoh_clean_tb %>% 
  filter(tb_disagg == "Age/Sex/TBScreen") %>% 
  select(-c(tb_disagg)) %>% 
  ndoh_post_processing(kp = FALSE, export_type = "Validation")

tb_return <- ndoh_clean_tb %>% 
  filter(tb_disagg == "Specimen Return") %>% 
  select(-c(tb_disagg)) %>% 
  ndoh_post_processing(kp = FALSE, export_type = "Validation") %>% 
  filter(str_detect(dataElement, "Return"))

tb_testtype <- ndoh_clean_tb %>% 
  filter(tb_disagg == "Specimen Sent Total") %>% 
  select(-c(tb_disagg)) %>% 
  ndoh_post_processing(kp = FALSE, export_type = "Validation")

tb_sent <- ndoh_clean_tb %>% 
  filter(tb_disagg == "Specimen Sent Total") %>%
  dplyr::group_by(usaid_facility, ou5uid, datim_uid, old_ou5code, period, DSD_TA,
                  Province, District, SubDistrict, Facility,
                  Sex, CoarseAgeGroup, Result, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")

#grab column names for NDOH
col_names <- tb_sent %>%
  names()

col_names <- col_names[col_names %ni% c("Total")]
group_vars <- c("Sex", "CoarseAgeGroup", "Result", "DSD_TA")
group_vars <- col_names[col_names %ni% c("usaid_facility", "ou5uid", "datim_uid",
                                             'old_ou5code', 'period', 'Province', 'District',
                                             'SubDistrict', 'Facility')]


tb_sent_map <- tb_sent %>% 
  dplyr::left_join(df_map_distinct %>%
                     rename(DSD_TA = `Support Type`) %>% 
                     dplyr::select(-c(`Test Resuts/Outcome/Duration`)) %>%
                     dplyr::filter(stringr::str_detect(dataElement, "Specimen Sent/HIVStatus")), by = c(group_vars)) %>%
  dplyr::distinct() %>%
  dplyr::left_join(mech_df, by = c("datim_uid" = "facilityuid")) %>% 
  rename(orgUnit_uid= datim_uid,
         value = Total) %>%
  select(dcr_vars)


tb_all <- bind_rows(tb_age_sex, tb_return, tb_testtype) %>% 
  filter(!is.na(dataElement)) %>% 
  select(dcr_vars) 

tb_all_final <- bind_rows(tb_all, tb_sent_map)
 # # select(-c(tb_disagg)) %>% 
 #  ndoh_post_processing(kp = FALSE, export_type = "Validation")

# BIND WITH PREP

bind_rows(prep_ct_final_testresult, tb_all_final) %>% 
  write_csv(glue::glue("data-raw/Validation Files/FY22Q2_PrEPCT_TXTB_DCR_{today}.csv"))

write_csv(tb_all_final, glue::glue("data-raw/Validation Files/FY22Q2_TX_TB_D_{today}.csv"))

##################### 

# check TB


ndoh_fac <- unique(ndoh_tb$Facility)
mfl_fac <- unique(ndoh_join$usaid_facility)
setdiff(ndoh_fac, mfl_fac)

tb_age_sex %>% 
  filter(
    #`Test Result/Outcome/Duration` == "Previously On ART",
         Result == "Negative") %>%
  group_by(Province, District,
           #SubDistrict,Facility,
           #`Test Result/Outcome/Duration`,
           #  orgUnit_uid,
           #period,
         indicator) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  filter(`Test Result/Outcome/Duration` != "Other")



