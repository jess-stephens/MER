# PROJECT:  SA-SI-MER
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Q3 MER Processing
# LICENSE:  MIT
# DATE:   2022-06-23
# UPDATED: 2022-07-11
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
library(readxl)
library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------

#site adjusted dataset
dsd_folderpath <- "Data/DSD TA"
ndoh_folderpath <- "Data/NDOH Import"
fy22files_folderpath <- "Data/Files to be added to FY22Q2 TX_RTT import"
template_folderpath <- "Data/Import Files"
genie_folderpath <- "Data/Genie"
data_folder <- "Data/"

import_out_folder <- "Data/Partner Import Files Q3"

genie_path <- genie_folderpath %>% 
  return_latest("Genie_SITE_IM_South_Africa_Frozen")


mfl_gid <- "1J7Jw7RuKDgS4syWlWR-OJjCv73rNVd4eJDkDU3pOxls"

#use new file sent by Mabel
mfl_new_id <- "120sDwX4_tYYVDVpbR6Lc-biPyMJO2weO-UphHcQNnrU"

q3_mfl_id <- '1CqzP-ibx4Ro3i7FstaPwui2q9rVVoJvUuowAlsarcJc'

# msd_source <- source_info()
# curr_pd <- source_info(genie_path, return = "period")
# curr_fy <- source_info(genie_path, return = "fiscal_year")
# curr_qtr <- source_info(genie_path, return = "quarter")

# Set country of interest
cntry <- "South Africa"

# Get OU/Country orgunit uid
uid <- get_ouuid(cntry)

hierachy <-Wavelength::pull_hierarchy(uid, datim_user(), datim_pwd())

# IMPORT ----------------------------------------------------------------

#Read in updated MFL
    # mfl_df <- read_sheet(mfl_gid, sheet = "USAID_MFL")
    # mfl_new_df <- read_sheet(mfl_new_id)
mfl_new_df <- read_sheet(q3_mfl_id)

  #Is this only anova?

#Q2 NDOH FILE
df_q2 <- ndoh_folderpath %>% 
  return_latest("MER Reporting FY22Q2_v1.2_03052022.xlsx")

df_q3 <- ndoh_folderpath %>% 
  return_latest("MER Reporting FY22Q3 v1_21072022")

ndoh_filepath <- ndoh_folderpath %>%
  return_latest("MER Reporting FY22Q3 v1_21072022.xlsx")  


#MER mapping file
mapping_df <- data_folder %>% 
return_latest("MER Mapping Draft 2 62322.xlsx") %>% 
  read_xlsx()

#MER mapping file
mapping_df <- data_folder %>% 
  return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>% 
  read_xlsx()

#MER mapping file
mapping_df <- data_folder %>% 
  return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>% 
  read_xlsx()

# READ NDOH TABS --------------------------------------------------------

#get tab names
tabnames <- ndoh_filepath %>% 
  readxl::excel_sheets()

# ndoh_filepath %>% 
#   readxl::excel_sheets() %>%
#   #setdiff("meta") %>%
# #  stringr::str_subset("CIRG") %>%
#   purrr::map_dfr(.f = ~ readxl::read_excel(ndoh_filepath, sheet = .x, col_types = "text"))

#STORE NAMES --
standard_names <- c("Province", "District", "SubDistrict", "Facility",
                    "Code")
kp_names <- c("KP_Location", "KP_Type")

names_prep_new <- c(standard_names, "Sex", "CoarseAgeGroup", "Total")
names_prep_ct <- c(standard_names, "Sex", "CoarseAgeGroup", "Result", "Total")
names_hts_tst <- c(standard_names, "Test Result/Outcome/Duration" ,"Sex", "CoarseAgeGroup", "HIVTestOfferedIn", "Total")
names_pmtct_eid <- c(standard_names, "CoarseAgeGroup", "Total")
names_tx_new <- c(standard_names,  "Sex", "CoarseAgeGroup", "Total")
names_pmtct_hei_pos <- c(standard_names, "CoarseAgeGroup", "Total")
names_tx_curr <- c(standard_names, "Sex", "CoarseAgeGroup", "Total")
names_tx_rtt <- c(standard_names, "Sex", "CoarseAgeGroup","Test Result/Outcome/Duration", "Total")
names_tx_ml <- c(standard_names, "Test Result/Outcome/Duration", "CoarseAgeGroup", "Sex", "Total")
names_pmtct_art <- c(standard_names, "CoarseAgeGroup", "Test Result/Outcome/Duration", "Total")
names_tb_art <- c(standard_names, "CoarseAgeGroup", "Sex","Test Result/Outcome/Duration", "Total")
names_tx_pvls <- c(standard_names, "Sex", "CoarseAgeGroup", "Total")
names_tb_stat_d <- c(standard_names, "Sex", "CoarseAgeGroup", "Total")
names_tb_stat_n <- c(standard_names, "Test Result/Outcome/Duration", "Sex", "CoarseAgeGroup", "Total")
names_tx_tb_d <- c(standard_names, "Test Result/Outcome/Duration", "CoarseAgeGroup",  "Sex", "Result","Total")
names_tx_tb_n <- c(standard_names, "Test Result/Outcome/Duration", "CoarseAgeGroup",  "Sex","Total")
names_tb_prev <- c(standard_names, "Test Result/Outcome/Duration",  "Sex", "CoarseAgeGroup", "Total")

names_prep_new_kp <- c(names_prep_new, kp_names, "Total")[-8]
names_prep_ct_kp <- c(names_prep_ct,kp_names, "Total")[-9]
names_hts_tst_kp <- c(names_hts_tst,kp_names, "Total")[-10]
names_tx_new_kp <- c(names_tx_new, kp_names, "Total")[-8]
names_tx_curr_kp <- c(names_tx_curr, kp_names, "Total")[-8]
names_tx_ml_kp <- c(names_tx_ml,kp_names, "Total")[-9]
names_tx_pvls_kp <- c(names_tx_pvls, kp_names, "Total")[-8]

names_arvdisp <- c(standard_names, "CoarseAgeGroup", "RegimenCode", "Packs")


#function to read all the tabs and switch names for each indic
read_all_the_things <- function(path, sheet){
  
  col_renamed <- switch(sheet,
                        "PrEP_New" = names_prep_new,
                        "PrEP_CT" = names_prep_ct,
                        "HTS_TST" = names_hts_tst,
                        "PMTCT_EID" = names_pmtct_eid,
                        "TX_NEW" = names_tx_new,
                        "PMTCT_HEI_POS" = names_pmtct_hei_pos,
                        "PMTCT_HEI_POS_ART" = names_pmtct_hei_pos,
                        "TX_CURR" = names_tx_curr,
                        "TX_RTT" = names_tx_rtt,
                        "TX_ML" = names_tx_ml,
                        "PMTCT_ART" = names_pmtct_art,
                        "TB_ART" = names_tb_art,
                        "TX_PVLS_Denom" = names_tx_pvls,
                        "TX_PVLS_Numer" = names_tx_pvls,
                        "TB_STAT_Denom" = names_tb_stat_d,
                        "TB_STAT_Numer" = names_tb_stat_n,
                        "TX TB_D" = names_tx_tb_d,
                        "TX TB_N" = names_tx_tb_n,
                        "TB PREV_D" = names_tb_prev,
                        "TB PREV_N" = names_tb_prev,
                        "PrEP_New_KP" = names_prep_new_kp,
                        "PrEP_CT_KP" = names_prep_ct_kp,
                        "HTS_TST_KP" = names_hts_tst_kp,
                        "TX_NEW_KP" = names_tx_new_kp,
                        "TX_CURR_KP" = names_tx_curr_kp,
                        "TX_ML_KP" = names_tx_ml_kp,
                        "TX_PVLS_Denom_KP" = names_tx_pvls_kp,
                        "TX_PVLS_Numer_KP" = names_tx_pvls_kp,
                        "ARVDISP" = names_arvdisp)
  
  df <- readxl::read_excel(path, sheet, col_names = col_renamed,  col_types = "text", skip =1)
  
 df <-  df %>% 
    mutate(indicator = sheet,
           Total = as.numeric(Total),
           Code = str_replace(Code, ".0$", ""))
  
  return(df)
}

#Use select indicators right now as we sort out SC indicators
  indic_test <- c("PrEP_New","PrEP_CT","HTS_TST", "PMTCT_EID", "TX_NEW","PMTCT_HEI_POS","PMTCT_HEI_POS_ART",
                  "TX_CURR", "TX_RTT", "TX_ML", "PMTCT_ART", "TB_ART", "TX_PVLS_Denom", "TX_PVLS_Numer",
                  "TB_STAT_Denom", "TB_STAT_Numer"
                 # ,"TX TB_D", "TX TB_N", "TB PREV_N", "TB PREV_D"
                  )
  
  ndoh_all <- indic_test %>% 
    #readxl::excel_sheets() %>%
    #setdiff("meta") %>%
    #  stringr::str_subset("CIRG") %>%
    purrr::map_dfr(.f = ~ read_all_the_things(ndoh_filepath, sheet = .x))
  
  #MUNGE NDOH AND MERGE WITH NFL- do we keep HIVTestOfferedIn?
ndoh_all <- ndoh_all %>% 
    select(Province, District, SubDistrict, Facility, Code, 
           `Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result, Total, indicator)




#JOIN MFL AND NDOH ---------------------------------------------------

#get orgunituids
orgunits <- hierachy %>% 
  filter(level == 7) %>% 
  select(orgunit, orgunituid, countryname, snu1, psnu, psnuuid)

#Reshape facility list - for Fy22Q3 list, change to account for DSD/Roving TA
df_fac <- mfl_new_df %>% 
  filter(!is.na(OU2name)) %>% 
  janitor::clean_names() %>% 
 # select(c(1:19), starts_with("FY22")) %>% 
  select(ou5name, ou5uid, datim_uid, ou5code, starts_with("fy22")) %>% 
 # select(OU5name, OU5uid, OU5Code, starts_with("fy22")) %>% 
  pivot_longer(cols = starts_with("fy22"), names_to = "period", values_to = "DSD_TA") %>%
  #view()
  mutate(period = str_sub(period, start = 1, end = 6) %>% toupper(),
         ou5code = as.character(ou5code),
         DSD_TA = ifelse(DSD_TA == "DSD+Roving TA", "DSD", DSD_TA)) %>% 
  rename(usaid_facility = ou5name)

#check if facilities have multiple uids
multiple_facq1 <- df_fac %>% 
  left_join(orgunits, by = c("usaid_facility" = "orgunit")) %>% 
  count(usaid_facility) %>%
  filter(n > 3) %>% 
  pull(usaid_facility)

#filter NDOH to USAID districts - get list from genie
df_genie <- genie_path %>% 
  read_msd()

usaid_district <- df_genie %>% 
  count(funding_agency, snu1, psnu) %>% 
  filter(psnu %ni% c("ec Nelson Mandela Bay Municipality",
                     "gp City of Tshwane Metropolitan Municipality",
                     "gp Ekurhuleni Metropolitan Municipality",
                     "lp Vhembe District Municipality")) %>% 
  pull(psnu)

#now, filter to usaid districts and pull facilities with 4 digit codes
ndoh_all <- ndoh_all %>%
  mutate(District = recode(District,
                           "fs Thabo Mofutsanyana District Municipality" = "fs Thabo Mofutsanyane District Municipality")) %>% 
  filter(District %in% usaid_district) %>% 
  mutate(code_num = str_length(Code)) %>%
  group_by(Province, District, SubDistrict, Facility) %>% 
  arrange(desc(code_num)) %>% 
  left_join(mfl_new_df %>% select(OU5name, OU5Code), by = c("Facility" = "OU5name")) %>% 
  mutate(OU5Code = as.character(OU5Code),
    Code = ifelse(code_num < 6, OU5Code, Code)) %>%
  fill(Code) %>% 
  ungroup() %>% 
 #count(Facility, Code) %>% view()
  select(-c(code_num, OU5Code)) 


#JOIN TO NDOH to bring in period, DSD/TA and then eventually mechs
ndoh_join <- ndoh_all %>% 
  left_join(df_fac, by = c("Code" = "ou5code"))

#join NDOH to MFL
ndoh_join2 <- df_fac %>% 
  left_join(ndoh_all,  by = c("ou5code" = "Code")) 

#CHECK******
#what facilities are in NDOH but not in MFL?
ndoh_code <- unique(ndoh_all$Code)
mfl_code <- unique(df_fac$ou5code)
setdiff(ndoh_code, mfl_code)

#MAP TO DATA ELEMENT WITH MER MAPPING ----------------------------------------

#create indicator and N/D variable in mapping file
df_map_clean <- mapping_df %>% 
 # count(`Datim UID`) %>% 
  mutate(indicator = str_extract(`Datim UID`, "[^ (]+"),
         numeratordenom = str_extract(`Datim UID`, "[^,]+"),
         numeratordenom = str_sub(numeratordenom, start= -1),
         CoarseAgeGroup = ifelse(str_detect(`Data Element`, "TX_RTT") & CoarseAgeGroup %in% c("50-54", "55-59", "60-64", "65+"),
                                 "50+", CoarseAgeGroup),
         DSD_TA = str_split(`Datim UID`, ", ") %>% unlist() %>% nth(2),
         indicator = ifelse(indicator == "PMTCT_HEI_POS" & dataElement == "PMTCT_HEI_POS (N, DSD, Age/HIVStatus/ARTStatus): Infant Testing",
                            "PMTCT_HEI_POS_ART", indicator)) %>% 
  select(-c(Total))



#now address NDOH file numdenom stuff
remove <- c("$_N", "$_D")

ndoh_clean2 <- ndoh_join2 %>%
  mutate(indicator = recode(indicator, "PrEP_New" = "PrEP_NEW",
                            "TB PREV_D" = "TB_PREV_D",
                            "TB PREV_N" = "TB_PREV_N",
                            "TB_STAT_Denom" = "TB_STAT_D",
                            "TB_STAT_Numer" = "TB_STAT_N",
                            "TX TB_D" = "TX_TB_D",
                            "TX TB_N" = "TX_TB_N",
                            "TX_PVLS_Denom" = "TX_PVLS_D",
                            "TX_PVLS_Numer" = "TX_PVLS_N"),
         numeratordenom = ifelse(str_detect(indicator, "_D"), "D", "N"),
         CoarseAgeGroup = ifelse(indicator != "TX_CURR" & CoarseAgeGroup %in% c("50-54", "55-59", "60-64", "65+"),
                                 "50+", CoarseAgeGroup),
         indicator = recode(indicator, "TB_PREV_D" = "TB_PREV",
                            "TB_PREV_N" = "TB_PREV",
                            "TB_STAT_N" = "TB_STAT",
                            "TB_STAT_D" = "TB_STAT",
                            "TX_PVLS_D" = "TX_PVLS",
                            "TX_PVLS_N" = "TX_PVLS",
                            "TX_TB_N" = "TX_TB",
                            "TX_TB_D" = "TX_TB"))

#address over50 disagg issue
 ndoh_over50 <- ndoh_clean2 %>% 
   filter(CoarseAgeGroup == "50+") %>% 
  group_by(usaid_facility, ou5uid, datim_uid, ou5code, period, DSD_TA,
           Province, District, SubDistrict, Facility, `Test Result/Outcome/Duration`,
           Sex, CoarseAgeGroup, Result, indicator, numeratordenom) %>% 
  summarise(across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")
 
 ndoh_clean2 <- ndoh_clean2 %>% 
   filter(CoarseAgeGroup != "50+") %>% 
   bind_rows(ndoh_over50)


df_map_distinct <- df_map_clean %>% distinct(`Test Resuts/Outcome/Duration`, Sex, CoarseAgeGroup, Result, indicator, numeratordenom,
                                             dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, `Support Type`)

additional_map <- getwd() %>% 
  return_latest("additional-disagg-mapping-fy22q3") %>% 
  read_xlsx()

additional_map <- additional_map %>% 
  select(`Test Resuts/Outcome/Duration`, Sex, CoarseAgeGroup, Result, dataElement, dataElement_uid,
         categoryOptionComboName, categoryOptionCombo_uid, DSD_TA, indicator, numeratordenom) %>% 
  distinct() %>% 
  rename(`Test Resuts/Outcome/Duration` = `Test Resuts/Outcome/Duration`,
         `Support Type` = DSD_TA)
  

df_map_distinct <- df_map_distinct %>% bind_rows(additional_map)


ndoh_mapped <- ndoh_clean2 %>% 
  left_join(df_map_distinct, by = c("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                                 "Sex", "CoarseAgeGroup", "Result", "indicator", "numeratordenom", "DSD_TA" = "Support Type"))
  


# now pull mech codes from MSD ------------------

library(Wavelength)
library(curl)

mechs <- pull_mech2() #run from mech script

mech_xwalk <- mechs %>% 
  filter(operatingunit == "South Africa",
         mech_code %in% c(70310, 70287, 81902, 70290, 70301))

msd_mechs <- df_msd %>% 
  filter(funding_agency == "USAID",
         fiscal_year == 2022,
         mech_code %in% c(70310, 70287, 81902, 70290, 70301)) %>% 
  count(sitename, facilityuid, mech_code, mech_name)

#join mech metadata
ndoh_mapped <- ndoh_mapped %>% 
  left_join(msd_mechs, by = c("datim_uid" = "facilityuid")) %>% 
  left_join(mech_xwalk, by = c('mech_code'))
  
  ndoh_mapped <- ndoh_mapped %>% 
  select(-c(mech_name.y)) %>% 
  rename(mech_name = mech_name.x)
  
  #Filter NA's for District
  
  ndoh_mapped %>% 
    filter(!is.na(Facility))




#Figure out what is not getting mapped ---
not_mapped <- ndoh_mapped %>% 
  filter(is.na(dataElement)) %>% 
  count(`Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result, indicator, numeratordenom, DSD_TA, dataElement)

# DISAGG COMBOS - troubleshoot
  #combinations msising
  #DSD_TA missing
  #facilities with DSD and Roving TA


 missing_disagg2 <-  ndoh_mapped %>% 
  filter(is.na(dataElement),
         !is.na(District)) %>%
  select(indicator, DSD_TA, District, Facility, Sex, CoarseAgeGroup, `Test Result/Outcome/Duration`,
         Result, numeratordenom, dataElement)

missing_disagg <- missing_disagg %>% 
  select(indicator, Sex, CoarseAgeGroup, Result, `Test Result/Outcome/Duration`, numeratordenom, DSD_TA)



fix_disagg <- ndoh_clean2 %>% distinct(indicator, Sex, CoarseAgeGroup, Result,
                                       `Test Result/Outcome/Duration`, numeratordenom, DSD_TA) %>% 
  semi_join(missing_disagg2) 

write_csv(fix_disagg, "Dataout/FY22Q3-distinct-disagg-20220724.csv")

write_csv(not_mapped, "Dataout/Q3_datimuid_unmapped.csv")


# TO-DO -------------------------------------------------------------------------

# BREAK INTO PARTNER IMPORT FILES - MFL only has ANOVA?

str(ndoh_mapped)

validation_file <- ndoh_mapped %>% 
  select(period,Province, District,SubDistrict, Facility, datim_uid,
         mech_name, mech_code, mech_uid, primepartner, indicator, numeratordenom,
         `Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result,
         dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Total) %>% 
  rename(value = Total,
        # mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid)

validation_file_bmw <- ndoh_mapped %>% 
  select(period, primepartner, mech_uid, Province, District,SubDistrict, Facility, datim_uid, indicator,
         dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Total) %>% 
  rename(value = Total,
     #    mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid)


import_file_clean <- ndoh_mapped %>% 
  select(mech_code, mech_uid, Facility, datim_uid, dataElement, dataElement_uid, categoryOptionCombo_uid,
         Total, SubDistrict, District) %>% 
  rename(value = Total,
         #mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid,
         orgUnitName = Facility)

write_csv(import_file_clean, "Dataout/test-q2-import-file-readyforqc.csv")
write_csv(validation_file_bmw, "Dataout/test-q2-import-file-readyforqc-bmw.csv")
write_csv(validation_file, "Dataout/FY22Q2_combined_import_ready-for-QC-WITHDISAGGS.csv")


ndoh_clean2 %>% 
  count(mechanism_i_d)

mfl_df %>% 
  count(Partner, `Mechanism I.D`)

#DEAL WITH KP TABS -----------------------------------------------------

#how to map dataElement and categoryoptioncombo? we will have to do manually

kp_indic <- c("PrEP_New_KP", "PrEP_CT_KP", "HTS_TST_KP", "TX_NEW_KP", 'TX_CURR_KP', "TX_ML_KP",
              "TX_PVLS_Denom_KP", "TX_PVLS_Numer_KP")

ndoh_kp <- kp_indic %>% 
  #readxl::excel_sheets() %>%
  #setdiff("meta") %>%
  #  stringr::str_subset("CIRG") %>%
  purrr::map_dfr(.f = ~ read_all_the_things(ndoh_filepath, sheet = .x)) %>% 
  select(Province, District, SubDistrict, Facility, Code, 
         `Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result, KP_Location, KP_Type, Total, indicator)

ndoh_kp <- ndoh_kp %>% 
  filter(District %in% usaid_district) %>% 
  mutate(code_num = str_length(Code)) %>% 
  group_by(Province, District, SubDistrict, Facility) %>% 
  arrange(desc(code_num)) %>%
  left_join(mfl_new_df %>% select(OU5name, OU5Code), by = c("Facility" = "OU5name")) %>% 
  mutate(OU5Code = as.character(OU5Code),
    Code = ifelse(code_num < 6, OU5Code, Code)) %>%
  fill(Code) %>% 
  ungroup() %>% 
  # count(Facility, Code) %>% view()
  select(-c(code_num, OU5Code)) 

#Aggregate across KP groups
ndoh_kp_agg <- ndoh_kp %>% 
  group_by(Province, District, SubDistrict, Facility, Code, `Test Result/Outcome/Duration`,
           Sex, CoarseAgeGroup, Result, indicator) %>% 
  summarise(across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop") 


#join NDOH to MFL
ndoh_join_kp <- df_fac %>% 
  left_join(ndoh_kp_agg,  by = c("ou5code" = "Code"))

#CHECK******
#what facilities are in NDOH but not in MFL?
ndoh_kp_code <- unique(ndoh_kp$Code)
mfl_code <- unique(df_fac$ou5code)
setdiff(ndoh_kp_code, mfl_code)

#now address NDOH file numdenom stuff
remove <- c("$_N", "$_D")

ndoh_kp_clean <- ndoh_join_kp %>% 
  mutate(indicator = str_replace(indicator, "_KP", "")) %>%
  mutate(indicator = recode(indicator, "PrEP_New" = "PrEP_NEW",
                            "TX_PVLS_Denom" = "TX_PVLS_D",
                            "TX_PVLS_Numer" = "TX_PVLS_N"),
         numeratordenom = ifelse(str_detect(indicator, "_D"), "D", "N"),
         CoarseAgeGroup = ifelse(indicator != "TX_CURR" & CoarseAgeGroup %in% c("50-54", "55-59", "60-64", "65+"),
                                 "50+", CoarseAgeGroup),
         indicator = recode(indicator,
                            "TX_PVLS_D" = "TX_PVLS",
                            "TX_PVLS_N" = "TX_PVLS"))

#address over50 disagg issue
ndoh_kp_over50 <- ndoh_kp_clean %>% 
  filter(CoarseAgeGroup == "50+") %>% 
  group_by(usaid_facility, ou5uid, datim_uid, ou5code, period, DSD_TA,
           Province, District, SubDistrict, Facility, `Test Result/Outcome/Duration`,
           Sex, CoarseAgeGroup, Result, indicator, numeratordenom) %>% 
  summarise(across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")

ndoh_kp_clean <- ndoh_kp_clean %>% 
  filter(CoarseAgeGroup != "50+") %>% 
  bind_rows(ndoh_kp_over50)

#map to mapping file
ndoh_kp_mapped <- ndoh_kp_clean %>% 
  left_join(df_map_distinct, by = c("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                                    "Sex", "CoarseAgeGroup", "Result", "indicator", "numeratordenom", "DSD_TA" = "Support Type"))

#join mech metadata to KP data
ndoh_kp_mapped <- ndoh_kp_mapped %>% 
  left_join(msd_mechs, by = c("datim_uid" = "facilityuid")) %>% 
  left_join(mech_xwalk, by = c('mech_code'))

ndoh_kp_mapped <- ndoh_kp_mapped %>% 
  select(-c(mech_name.y)) %>% 
  rename(mech_name = mech_name.x) 

#NOW SAVE VALIDATION FILE OF JUST KP

kp_bmw_validation_file <- ndoh_kp_mapped %>% 
  select(period, primepartner, mech_uid, Province, District,SubDistrict, Facility, datim_uid, indicator,
         dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Total) %>% 
  rename(value = Total,
         #    mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid) %>% 
  filter(!is.na(Province) & !is.na(District) & !is.na(SubDistrict)& !is.na(Facility))


kp_validation_file <- ndoh_kp_mapped %>% 
  select(period,Province, District,SubDistrict, Facility, datim_uid,
         mech_name, mech_code, mech_uid, primepartner, indicator, numeratordenom,
         `Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result,
         dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Total) %>% 
  rename(value = Total,
         # mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid)

import_file_clean_kp <- ndoh_kp_mapped %>% 
  select(mech_code, mech_uid, Facility,dataElement, datim_uid, dataElement_uid, categoryOptionCombo_uid,
         Total, SubDistrict, District) %>% 
  rename(value = Total,
         #mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid,
         orgUnitName = Facility)

write_csv(kp_bmw_validation_file, "FY22Q2_import_file_KP_TABS_QC.csv")
write_csv(kp_validation_file, "FY22Q2_import_file_KP_TABS_QC-WITHDISAGGS.csv")


#ARVDISP - use Given's file -----------------------------------------------------

#MER mapping file
arvdisp_mapping <- data_folder %>% 
  return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>% 
  read_xlsx(sheet= "ARVDispense")

df_arvdisp <- readxl::read_excel(ndoh_filepath, sheet = "ARVDISP")

#now, filter to usaid districts and pull facilities with 4 digit codes
df_arvdisp <- df_arvdisp %>%
  filter(District %in% usaid_district) %>%
  mutate(code_num = str_length(Code)) %>% 
  group_by(Province, District, Community, Facility) %>% 
  arrange(desc(code_num)) %>%
  left_join(mfl_new_df %>% select(OU5name, OU5Code), by = c("Facility" = "OU5name")) %>% 
  mutate(Code = ifelse(code_num < 6, OU5Code, Code)) %>%
  fill(Code) %>% 
  ungroup() %>% 
  # count(Facility, Code) %>% view()
  select(-c(code_num, OU5Code)) %>% 
  mutate(Code = as.character(Code))

#join NDOH to MFL
join_arvdisp <- df_fac %>% 
  left_join(df_arvdisp,  by = c("ou5code" = "Code"))

#CHECK******
#what facilities are in NDOH but not in MFL?
arvdisp_code <- unique(df_arvdisp$Code)
mfl_code <- unique(df_fac$ou5code)
setdiff(arvdisp_code, mfl_code)

df_arv_clean <- join_arvdisp %>% 
  mutate(indicator = "SC_ARVDISP",
         RegimenCode = recode(RegimenCode, "13.0" = "13")) 

  #create indicator and N/D variable in mapping file
  arv_map_clean <- arvdisp_mapping %>% 
    # count(`Datim UID`) %>% 
    mutate(indicator = str_extract(dataElement, "[^ (]+")) %>% 
    select(-c(Packs))

  #now map
  arv_map_distinct <- arv_map_clean %>% distinct(CoarseAgeGroup, RegimenCode, indicator,
                                               dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid)
  
  df_arv_mapped <- df_arv_clean %>%
    left_join(arv_map_distinct, by = c("CoarseAgeGroup", "RegimenCode", "indicator"))
  
  #NOT MAPPED ---
  arv_not_mapped <- df_arv_mapped %>% filter(is.na(dataElement), !is.na(Packs)) %>% 
    select(indicator, DSD_TA, District, Facility, CoarseAgeGroup, RegimenCode, dataElement)
  
  write_csv(arv_not_mapped, "Dataout/arv-distinct-missing-disaggs.csv")
  
 df_arv_final <-  df_arv_mapped %>% 
  left_join(msd_mechs, by = c("datim_uid" = "facilityuid")) %>% 
    left_join(mech_xwalk, by = c('mech_code')) %>% 
   select(-c(mech_name.y)) %>% 
   rename(mech_name = mech_name.x) 
 
 my_arv_validation <- df_arv_final %>% 
   select(period,Province, District, Community, Facility, datim_uid,
          mech_name, mech_code, mech_uid, primepartner, indicator,
          CoarseAgeGroup, RegimenCode,
          dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Packs) %>% 
   rename(SubDistrict = Community,
          value = Packs,
          orgUnit_uid = datim_uid)
 
 import_file_clean_arv <- df_arv_final %>% 
   select(mech_code, mech_uid, Facility, datim_uid, dataElement , dataElement_uid, categoryOptionCombo_uid,
          Packs, Community, District) %>% 
   rename(value = Packs,
          SubDistrict = Community,
          #mech_uid = mechanism_uid,
          orgUnit_uid = datim_uid,
          orgUnitName = Facility)
 
# SPLIT INTO PARTNER FILES ----------------------------------------------------------------- 
 
 #Exclude ARVDISP for now
 final_validation_Q3 <-  bind_rows(import_file_clean, import_file_clean_kp)
 final_checks_Q3 <-  bind_rows(validation_file, kp_validation_file)
 
 write_csv(final_checks_Q3, "Dataout/Partner Import Files/FY22Q3_ALL_PARTNER_import.csv.csv")
 
 
 
 
 #BROADREACH
 FY22Q3_Broadreach_import <- final_validation_Q3 %>% 
   filter(!is.na(dataElement)) %>% 
   filter(mech_code == 70287)
 
 #RTC
 FY22Q3_RTC_import <- final_validation_Q3 %>% 
   filter(!is.na(dataElement)) %>% 
   filter(mech_code == 70290)
 
 #WRHI
 FY22Q3_WRHI_import <- final_validation_Q3 %>% 
   filter(!is.na(dataElement)) %>% 
   filter(mech_code == 70301)
 
 #ANOVA
 FY22Q3_ANOVA_import <- final_validation_Q3 %>% 
   filter(!is.na(dataElement)) %>% 
   filter(mech_code == 70310)
 
 #MATCH
 FY22Q3_MATCH_import <- final_validation_Q3 %>%
   filter(!is.na(dataElement)) %>% 
   filter(mech_code == 81902)
 
 
 write_csv(FY22Q3_ANOVA_import, "Dataout/Partner Import Files/FY22Q3_ANOVA_import.csv")
 write_csv(FY22Q3_MATCH_import, "Dataout/Partner Import Files/FY22Q3_MATCH_import.csv")
 write_csv(FY22Q3_WRHI_import, "Dataout/Partner Import Files/FY22Q3_WRHI_import.csv")
 write_csv(FY22Q3_RTC_import, "Dataout/Partner Import Files/FY22Q3_RTC_import.csv")
 write_csv(FY22Q3_Broadreach_import, "Dataout/Partner Import Files/FY22Q3_BroadReach_import.csv")
 
 
 
 
 
 
 #Add to validation files
 
 arv_validaton_bmw <- df_arv_final %>% 
   select(period, primepartner, mech_uid, Province, District,Community, Facility, datim_uid, indicator,
          dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Packs) %>% 
   rename(SubDistrict = Community,
          value = Packs,
          orgUnit_uid = datim_uid)
 
final_validation_bmw <-  bind_rows(validation_file_bmw, arv_validaton_bmw)
 
 
arv_import <- df_arv_final %>% 
   select(mech_uid, datim_uid, dataElement_uid, categoryOptionCombo_uid, Packs) %>% 
   rename(value = Packs,
          #mech_uid = mechanism_uid,
          orgUnit_uid = datim_uid)

bind_rows(import_file_clean, arv_import)
  

# ------------------------------------------------

#re-do mapping with new file

#TX TB?


#VALIDATIONS -------------------------------------------------------------------


