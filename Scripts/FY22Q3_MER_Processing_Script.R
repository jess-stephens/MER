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

genie_path <- genie_folderpath %>% 
  return_latest("Genie_SITE_IM_South_Africa_Frozen")


mfl_gid <- "1J7Jw7RuKDgS4syWlWR-OJjCv73rNVd4eJDkDU3pOxls"

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
mfl_df <- read_sheet(mfl_gid, sheet = "USAID_MFL")

  #Is this only inova?

#Q2 NDOH FILE
df_q2 <- ndoh_folderpath %>% 
  return_latest("MER Reporting FY22Q2_v1.2_03052022.xlsx")

ndoh_filepath <- ndoh_folderpath %>%
  return_latest("MER Reporting FY22Q2_v1.2_03052022.xlsx")  


#MER mapping file
mapping_df <- data_folder %>% 
return_latest("MER Mapping Draft 2 62322.xlsx") %>% 
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
                        "TB PREV_N" = names_tb_prev)
  
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
                  "TB_STAT_Denom", "TB_STAT_Numer", "TX TB_D", "TX TB_N", "TB PREV_N", "TB PREV_D")
  
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

#Reshape facility list
df_fac <- mfl_df %>% 
  filter(!is.na(OU2name)) %>% 
  janitor::clean_names() %>% 
 # select(c(1:19), starts_with("FY22")) %>% 
  select(partner, mechanism_i_d, mechanism_uid,ou5name, ou5uid, datim_uid, ou5code, starts_with("fy22")) %>% 
 # select(OU5name, OU5uid, OU5Code, starts_with("fy22")) %>% 
  pivot_longer(cols = starts_with("fy22"), names_to = "period", values_to = "DSD_TA") %>% 
  #view()
  mutate(period = str_sub(period, start = 1, end = 6) %>% toupper(),
         ou5code = as.character(ou5code)) %>% 
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
  filter(District %in% usaid_district) %>% 
  mutate(code_num = str_length(Code)) %>%
  group_by(Province, District, SubDistrict, Facility) %>% 
  arrange(desc(code_num)) %>%
  left_join(mfl_df %>% select(OU5name, OU5Code), by = c("Facility" = "OU5name")) %>% 
  mutate(Code = ifelse(code_num < 6, OU5Code, Code)) %>%
  fill(Code) %>% 
  ungroup() %>% 
# count(Facility, Code) %>% view()
  select(-c(code_num, OU5Code)) 


#JOIN TO NDOH to bring in period, DSD/TA and then eventually mechs
ndoh_join <- ndoh_all %>% 
  left_join(df_fac, by = c("Code" = "ou5code"))

#join NDOH to MFL
ndoh_join2 <- df_fac %>% 
  left_join(ndoh_all,  by = c("ou5code" = "Code")) %>% str()

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
         DSD_TA = str_split(`Datim UID`, ", ") %>% unlist() %>% nth(2)) %>% 
  select(-c(Total)) 


split <- str_split("TX_RTT (N, DSD, ARTNoContactReasonIIT/HIVStatus)", ", ") %>% unlist() %>% nth(2)

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


df_map_distinct <- df_map_clean %>% distinct(`Test Resuts/Outcome/Duration`, Sex, CoarseAgeGroup, Result, indicator, numeratordenom,
                                             dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, `Support Type`)

ndoh_mapped <- ndoh_clean2 %>% 
  left_join(df_map_distinct, by = c("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                                 "Sex", "CoarseAgeGroup", "Result", "indicator", "numeratordenom", "DSD_TA" = "Support Type")) %>% 
  distinct() 
  
  # count(`Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result,
  #       indicator, numeratordenom, `Datim UID`) %>%

#Figure out what is not getting mapped
not_mapped <- ndoh_mapped %>% 
  filter(is.na(dataElement)) %>% 
  count(`Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result, indicator, numeratordenom, DSD_TA, dataElement)

write_csv(not_mapped, "Dataout/Q3_datimuid_unmapped.csv")

not_mapped %>% 
  group_by(indicator) %>% 
  mutate(standardizeddisaggregate = "Age/Sex/NewExistingArt/HIVStatus): TB/HIV on ART") %>% 
  ungroup() %>% 
  mutate(datim_uid = glue("{indicator} ({numeratordenom}, {DSD_TA},{standardizeddisaggregate}{CoarseAgeGroup}\\
                          , {Sex}")) %>% 
  select(datim_uid)

"TB_ART (N, DSD, Age/Sex/NewExistingArt/HIVStatus): TB/HIV on ART<1, Female, Life-long ART, Already, Positive"

# TO-DO -------------------------------------------------------------------------

# BREAK INTO PARTNER IMPORT FILES - MFL only has ANOVA?

str(ndoh_mapped)

validation_file <- ndoh_mapped %>% 
  select(period, partner, mechanism_uid, Province, District,SubDistrict, Facility, datim_uid, indicator,
         `Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result,
         dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Total) %>% 
  rename(value = Total,
         mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid)

validation_file_bmw <- ndoh_mapped %>% 
  select(period, partner, mechanism_uid, Province, District,SubDistrict, Facility, datim_uid, indicator,
         dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Total) %>% 
  rename(value = Total,
         mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid)


import_file_clean <- ndoh_mapped %>% 
  select(mechanism_uid, datim_uid, dataElement_uid, categoryOptionCombo_uid, Total) %>% 
  rename(value = Total,
         mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid)

write_csv(import_file_clean, "Dataout/test-q2-import-file-readyforqc.csv")
write_csv(validation_file_bmw, "Dataout/test-q2-import-file-readyforqc-bmw.csv")


ndoh_clean2 %>% 
  count(mechanism_i_d)

mfl_df %>% 
  count(Partner, `Mechanism I.D`)


# Add KP facilities but add as normal - just to make sure we get all the facilities

#ARVDISP - use Given's file

#re-do mapping with new file

#TX TB?


#VALIDATIONS -------------------------------------------------------------------


