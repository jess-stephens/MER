# PROJECT:  SA-SI-MER
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TX_RTT Data Change Request - q2 data direct submission
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
library(readxl)

# GLOBAL VARIABLES --------------------------------------------------------

#site adjusted dataset
dsd_folderpath <- "Data/DSD TA"
ndoh_folderpath <- "Data/NDOH Import"
fy22files_folderpath <- "Data/Files to be added to FY22Q2 TX_RTT import"
template_folderpath <- "Data/Import Files"
genie_folderpath <- "Data/Genie"

genie_path <- genie_folderpath %>% 
  return_latest("Genie_SITE_IM_South_Africa_Frozen")

msd_source <- source_info()
curr_pd <- source_info(genie_path, return = "period")
curr_fy <- source_info(genie_path, return = "fiscal_year")
curr_qtr <- source_info(genie_path, return = "quarter")

# Set country of interest
cntry <- "South Africa"

# Get OU/Country orgunit uid
uid <- get_ouuid(cntry)

hierachy <-Wavelength::pull_hierarchy(uid, datim_user(), datim_pwd())

# IMPORT ------------------------------------------------------------------

#used to QC or Age/sex/hivstatus
df_genie <- genie_folderpath %>% 
  return_latest("Genie_SITE_IM_South_Africa_Frozen") %>% 
  read_msd()

#DSD/TA
facility_list <- dsd_folderpath %>% 
  return_latest("MASTER") %>% 
  read_csv()

#Q1 NDOH FILE
df_q2 <- ndoh_folderpath %>% 
  return_latest("MER Reporting FY22Q2_v1.2_03052022.xlsx") %>% 
  read_xlsx(sheet = "TX_RTT")


template_q1 <- template_folderpath %>% 
  return_latest("USAID ImportFile") %>% 
  read_csv()

template_q2 <- template_folderpath %>%
  return_latest("FY22Q2 RTT") %>%
  read_csv()

#has mech code and mech uid
template_q2i <- template_folderpath %>%
  return_latest("Copy") %>%
  read_csv()

# MUNGE ------------------------------------------------------------------

#get orgunituids
orgunits <- hierachy %>% 
  filter(level == 7) %>% 
  select(orgunit, orgunituid, countryname, snu1, psnu, psnuuid)

#Reshape facility list
df_fac <- facility_list %>% 
  filter(!is.na(OU2name)) %>% 
  select(c(1:19)) %>% 
  select(OU5name, OU5uid, starts_with("FY22")) %>% 
  pivot_longer(cols = starts_with("FY22"), names_to = "period", values_to = "DSD_TA") %>% 
  #view()
  mutate(period = str_sub(period, start = 1, end = 6)) %>% 
  rename(usaid_facility = OU5name)

#check if facilities have multiple uids
multiple_facq1 <- df_fac %>% 
  left_join(orgunits, by = c("usaid_facility" = "orgunit")) %>% 
  count(usaid_facility) %>%
  filter(n > 3) %>% 
  pull(usaid_facility)

#check for facilities with multiple facility_uids in Genie output
#check period (reporting results in Q1)
facility_check <- df_genie %>% 
  filter(fiscal_year == 2022,
           !is.na(qtr2),
         facility %in% multiple_facq1) %>% 
  count(facility, facilityuid) 

#join to import file and filter out the incorrect uids
wrong_facq1 <- left_join(facility_check, template_q2, by = c("facilityuid" = "orgUnit")) %>%
  filter(is.na(dataElement_uid)) %>% 
  distinct(facility, facilityuid) %>% 
  rename(orgunituid = facilityuid)
#pull(facilityuid)

#join facility list with orgunituid and filter out double counting with anti join
df_facility_join <- df_fac %>% 
  left_join(orgunits, by = c("usaid_facility" = "orgunit")) %>% 
  anti_join(wrong_facq1, by = c("orgunituid")) %>% 
  filter(usaid_facility %in% q2_genie_fac) #run from lines below




#get mech and mechuid
mech_join <- template_q2i %>% 
  count(mech_code, mech_uid) %>% 
  select(-n) %>% 
  filter(!is.na(mech_code)) %>% 
  mutate(mech_code = as.character(mech_code))


#Aggregate NDOH - group by facility and IIT Duration
df_q2_agg <- df_q2 %>% 
  group_by(Facility, IIT_Duration) %>% 
  summarise(across(c(starts_with("Total")), sum, na.rm = TRUE), .groups = "drop") %>% 
  rename(value = Total)

#add data element uid for DSD / TA
df_q2_join <- df_facility_join %>% 
  filter(period == "FY22Q2") %>% 
  left_join(df_q2_agg, by = c("usaid_facility" = "Facility")) %>% 
  mutate(dataElement_uid = ifelse(DSD_TA == "DSD", "SnpwfHHrqAu", "euQty0yYWEk"),
         categoryOptionCombo_uid = case_when(IIT_Duration == "IIT <3" ~ "SukunRmFdLh",
                                             IIT_Duration == "IIT 3-5" ~ "kyWqLuvAUFb",
                                             IIT_Duration == "IIT 6+" ~ "HZwrrx9g2Uz"),
         indicator = "TX_RTT") 

# QC -------------------------------------------------------------------------------

#QC: for each mehc code, sum of Age/HIV/Status for Q1 should= sum of No contact
# ARTNoContactReasonIIT/HIVStatus = IIT <3, 3-5, 6+
# In Q2, which mechs got it right?

#check Genie
q2_genie_num <- df_genie %>% 
  filter(fiscal_year == 2022,
         snu1 != "wc Western Cape Province",
         indicator == "TX_RTT",
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "ARTNoContactReason/HIVStatus"),
         mech_code %in% c(70310, 70287, 81902, 70290, 70301)) %>% 
  #  group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator) %>% 
  group_by(fiscal_year,indicator, standardizeddisaggregate, snu1,
           psnu, facility, facilityuid, mech_code
  ) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  filter(period == "FY22Q2") 

#Check NDOH
ndoh_q2_num <- df_q2_join %>%
  group_by(period,indicator, snu1, psnu, usaid_facility, orgunituid) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
  rename(val = value) 

#unique facility count
q2_genie_fac <- unique(q2_genie_num$facility)
q2_ndoh_fac <- unique(ndoh_q2_num$usaid_facility)

#missing Facilities between files
missing_facilities_q2 <- setdiff(q2_genie_fac, q2_ndoh_fac)

missing_fac_genie <- q1_genie_num %>% 
  filter(facility %in% missing_facilities) 

write_csv(missing_fac_genie, "Dataout/missing_facilities_fromNDOH.csv")

#check for misaligned values for facilities between files
test_q2 <- ndoh_q2_num %>% 
  left_join(q2_genie_num, by = c("period","indicator", "snu1", "psnu", "usaid_facility" = "facility",
                                 "orgunituid" = "facilityuid")) %>% 
  mutate(gap = ifelse(val == value, TRUE, FALSE)) %>% 
  filter(gap == FALSE)

write_csv(test_q2, "Dataout/misaligned_facilities_q2.csv")

# FINAL PROCESSING -------------------------------------------------------------------


#add mech codes
df_q2_join <- df_q2_join %>% 
  left_join(q2_genie_num %>% select(period, indicator, snu1, psnu,
                                    facility, facilityuid, mech_code), by = c("period","indicator", "snu1", "psnu", "usaid_facility" = "facility",
                                                                              "orgunituid" = "facilityuid")) %>% 
  #str()
  left_join(mech_join, by = c("mech_code")) 

#restructure to fit q2i temp
df_q2_final <- df_q2_join %>% 
  rename(orgUnitName = usaid_facility,
         orgUnit_uid = orgunituid,
         categoryOptionComboName = IIT_Duration) %>% 
  mutate(dataElementName = "TX_RTT (N, DSD, ARTNoContactReasonIIT/HIVStatus): Restarted ARV",
         categoryOptionComboName = case_when(categoryOptionComboName == "IIT <3" ~ "No Contact Outcome - Interruption in Treatment (<3 Months Interruption), Positive",
                                             categoryOptionComboName == "IIT 3-5" ~ "No Contact Outcome - Interruption in Treatment (3-5 Months Interruption), Positive",
                                             categoryOptionComboName == "IIT 6+" ~ "No Contact Outcome - Interruption In Treatment (6+ Months Interruption), Positive")) %>% 
  select(c(mech_code, mech_uid, orgUnitName,orgUnit_uid, dataElementName, 
           dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, value))

write_csv(df_q2_final, "Dataout/q2_processedNDOH.csv")

# BIND OTHER PARTNER FILES: SC_CURR, MATCH, TB_PREV FILES -----------------------------------

anova <- fy22files_folderpath %>% 
  return_latest("ANOVA Data Import Template - FY22Q2_Clean_Up") %>% 
  read_xlsx(sheet = 2) %>% 
  select(-c(1:4)) %>% 
  mutate(mech_code = as.character(mech_code))

match_sccurr <- fy22files_folderpath %>% 
  return_latest("MatCH Data Import Template - FY22Q2 060622") %>% 
  read_xlsx(sheet = 2) %>% 
  select(-c(1:4)) %>% 
  mutate(mech_code = as.character(mech_code))


anova_tb <- fy22files_folderpath %>% 
  return_latest("ANOVA Data Import Template_TB_PREV") %>% 
  read_xlsx(sheet = 2) %>% 
  select(-c(1:4)) %>% 
  mutate(mech_code = as.character(mech_code))


broadreach_tb <- fy22files_folderpath %>% 
  return_latest("BroadReach Data Import Template_FY22Q2_TB_PREV") %>% 
  read_xlsx(sheet = 2) %>% 
  select(-c(1:4)) %>% 
  mutate(mech_code = as.character(mech_code))


match_tb <- fy22files_folderpath %>% 
  return_latest("MatCH Data Import Template - FY22Q2 - TB_PREV") %>% 
  read_xlsx(sheet = 2) %>% 
  select(-c(1:4)) %>% 
  mutate(mech_code = as.character(mech_code))


rtc_tb <- fy22files_folderpath %>% 
  return_latest("RTC Data Import Template - FY22Q2_TB_PREV") %>% 
  read_xlsx(sheet = 2) %>% 
  select(-c(1:4)) %>% 
  mutate(mech_code = as.character(mech_code))


wrhi_tb <- fy22files_folderpath %>% 
  return_latest("WRHI Data Import Template - FY22Q2_TB_PREV") %>% 
  read_xlsx(sheet = 2) %>% 
  select(-c(1:4)) %>% 
  mutate(mech_code = as.character(mech_code))


final_q2_all <- bind_rows(df_q2_final, anova, match_sccurr, anova_tb, match_tb, broadreach_tb, rtc_tb, wrhi_tb)

write_csv(final_q2_all, "Dataout/fy22q2_direction_submission_processed.csv")



