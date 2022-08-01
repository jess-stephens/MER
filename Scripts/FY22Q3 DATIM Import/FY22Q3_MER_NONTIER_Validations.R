# PROJECT:  SA-SI-MER- NONTIER [IN PROGRESS]
# AUTHOR:   Vanessa Da Costa| USAID
# PURPOSE:  Q3 MER Processing- NONTIER Indicators
# LICENSE:  MIT
# DATE:   2022-07-27
# UPDATED: 2022-08-01
# NOTE: Adapted from SA-SI-MER Script by Karishma Srikanth 


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

# IMPORT ------------------------------------------------------------------

#MSD
#df_msd <- si_path() %>% 
 # return_latest("MER_Structured_Datasets_Site_IM_FY20-23_20220617_v2_1_South Africa") %>% 
  #read_msd()

df_msd<- read.delim("Q3 Datasets/MER_Structured_Datasets_Site_IM_FY20-23_20220617_v2_1_South Africa.txt")

#facility list for MFL checks adding district and sub-district
#df_fac <- read_csv("Dataout/FY22Q3-reshaped-facility-list.csv") #OU3 and OU4 name
df_fac<-read_excel("Q3 Datasets/USAID_MASTER_FACILITY_LIST_FY22Q3_draft v2.xlsx", sheet="MFL")

#Aggregate final NON-TIER import files for all DSP partners
ANOVA<-read_excel("Q3 Datasets/ANOVA _Non-Tier Import Template - FY22Q3.xlsx", sheet="import") %>% 
  mutate(primepartner = "ANOVA HEALTH INSTITUTE")
  
MATCH<-read_excel("Q3 Datasets/MatCH_Non Tier Data Import Template - FY22Q3.xlsx", sheet="import") %>% 
  mutate(primepartner = "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC")

BRCH<-read_excel("Q3 Datasets/BroadReach DATIM Q3FY22 Import_File NonTIER Indicators_28072022.xlsx", sheet="import") %>% 
  mutate(primepartner = "BROADREACH HEALTHCARE (PTY) LTD")

RTC<-read_excel("Q3 Datasets/RTC_NonTIER Data Import Template - FY22Q3.xlsx", sheet="import") %>%
  mutate(primepartner = "RIGHT TO CARE")

WRHI_70301<-read_excel("Q3 Datasets/WRHI_70301_Non Tier Data Import File - FY22Q3.xlsx", sheet="import")%>%
  mutate(primepartner = "WITS HEALTH CONSORTIUM (PTY) LTD")

WRHI_80007<-read_excel("Q3 Datasets/WITS_RHI_Q3_80007 Data Import.xlsx", sheet="import") %>%
  dplyr::mutate(primepartner = "WITS HEALTH CONSORTIUM (PTY) LTD")

#File to be used for NON-TIER checks      
FY22Q3_Master_NonTier_Validations<- bind_rows(ANOVA, MATCH, BRCH, RTC, WRHI_70301, WRHI_80007) %>% 
  dplyr::mutate(`period`="2022Q3") %>% 
  drop_na() %>% 
  #add a code that changes dataelement name to indicator
  mutate(indicator = str_extract(`dataElementName`, "[^ (]+"),
         numeratordenom = str_extract(`dataElementName`, "[^,]+"),
       numeratordenom = str_extract(`numeratordenom`, "(?<=())[^()]*$"),
         DSD_TA = str_split(`dataElementName`, ", ") %>% unlist() %>% nth(2),
        standardizeddisaggregate = str_extract(`dataElementName`, "(?<=,)[^,]*$"))
        #File to be combined with TIER indicators      
FY22Q3_Master_NonTier_Final<- bind_rows(ANOVA, MATCH, BRCH, RTC, WRHI_70301, WRHI_80007) %>% 
  mutate(`period`="2022Q3") %>% 
  select (`mech_code`: `period`) %>% 
  drop_na()
  
#Master NONTIER Import File
write.csv(FY22Q3_Master_NonTier_Final, paste0("Q3_USAID_SA_Import_NonTier_", format(Sys.time(), "%d-%b-%Y"), ".csv"))


# LEVEL 1 ------------------------------------------------------------------

genie_validation <- df_msd %>% 
  filter(funding_agency == "USAID",
         fiscal_year == 2022,
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(prime_partner_name,sitename, facilityuid, funding_agency, indicator, fiscal_year, standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  #select only non-tier indicators 
   filter(indicator %in% c("HTS_INDEX,","HTS_SELF", "HTS_TST", "PMTCT_ART", "PMTCT_EID", "PMTCT_HEI_POS", "PMTCT_HEI_POS_ART","PMTCT_STAT", "HTS_RECENT"))


#aggregate results up to facility by indicator
validation_agg_result <- FY22Q3_Master_NonTier_Validations%>%
  #exclude dataelements that are duplicates within the indicator
  filter(!dataElementName %in% c("HTS_SELF (N, TA, KeyPop/HIVSelfTest): HIV self test kits distributed",
           "PMTCT_HEI_POS (N, TA, Age/HIVStatus/ARTStatus): Infant Testing","PMTCT_HEI_POS (N, DSD, Age/HIVStatus/ARTStatus): Infant Testing")) %>% 
  group_by(primepartner,mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop")

#check for results in previous quarter and missing in current quarter
check_1_1 <- genie_validation %>% 
  left_join(validation_agg_result, by = c("facilityuid" = "orgUnit_uid", "indicator")) %>%
  mutate(check = ifelse(!is.na(qtr2) & is.na(value), "Value reported in previous quarter but missing in this quarter", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "Result in previous quarter") %>% 
  filter(!is.na(check)) %>% 
  select(prime_partner_name, level, today_date, type_check,check,indicator, sitename) 


#bind all relevant checks
level1_checks <- check_1_1

#ANOVA
anova_level_1 <- level1_checks %>% 
  filter(primepartner == "ANOVA HEALTH INSTITUTE")

#BROADREACH
BR_level_1 <- level1_checks %>% 
  filter(primepartner == "BROADREACH HEALTHCARE (PTY) LTD")

#MATCH
MATCH_level_1 <- level1_checks %>% 
  filter(primepartner == "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC")

#RIGHT TO CARE
RTC_level_1 <- level1_checks %>% 
  filter(primepartner == "RIGHT TO CARE")

#RIGHT TO CARE
WRHI_level_1 <- level1_checks %>% 
  mutate(primepartner = "WITS HEALTH CONSORTIUM (PTY) LTD")

write_excel_csv(level1_checks, paste0("MASTER_Level1_NONTIERchecks_FY22Q3_", format(Sys.time(), "%d-%b-%Y"), ".csv"))
write_csv(anova_level_1, "ANOVA_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(anova_level_1, "ANOVA_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(BR_level_1, "BroadReach_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(MATCH_level_1, "MATCH_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(RTC_level_1, "RTC_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(WRHI_level_1, "WRHI_Level1_NONTIERchecks_FY22Q3.csv")

# LEVEL 2 ---------------------------------------------------------------

#2.9 - Index_offered<Index_accepted (facility) [NON-TIER]

check_2_9 <- FY22Q3_Master_NonTier_Validations %>% 
  filter(indicator %in% c("HTS_INDEX")) %>% 
  filter(standardizeddisaggregate %in% c(" Index/Age/Sex/CasesOffered): Number of index cases offered"," Index/Age/Sex/CasesAccepted): Number of index cases accepted")) %>% 
  group_by(primepartner,mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, standardizeddisaggregate, period) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>%
  mutate(check = ifelse(` Index/Age/Sex/CasesOffered): Number of index cases offered` < ` Index/Age/Sex/CasesAccepted): Number of index cases accepted`, "Index_offered<Index_accepted", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>% 
  filter(!is.na(check)) %>% 
  select(primepartner,level,today_date,check, period, orgUnitName)

#2.10 - Index_contacts <contacts_ new_pos + contacts_ new_neg + contacts_ known_pos (facility)
check_2_10 <- FY22Q3_Master_NonTier_Validations %>% 
  filter(indicator %in% c("HTS_INDEX")) %>% 
  filter(standardizeddisaggregate %in% c(" Index/Age Aggregated/Sex/Contacts): Number of contacts", " Index/Age/Sex/Result): HTS Result")) %>%
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, standardizeddisaggregate, period, primepartner) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>%
  mutate(check = ifelse(` Index/Age Aggregated/Sex/Contacts): Number of contacts` < ` Index/Age/Sex/Result): HTS Result`, "Index_contacts <contacts_ new_pos + contacts_ new_neg + contacts_ known_pos", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>% 
  filter(!is.na(check)) %>% 
  select(primepartner,level,today_date,check, period, orgUnitName)
  #select(primepartner,level,today_date,check, period, orgUnitName,` Index/Age Aggregated/Sex/Contacts): Number of contacts`,` Index/Age/Sex/Result): HTS Result`)


#2.11 - HTS_TST_POS (>=15) >=HTS_RECENT #note, no partners reported HTS_RECENT as of 7.29
HTS_TST_POS_15plus <- FY22Q3_Master_NonTier_Validations %>% 
  filter((`indicator`== "HTS_TST" & (categoryOptionComboName== "15-19, Male, Positive" | categoryOptionComboName==  "15-19, Female, Positive"
                                     | categoryOptionComboName== "20-24, Male, Positive" | categoryOptionComboName==  "20-24, Female, Positive"
                                     | categoryOptionComboName== "25-29, Male, Positive" | categoryOptionComboName==  "25-29, Female, Positive"
                                     | categoryOptionComboName== "30-34, Male, Positive" | categoryOptionComboName==  "30-34, Female, Positive"
                                     | categoryOptionComboName== "35-39, Male, Positive" | categoryOptionComboName==  "35-39, Female, Positive"
                                     | categoryOptionComboName== "40-44, Male, Positive" | categoryOptionComboName==  "40-44, Female, Positive"
                                     | categoryOptionComboName== "45-49, Male, Positive" | categoryOptionComboName==  "45-49, Female, Positive"
                                     | categoryOptionComboName== "50+, Male, Positive" | categoryOptionComboName==  "50+, Female, Positive")))

HTS_RECENT <- FY22Q3_Master_NonTier_Validations %>% 
  filter(indicator %in% c("HTS_RECENT")) 
  
check_2_11<-bind_rows(HTS_TST_POS_15plus, HTS_RECENT) %>% 
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`HTS_TST` >= `HTS_RECENT`, "HTS_TST_POS (>=15) >=HTS_RECENT", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>% 
  filter(!is.na(check)) %>% 
  select(primepartner,level,today_date,check, period, orgUnitName)


#2.12 - HTS_RECENT >= Key Populations Sub-total

#2.13 - HTS_TST<HTS_TST_POS
check_2_13 <-  FY22Q3_Master_NonTier_Validations %>% 
  filter(indicator %in% c("HTS_TST")) %>% 
          group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner,categoryOptionComboName) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "categoryOptionComboName", values_from = "value") %>% 
  mutate(HTS_TST_POS = `Unknown Age, Female, Positive` +`Unknown Age, Male, Positive` + `<1, Female, Positive`+
           `<1, Male, Positive`+ `10-14, Female, Positive` +
           `10-14, Male, Positive`+ `1-4, Female, Positive`+ `1-4, Male, Positive`+ `15-19, Female, Positive`+
           `15-19, Male, Positive`+ `20-24, Female, Positive`+ `20-24, Male, Positive`+ `25-29, Female, Positive`+
           `25-29, Male, Positive`+ `30-34, Female, Positive`+ `30-34, Male, Positive`+ `35-39, Female, Positive`+
           `35-39, Male, Positive`+ `40-44, Female, Positive`+ `40-44, Male, Positive`+ `45-49, Female, Positive`+ 
           `45-49, Male, Positive`+ `50+, Female, Positive`+ `50+, Male, Positive`+`5-9, Female, Positive`+ `5-9, Male, Positive`,
            HTS_TST= HTS_TST_POS + `Unknown Age, Female, Negative` +`Unknown Age, Male, Negative` + `<1, Female, Negative`+
           `<1, Male, Negative`+ `10-14, Female, Negative` +
           `10-14, Male, Negative`+ `1-4, Female, Negative`+ `1-4, Male, Negative`+ `15-19, Female, Negative`+
           `15-19, Male, Negative`+ `20-24, Female, Negative`+ `20-24, Male, Negative`+ `25-29, Female, Negative`+
           `25-29, Male, Negative`+ `30-34, Female, Negative`+ `30-34, Male, Negative`+ `35-39, Female, Negative`+
           `35-39, Male, Negative`+ `40-44, Female, Negative`+ `40-44, Male, Negative`+ `45-49, Female, Negative`+ 
           `45-49, Male, Negative`+ `50+, Female, Negative`+ `50+, Male, Negative`+`5-9, Female, Negative`+ `5-9, Male, Negative`,
         check = ifelse(`HTS_TST` < `HTS_TST_POS`, "HTS_TST < HTS_TST_POS", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(primepartner,level,today_date,check, period, orgUnitName)


#2.14 - Unknown /OVC_HIV_STAT not greater than 10% by APR less than 5%
  ##TO ADD FOR FY22Q4

#2.15 - PMTCT_EID 0-2months > PMTCT_STAT_POS+HTS_TST_POS_PMTCT PostANC1/Age/Sex/Result
PMTCT_EID_0_2months <- FY22Q3_Master_NonTier_Validations %>% 
  filter((`indicator`== "PMTCT_EID" & categoryOptionComboName== "<= 2 months"))

PMTCT_STAT_POS <- FY22Q3_Master_NonTier_Validations %>% 
  filter(`indicator`== "PMTCT_STAT") %>%
  filter(numeratordenom %in% c("N")) %>% 
  filter(str_detect(categoryOptionComboName, "Positive"))

HTS_TST_POS_PMTCT <-FY22Q3_Master_NonTier_Validations %>% 
  filter(dataElementName %in% c("HTS_TST (N, DSD, PMTCT PostANC/Age/Sex/Result): HTS received results", "HTS_TST (N, TA, PMTCT PostANC/Age/Sex/Result): HTS received results")) %>% 
filter(str_detect(categoryOptionComboName, "Positive")) #INDICATOR IS HTS_TCT

check_2_15<-bind_rows(PMTCT_EID_0_2months, PMTCT_STAT_POS, HTS_TST_POS_PMTCT) %>% 
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(PMTCT_POS = PMTCT_STAT + HTS_TST,  
         check = ifelse(PMTCT_EID > PMTCT_POS, "PMTCT_EID 0-2months > PMTCT_STAT_POS+HTS_TST_POS_PMTCT", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>% 
  filter(!is.na(check)) %>% 
  select(primepartner,level,today_date,check, period, orgUnitName)

#2.16 - PMTCT_HEI_POS > PMTCT_EID
PMTCT_EID <- FY22Q3_Master_NonTier_Validations %>% 
  filter(`indicator`== "PMTCT_EID")

PMTCT_HEI_POS <- FY22Q3_Master_NonTier_Validations %>% 
  filter(`indicator`== "PMTCT_HEI_POS") %>% 
filter(categoryOptionComboName %in% c("<= 2 months, Positive","2 - 12 months , Positive")) 
  
check_2_16 <-  bind_rows(PMTCT_EID,PMTCT_HEI_POS ) %>% 
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% view() %>% 
mutate(check = ifelse(`PMTCT_HEI_POS` > `PMTCT_EID`, "PMTCT_HEI_POS > PMTCT_EID", NA),
       level = "Level 2",
       today_date = lubridate::today(),
       type_check = "IntraIndicator") %>%
filter(!is.na(check)) %>% 
  select(primepartner,level,today_date,check, period, orgUnitName)

# 2.17 - PMTCT_STAT numerator > PMTCT_STAT denominator
check_2_17 <-  FY22Q3_Master_NonTier_Validations %>% 
  filter(indicator %in% c("PMTCT_STAT")) %>% 
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, numeratordenom,primepartner) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "numeratordenom", values_from = "value") %>%
  mutate(check = ifelse(`N` > `D`, "PMTCT_STAT_N >  PMTCT_STAT_D", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(primepartner,level,today_date,check, period, orgUnitName)

# 2.19 - PMTCT_STAT_POS < PMTCT_ART
PMTCT_ART <- FY22Q3_Master_NonTier_Validations %>% 
  filter(`indicator`== "PMTCT_ART")

check_2_19 <-  bind_rows(PMTCT_STAT_POS, PMTCT_ART) %>% 
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`PMTCT_STAT` < `PMTCT_ART`, "PMTCT_STAT_POS < PMTCT_ART", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(primepartner,level,today_date,check, period, orgUnitName)


#without HTS_RECENT

#without HTS_RECENT
level2_checks <- bind_rows( check_2_9,  check_2_10, check_2_13, check_2_15, check_2_16, check_2_17, check_2_19)

#ALL CHECK
level2_checks <- bind_rows( check_2_9,  check_2_10, check_2_11, check_2_13, check_2_15, check_2_16, check_2_17, check_2_19)
#ANOVA
anova_level_2 <- level2_checks %>% 
  filter(primepartner == "ANOVA HEALTH INSTITUTE")

#BROADREACH
BR_level_2 <- level2_checks %>% 
  filter(primepartner == "BROADREACH HEALTHCARE (PTY) LTD")

#MATCH
MATCH_level_2 <- level2_checks %>% 
  filter(primepartner == "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC")

#RIGHT TO CARE
RTC_level_2 <- level2_checks %>% 
  filter(primepartner == "RIGHT TO CARE")


write_csv(anova_level_2, "ANOVA_Level2_NONTIERchecks_FY22Q3.csv")
write_csv(BR_level_2, "BroadReach_Level2_NONTIERchecks_FY22Q3.csv")
write_csv(MATCH_level_2, "MATCH_Level2_NONTIERchecks_FY22Q3.csv")
write_csv(RTC_level_2, "RTC_Level2_NONTIERchecks_FY22Q3.csv")

write_csv(level2_checks, "Level2_NONTIERchecks_readyforqc_FY22Q3.csv")

#Save all checks in master Excel workbook
Master_NONTIER_wb<- openxlsx::loadWorkbook("Dataout/MASTER MER NONTIER DQRT Tracker.xlsx") 

openxlsx::writeData(Master_NONTIER_wb, sheet = "Level1", x = level1_checks, 
                    colNames=T, withFilter=T)
openxlsx::writeData(Master_NONTIER_wb, sheet = "Level2", x = level2_checks, 
                    colNames=T, withFilter=T)
openxlsx::saveWorkbook(Master_NONTIER_wb, "Dataout/MASTER MER NONTIER DQRT Tracker.xlsx", overwrite = TRUE)


