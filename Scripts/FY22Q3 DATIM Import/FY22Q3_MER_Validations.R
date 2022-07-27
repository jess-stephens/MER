# PROJECT:  SA-SI-MER
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Q3 MER Processing
# LICENSE:  MIT
# DATE:   2022-07-11
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


# IMPORT ------------------------------------------------------------------

#MSD
df_msd <- si_path() %>% 
  return_latest("MER_Structured_Datasets_Site_IM_FY20-23_20220617_v2_1_South Africa") %>% 
  read_msd()

# Load final import file for all partners
final_checks_Q3 <- read_csv("Dataout/Partner Import Files/FY22Q3_ALL_PARTNER_import.csv")

#facility list for MFL checks
df_fac <- read_csv("Dataout/FY22Q3-reshaped-facility-list.csv")

# LEVEL 1 ------------------------------------------------------------------


genie_validation <- df_msd %>% 
  filter(funding_agency == "USAID",
         fiscal_year == 2022,
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(sitename, facilityuid, funding_agency, indicator, fiscal_year, standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") 


#1.1 - Result in previous quarter but no results in current quarter 
  # Compare to Genie file
  #outcome - facility this corresponds to
  #what is facility count for this?


#aggregate results up to facility by indicator
validation_agg_result <- final_checks_Q3 %>% 
  group_by(Province, District, SubDistrict,Facility, primepartner, orgUnit_uid, period, indicator) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop")

check_1_1 <- genie_validation %>% 
  left_join(validation_agg_result, by = c("facilityuid" = "orgUnit_uid", "indicator")) %>%
  mutate(check = ifelse(!is.na(qtr2) & is.na(value), "Value reported in previous quarter but missing in this quarter", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "Result in previous quarter") %>% 
  filter(!is.na(check)) %>% 
  select(primepartner, level, today_date, type_check, period, check, SubDistrict, indicator, Facility) 
  
#1.2 - Missing Data - use check 1.1

#1.3 - Missing Facilities from NDOH that are in master facility list - this check is just for us, not partners
#CHECK******
#what facilities are in NDOH but not in MFL?
ndoh_code2 <- unique(final_checks_Q3$Facility)
mfl_code2 <- unique(df_fac$usaid_facility)
setdiff(ndoh_code2, mfl_code2)

#1.4 - Do numbers in NDOH and import files match? - just a check for us, not for partners

ndoh_agg_validation <- ndoh_all %>% 
  group_by(Province, District, SubDistrict, Facility, indicator) %>% 
  summarise(across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")  
  
check_1_4 <- validation_agg_result %>% 
  left_join(ndoh_agg_validation, by = c("Province", "District", "SubDistrict", "Facility", "indicator")) %>% 
  mutate(check = ifelse(value != Total, "Value in import does not match NDOH", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "TriangulationWithNDOH") %>%
  filter(!is.na(check)) %>% view()
  select(primepartner, level, today_date, type_check, period, check, SubDistrict, indicator, Facility)

#1.5 - Does genie file = NDOH import file after submission? (Only possible on FY22Q2)
  
# genie_validation_targets <- df_msd %>% 
#   filter(funding_agency == "USAID",
#          fiscal_year == 2022,
#          standardizeddisaggregate == "Total Numerator") %>% 
#   group_by(sitename, facilityuid, funding_agency, indicator, fiscal_year, standardizeddisaggregate) %>% 
#   summarise(across(starts_with("targets"), sum, na.rm = TRUE), .groups = "drop") 
# 
# check_1_5 <- validation_file %>% 
#   group_by(Province, District, SubDistrict,Facility, primepartner, orgUnit_uid, period, indicator) %>% 
#   summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop")  %>% 
#   left_join(genie_validation, by = c("orgUnit_uid" = "facilityuid", "indicator")) %>% 
#   mutate(check = ifelse(value != qtr2, "Value in import does not match Genie", NA),
#          level = "Level 1",
#          today_date = lubridate::today(),
#          type_check = "TriangulationWithGeniePull") %>%
#   filter(!is.na(check)) %>% 
#   select(primepartner, level, today_date, type_check, period, check, SubDistrict, indicator, Facility)

#bind all relevant checks
level1_checks <- bind_rows(check_1_1, check_1_4)

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

#WRHI
WRHI_level_1 <- level1_checks %>% 
  filter(primepartner == "WITS HEALTH CONSORTIUM (PTY) LTD")


write_csv(anova_level_1, "ANOVA_Level1_checks_FY22Q3.csv")
write_csv(BR_level_1, "BroadReach_Level1_checks_FY22Q3.csv")
write_csv(MATCH_level_1, "MATCH_Level1_checks_FY22Q3.csv")
write_csv(RTC_level_1, "RTC_Level1_checks_FY22Q3.csv")
write_csv(WRHI_level_1, "WRHI_Level1_checks_FY22Q3.csv")

# LEVEL 2 ---------------------------------------------------------------

#2.1 - PP_PREV number of testing servces > PP_PREV numerator
  #Not in NDOH

#2.2 - PrEP_CT > KP_PREV 

#2.3 - OVC_HIVSTAT > OVC_SERV_under 18; OVC exited without Graduation/ OVC_SERV >10%

#2.4 - PrEP_CT  Q3 < PrEP_CT Q2+ PREP_NEW Q3 (facility)

#2.5 - PrEP_NEW > PrEP_CT (facility)

check_2_5 <- final_checks_Q3 %>% 
  filter(indicator %in% c("PrEP_NEW", "PrEP_CT")) %>% 
  group_by(Province, District, SubDistrict, Facility, indicator, mech_code, primepartner,period) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
 # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>% 
  #utate(dataElement_uid = indicator) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
 # rename()
  mutate(check = ifelse(`PrEP_NEW` > `PrEP_CT`, "PrEP_NEW > PrEP_CT", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>% 
  filter(!is.na(check)) %>%
  select(mech_code, primepartner, level, today_date, type_check, period, check, District, SubDistrict, Facility)

#2.6 - PREP_CT<=Test result disagg (facility)

  # ndoh_clean %>% 
  #   filter(indicator %in% c("PrEP_CT")) %>% 
  #   count(Result)

#2.7 - PREP_CT<= KP Disagg (facility)

#2.8 - TB_PREV_D < TB_PREV_N (facility)

check_2_8 <- final_checks_Q3 %>% 
  filter(indicator %in% c("TB_PREV")) %>% 
  group_by(Province, District, SubDistrict, Facility, indicator, numeratordenom, mech_code, primepartner,period) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>% 
  #utate(dataElement_uid = indicator) %>% 
  pivot_wider(names_from = "numeratordenom", values_from = "value") %>%
  # rename()
  mutate(check = ifelse(`D` < `N`, "TB_PREV_D < TB_PREV_N", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>% 
  filter(!is.na(check)) %>% 
  select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)


#2.9 - Index_offered<Index_accepted (facility)

#2.10 - Index_contacts <contacts_ new_pos + contacts_ new_neg + contacts_ known_pos (facility)

#2.11 - HTS_TST_POS (>=15) >=HTS_RECENT

#2.12 - HTS_RECENT >= Key Populations Sub-total

#2.13 - HTS_TST<HTS_TST_POS; + DATIM support checks - can this even be run on TIER?

check_2_13 <- final_checks_Q3 %>%
  filter(indicator %in% c("HTS_TST")) %>%
  group_by(Province, District, SubDistrict, Facility, indicator, `Test Result/Outcome/Duration`, mech_code, primepartner,period) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>% 
  #utate(dataElement_uid = indicator) %>% 
  pivot_wider(names_from = "Test Result/Outcome/Duration", values_from = "value") %>% 
  # rename()
  mutate(total = `Positive (Reactive)` + `Negative (Non-reactive)`,
    check = ifelse(total < `Positive (Reactive)`, "HTS_TST < HTS_TST_POS", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)


#2.14 - Unknown /OVC_HIV_STAT not greater than 10% by APR less than 5%

#2.15 - PMTCT_EID 0-2months > PMTCT_STAT_POS+HTS_TST_POS_PMTCT PostANC1/Age/Sex/Result

# 2.16 - PMTCT_HEI_POS > PMTCT_EID
    # which disaggs?

validation_file %>% 
  filter(indicator %in% c("PMTCT_EID", "PMTCT_HEI_POS")) %>% 
group_by(Province, District, SubDistrict, Facility, dataElement, dataElement_uid, indicator, partner,period) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "dataElement", values_from = "value") %>% view()
  mutate(check = ifelse(`PMTCT_HEI_POS` > `PMTCT_EID`, "PMTCT_HEI_POS > PMTCT_EID", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>% view()
  filter(!is.na(check)) %>% 
    select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)
  
  # 2.17 - PMTCT_STAT numerator > PMTCT_STAT denominator

  # 2.18 - TB_STAT numerator >  TB_STAT denominator
  
  check_2_18 <- final_checks_Q3 %>% 
    filter(indicator %in% c("TB_STAT")) %>% 
    group_by(Province, District, SubDistrict, Facility, indicator, numeratordenom, mech_code, primepartner,period) %>% 
    summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
    # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>% 
    #utate(dataElement_uid = indicator) %>% 
    pivot_wider(names_from = "numeratordenom", values_from = "value") %>%
    # rename()
    mutate(check = ifelse(`D` < `N`, "TB_STAT_N >  TB_STAT_D", NA),
           level = "Level 2",
           today_date = lubridate::today(),
           type_check = "IntraIndicator") %>%
    filter(!is.na(check)) %>% 
    select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)
  
  
  # 2.19 - PMTCT_STAT_POS < PMTCT_ART (non-TIER and TIER)
  
  # 2.20 - TB_STAT_POS < TB_ART
  
  # 2.21 - TX_NEW > TX_CURR

  check_2_21 <- final_checks_Q3 %>% 
    filter(indicator %in% c("TX_NEW", "TX_CURR")) %>% 
    group_by(Province, District, SubDistrict, Facility,indicator, mech_code, primepartner,period) %>% 
    summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = "indicator", values_from = "value") %>% 
  mutate(check = ifelse(`TX_NEW` > `TX_CURR`, "TX_NEW > TX_CURR", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
    select(mech_code, primepartner, level, today_date, type_check, period, check, District, SubDistrict, Facility)
  
  # 2.22 - TX_ML <3 months IIT> TX_ML +3months IIT
  
      #what about RIP and TFO?
  
 check_2_22 <-  final_checks_Q3 %>% 
    filter(indicator == "TX_ML") %>%
    rename(IIT = `Test Result/Outcome/Duration`) %>% 
    mutate(IIT = recode(IIT, "IIT 3-5" = "IIT 3+",
                        "IIT 6+" = "IIT 3+")) %>% 
    #mutate(IIT = recode(ifelse(IIT %in% ("IIT 3-5","IIT 6+"), "IIT 3+", IIT)))
    group_by(Province, District, SubDistrict, Facility,indicator, IIT,  mech_code, primepartner,period) %>%
    summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = "IIT", values_from = "value") %>% 
    mutate(check = ifelse(`IIT <3` > `IIT 3+`, "TX ML IIT <3 > IIT 3+", NA),
           level = "Level 2",
           today_date = lubridate::today(),
           type_check = "IntraIndicator") %>% 
    filter(!is.na(check)) %>% 
   select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)
 
    count(`Test Result/Outcome/Duration`)
  
  
  # 2.23 - TX_NEW > HTS_TST_POS at community level; TX_NEW Key pop (age bands) > TX_NEW
    
  # 2.24 - TX_RTT > TX_CURR
  
  check_2_24 <- final_checks_Q3 %>% 
    filter(indicator %in% c("TX_RTT", "TX_CURR")) %>% 
    group_by(Province, District, SubDistrict, Facility,indicator, mech_code, primepartner,period) %>% 
    summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = "indicator", values_from = "value") %>% 
    mutate(check = ifelse(`TX_RTT` > `TX_CURR`, "TX_RTT > TX_CURR", NA),
           level = "Level 2",
           today_date = lubridate::today(),
           type_check = "IntraIndicator") %>%
    filter(!is.na(check)) %>% 
    select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)
  
  # 2.25 - TX_RTT Key Pop > TX_RTT age bands 10-50+
    
  # 2.26 - TX_CURR>= TX_RTT
  
  # 2.27 - TX_CURR 2 quarters ago > TX_PVLS denominator
  
  # tx_curr_lag <- genie_validation %>% 
  #   filter(indicator == "TX_CURR_Lag1") %>% 
  #   group_by(sitename, facilityuid, indicator) %>% 
  #   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  #   pivot_wider(names_from = "indicator", values_from = "qtr2") %>% 
  #   select(sitename, `TX_CURR_Lag1`)
  # 
  # 
  # final_checks_Q3 %>% 
  #   filter(indicator %in% c("TX_PVLS"),
  #          numeratordenom == "D") %>% 
  #   group_by(Province, District, SubDistrict, Facility,indicator, numeratordenom, mech_code, primepartner,period) %>% 
  #   summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
  #  # filter(indicator == "TX_PVLS")
  #   pivot_wider(names_from = "indicator", values_from = "value") %>% 
  #   left_join(tx_curr_lag, by = c("Facility" = "sitename")) %>% 
  #   mutate(check = ifelse(`TX_CURR_Lag1` > `TX_PVLS`, "TX_CURR_Lag2 > TX_PVLS_D", NA),
  #          level = "Level 2",
  #          today_date = lubridate::today(),
  #          type_check = "IntraIndicator") %>%
  #   filter(is.na(check)) %>% 
  #   select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)
  # 
  # 2.28 - TX_CURR > SC_CURR
  
  check_2_28 <- final_validation_KRS %>% 
    filter(indicator %in% c("TX_CURR", "SC_ARVDISP")) %>%
    group_by(Province, District, SubDistrict, Facility, indicator, mech_code, primepartner,period) %>% 
    summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
    # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>% 
    #utate(dataElement_uid = indicator) %>% 
    pivot_wider(names_from = "indicator", values_from = "value") %>% 
    # rename()
    mutate(check = ifelse(`TX_CURR` > `SC_ARVDISP`, "TX_CURR > SC_CURR", NA),
           level = "Level 2",
           today_date = lubridate::today(),
           type_check = "IntraIndicator") %>%
    filter(!is.na(check)) %>% 
    select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)
  
  # 2.29 - TX_PVLS numerator > TX_PVLS denominator
  
  check_2_29 <- validation_file %>% 
    filter(indicator %in% c("TX_PVLS")) %>% 
    group_by(Province, District, SubDistrict, Facility, indicator, numeratordenom, mech_code, primepartner,period) %>% 
    summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
    # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>% 
    #utate(dataElement_uid = indicator) %>% 
    pivot_wider(names_from = "numeratordenom", values_from = "value") %>%
    # rename()
    mutate(check = ifelse(`D` < `N`, "TB_STAT_N >  TB_STAT_D", NA),
           level = "Level 2",
           today_date = lubridate::today(),
           type_check = "IntraIndicator") %>%
    filter(!is.na(check)) %>% 
    select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)
  
  # Compare previous and current quarter values, flag if zero or blank 
  
  # Flag if district has targets, but reported zero for the quarter
  
  
  level2_checks <- bind_rows(
    #check_2_5,
                             #check_2_8,
                             #check_2_13,
                             #check_2_18,
                             check_2_21
                             # , 
                             # check_2_22
                             ) %>%
                            # check_2_24) %>% 
    select(mech_code:Facility)
  
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
  
  #WRHI
  WRHI_level_2 <- level2_checks %>% 
    filter(primepartner == "WITS HEALTH CONSORTIUM (PTY) LTD")
  
  
  write_csv(anova_level_2, "ANOVA_Level2_checks_FY22Q3.csv")
  write_csv(BR_level_2, "BroadReach_Level2_checks_FY22Q3.csv")
  write_csv(MATCH_level_2, "MATCH_Level2_checks_FY22Q3.csv")
  write_csv(RTC_level_2, "RTC_Level2_checks_FY22Q3.csv")
  write_csv(WRHI_level_2, "WRHI_Level2_checks_FY22Q3.csv")
  
  write_csv(level2_checks, "Level2_checks_readyforqc_FY22Q2.csv")


# MER MAPPING CHECKS - what disaggregates do we not expect to have? -----------------
  
  # Missing: PMTCT_ART_D standard disagg: Age/Sex/KnownNewResult and other disagg: Known at Entry
  # Missing: PMTCT_ART_D standard disagg: Age/Sex/KnownNewResult and other disagg: Newly Identified
  # Missing: PMTCT_HEI_POS_ART standard disagg: Age/HIVStatus/ARTStatus
  # Missing: PMTCT_STAT_N standard disagg: Age/Sex/KnownNewResult
  # Missing: PMTCT_STAT_POS standard disagg: Age/Sex/KnownNewResult 
  # Missing: TX_CURR standard disagg: Age/Sex/ARVDispense/HIVStatus 
  # Missing: TX_ML standard disagg: Age/Sex/ARTCauseofDeath 

  # PARTNER COUNT CHECK -------------------------------

