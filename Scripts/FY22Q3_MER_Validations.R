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

#load NDOH_Clean from processing 
ndoh_clean

#MSD
df_msd <- si_path() %>% 
  return_latest("MER_Structured_Datasets_Site_IM_FY20-23_20220617_v2_1_South Africa") %>% 
  read_msd()

# Load DQRT as template
dqrt_id <- ("1NJb4p4fWGrqebVWsiRxWJCev2k2vzL1yvRgEWWHcGxY")

level2_dqrt <- read_sheet(dqrt_id, sheet = "ANOVA Level 2", skip = 5)

# LEVEL 1 ------------------------------------------------------------------


#1.1 - Result in previous quarter but no results in current quarter 
  # Compare to Genie file
  #outcome - facility this corresponds to
  #what is facility count for this?

validation_agg_result <- validation_file %>% 
  group_by(Province, District, SubDistrict,Facility, primepartner, orgUnit_uid, period, indicator) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop")

validation_agg_result %>% 
  left_join(genie_validation, by = c("orgUnit_uid" = "facilityuid", "indicator")) %>% 
  mutate(check = ifelse(!is.na(qtr1) & is.na(value), "Value reported in previous quarter but missing in this quarter", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "Result in previous quarter") %>% 
  select(Facility, orgUnit_uid, sitename, indicator, qtr1, value, qtr2, check) %>% view()

genie_validation %>% 
  left_join(validation_agg_result, by = c("facilityuid" = "orgUnit_uid", "indicator")) %>% 
  mutate(check = ifelse(!is.na(qtr1) & is.na(value), "Value reported in previous quarter but missing in this quarter", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "Result in previous quarter") %>% 
  select(Facility, facilityuid, sitename, indicator, qtr1, value, qtr2, check) %>% view()
  


#1.2 - Missing Data 
  #  number of facilities with missing data
  # need to get a facility count

#1.3 - Missing Facilities from NDOH that are in master facility list


#1.4 - Do numbers in NDOH and import files match?

ndoh_agg_validation <- ndoh_all %>% 
  group_by(Province, District, SubDistrict, Facility, indicator) %>% 
  summarise(across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")  
  
check_1_4 <- validation_agg_result %>% 
  left_join(ndoh_agg_validation, by = c("Province", "District", "SubDistrict", "Facility", "indicator")) %>% 
  mutate(check = ifelse(value != Total, "Value in import does not match NDOH", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "TriangulationWithNDOH") %>%
  filter(!is.na(check)) %>% 
  select(primepartner, level, today_date, type_check, period, check, SubDistrict, indicator, Facility)

#1.5 - Does genie fole = NDOH import file after submission?


genie_validation <- df_msd %>% 
  filter(funding_agency == "USAID",
         fiscal_year == 2022,
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(sitename, facilityuid, funding_agency, indicator, fiscal_year, standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") 

check_1_5 <- validation_file %>% 
  group_by(Province, District, SubDistrict,Facility, primepartner, orgUnit_uid, period, indicator) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop")  %>% 
  left_join(genie_validation, by = c("orgUnit_uid" = "facilityuid", "indicator")) %>% 
  mutate(check = ifelse(value != qtr2, "Value in import does not match Genie", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "TriangulationWithGeniePull") %>%
  filter(!is.na(check)) %>% 
  select(primepartner, level, today_date, type_check, period, check, SubDistrict, indicator, Facility)


level1_checks <- bind_rows(check_1_4, check_1_5)
write_csv(level1_checks, "Level1_checks_readyforqc_FY22Q2.csv")




# LEVEL 2 ---------------------------------------------------------------

#2.1 - PP_PREV number of testing servces > PP_PREV numerator
  #Not in NDOH

#2.2 - PrEP_CT > KP_PREV 

#2.3 - OVC_HIVSTAT > OVC_SERV_under 18; OVC exited without Graduation/ OVC_SERV >10%

#2.4 - PrEP_CT  Q3 < PrEP_CT Q2+ PREP_NEW Q3 (facility)



#2.5 - PrEP_NEW > PrEP_CT (facility)

check_2_5 <- validation_file %>% 
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
  select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)

#2.6 - PREP_CT<=Test result disagg (facility)

ndoh_clean %>% 
  filter(indicator %in% c("PrEP_CT")) %>% 
  count(Result)

#2.7 - PREP_CT<= KP Disagg (facility)

#2.8 - TB_PREV_D < TB_PREV_N (facility)

check_2_8 <- validation_file %>% 
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

#2.13 - HTS_TST<HTS_TST_POS; + DATIM support checks

check_2_13 <- validation_file %>%
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
  
  check_2_18 <- validation_file %>% 
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
  
  
  # 2.19 - PMTCT_STAT_POS < PMTCT_ART
  
  # 2.20 - TB_STAT_POS < TB_ART
  
  # 2.21 - TX_NEW > TX_CURR

  check_2_21 <- validation_file %>% 
    filter(indicator %in% c("TX_NEW", "TX_CURR")) %>% 
    group_by(Province, District, SubDistrict, Facility,indicator, mech_code, primepartner,period) %>% 
    summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = "indicator", values_from = "value") %>% 
  mutate(check = ifelse(`TX_NEW` > `TX_CURR`, "TX_NEW > TX_CURR", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
    select(mech_code, primepartner, level, today_date, type_check, period, check, SubDistrict, Facility)
  
  # 2.22 - TX_ML <3 months IIT> TX_ML +3months IIT
  
      #what about RIP and TFO?
  
 check_2_22 <-  validation_file %>% 
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
  
  check_2_24 <- validation_file %>% 
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
  
  # TX_RTT Key Pop > TX_RTT age bands 10-50+
    
  # TX_CURR>= TX_RTT
  
  # TX_CURR 2 quarters ago > TX_PVLS denominator
  
  # TX_CURR > SC_CURR
  
  # TX_PVLS numerator > TX_PVLS denominator
  
  # Compare previous and current quarter values, flag if zero or blank 
  
  # Flag if district has targets, but reported zero for the quarter
  
  
  level2_checks <- bind_rows(check_2_5,
                             #check_2_8,
                             #check_2_13,
                             #check_2_18,
                             check_2_21, 
                             check_2_22) %>%
                            # check_2_24) %>% 
    select(mech_code:Facility)
  
  write_csv(level2_checks, "Level2_checks_readyforqc_FY22Q2.csv")





