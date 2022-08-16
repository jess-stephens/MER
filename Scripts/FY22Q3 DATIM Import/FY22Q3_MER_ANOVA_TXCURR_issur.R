# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
# REF ID:   fb7ef9a9 
# LICENSE:  MIT
# DATE:     2022-08-04
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
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
  
  ref_id <- "fb7ef9a9"

# IMPORT ------------------------------------------------------------------

  #site adjusted dataset
  dsd_folderpath <- "Data/DSD TA"
  ndoh_folderpath <- "Data/NDOH Import"
  fy22files_folderpath <- "Data/Files to be added to FY22Q2 TX_RTT import"
  template_folderpath <- "Data/Import Files"
  genie_folderpath <- "Data/Genie"
  data_folder <- "Data/"
  partner_files <- "Dataout/Partner Import Files"
  
  ANOVA_FY22Q3_import <- partner_files %>% 
    return_latest("FY22Q3_ANOVA_import_FINAL") %>% 
    read_csv()
  
  #Read in updated MFL
  # mfl_df <- read_sheet(mfl_gid, sheet = "USAID_MFL")
  # mfl_new_df <- read_sheet(mfl_new_id)
  mfl_new_df <- read_sheet(mfl_new_id)
  
  
  #Is this only anova?

  
  df_q3 <- ndoh_folderpath %>% 
    return_latest("MER Reporting FY22Q3 v1_21072022")
  
  ndoh_filepath <- ndoh_folderpath %>%
    return_latest("MER Reporting FY22Q3 v1_21072022.xlsx")  
  
  
  # #MER mapping file
  # mapping_df <- data_folder %>% 
  #   return_latest("MER Mapping Draft 2 62322.xlsx") %>% 
  #   read_xlsx()
  # 
  # #MER mapping file
  # mapping_df <- data_folder %>% 
  #   return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>% 
  #   read_xlsx()
  
  #MER mapping file
  mapping_df <- data_folder %>% 
    return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>% 
    read_xlsx()
  

  
# filter facilities out of existing import file  -------------------------------------------------------------------

 ANOVA_filtered <-  ANOVA_FY22Q3_import %>% 
    # count(orgUnitName) %>% 
    filter(!(orgUnitName %in% c("lp Mamanyoha Clinic",  "lp Maphalle Clinic",
                                "lp Seapole Clinic",
                                "lp Sekgopo Clinic",
                                "lp Senobela Clinic",
                                "lp Shotong Clinic") &
             dataElement %in% c("TX_CURR (N, DSD, Age/Sex/HIVStatus): Receiving ART",
                                  "TX_PVLS (D, DSD, Age/Sex/Indication/HIVStatus): Viral Load Documented",
                                  "TX_PVLS (N, DSD, Age/Sex/Indication/HIVStatus): 12 Months Viral Load < 1000"))) %>% 
    mutate(mech_code = as.character(mech_code))
  

  
  #Use select indicators right now as we sort out SC indicators
  #Use select indicators right now as we sort out SC indicators
  indic_test <- c("PrEP_New","PrEP_CT","TX_NEW",
                  # "HTS_TST", "PMTCT_EID",
                  # "PMTCT_HEI_POS","PMTCT_HEI_POS_ART", "PMTCT_ART",
                  "TX_CURR", "TX_RTT", "TX_ML", "TB_ART", "TX_PVLS_Denom", "TX_PVLS_Numer",
                  "TB_STAT_Denom", "TB_STAT_Numer"
                  # ,"TX TB_D", "TX TB_N", "TB PREV_N", "TB PREV_D"
  )
  
  ndoh_anova <- indic_test %>% 
    #readxl::excel_sheets() %>%
    #setdiff("meta") %>%
    #  stringr::str_subset("CIRG") %>%
    purrr::map_dfr(.f = ~ read_all_the_things(ndoh_filepath, sheet = .x))
  
  #MUNGE NDOH AND MERGE WITH NFL- do we keep HIVTestOfferedIn?
  ndoh_anova <- ndoh_anova %>% 
    select(Province, District, SubDistrict, Facility, Code, 
           `Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result, Total, indicator) %>% 
    filter(Facility %in% c("lp Mamanyoha Clinic",  "lp Maphalle Clinic",
                              "lp Seapole Clinic",
                              "lp Sekgopo Clinic",
                              "lp Senobela Clinic",
                              "lp Shotong Clinic"),
           indicator %in% c("TX_CURR", "TX_PVLS_Denom", "TX_PVLS_Numer")) 

  
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
    select(ou5name, ou5uid, datim_uid, old_ou5code, starts_with("fy22")) %>% 
    # select(OU5name, OU5uid, OU5Code, starts_with("fy22")) %>% 
    pivot_longer(cols = starts_with("fy22"), names_to = "period", values_to = "DSD_TA") %>%
    #view()
    mutate(period = str_sub(period, start = 1, end = 6) %>% toupper(),
           old_ou5code = as.character(old_ou5code),
           DSD_TA = ifelse(DSD_TA == "DSD+Roving TA", "DSD", DSD_TA)) %>% 
    rename(usaid_facility = ou5name)
  
  write_csv(df_fac, "Dataout/FY22Q3-reshaped-facility-list.csv")
  
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
  ndoh_anova <- ndoh_anova %>%
    mutate(District = recode(District,
                             "fs Thabo Mofutsanyana District Municipality" = "fs Thabo Mofutsanyane District Municipality")) %>% 
    filter(District %in% usaid_district) %>% 
    mutate(code_num = str_length(Code)) %>%
    group_by(Province, District, SubDistrict, Facility) %>% 
    arrange(desc(code_num)) %>% 
    left_join(mfl_new_df %>% select(OU5name, Old_OU5Code), by = c("Facility" = "OU5name")) %>% 
    mutate(Old_OU5Code = as.character(Old_OU5Code),
           Code = ifelse(code_num < 6, Old_OU5Code, Code)) %>%
    fill(Code) %>% 
    ungroup() %>% 
    #count(Facility, Code) %>% view()
    select(-c(code_num, Old_OU5Code)) 
  
  
  #join NDOH to MFL
  ndoh_join2_anova <- df_fac %>% 
    left_join(ndoh_anova,  by = c("old_ou5code" = "Code")) 
  
  #CHECK******
  #what facilities are in NDOH but not in MFL? All correctional facilities
  ndoh_code <- unique(ndoh_anova$Code)
  mfl_code <- unique(df_fac$old_ou5code)
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
  
  ndoh_clean2_anova <- ndoh_join2_anova %>%
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
  ndoh_over50_anova <- ndoh_clean2_anova %>% 
    filter(CoarseAgeGroup == "50+") %>% 
    group_by(usaid_facility, ou5uid, datim_uid, old_ou5code, period, DSD_TA,
             Province, District, SubDistrict, Facility, `Test Result/Outcome/Duration`,
             Sex, CoarseAgeGroup, Result, indicator, numeratordenom) %>% 
    summarise(across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")
  
  ndoh_clean2_anova <- ndoh_clean2_anova %>% 
    filter(CoarseAgeGroup != "50+") %>% 
    bind_rows(ndoh_over50_anova)
  
  
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
  
  
  ndoh_mapped_anova <- ndoh_clean2_anova %>% 
    filter(indicator %ni% c("PrEP_CT", "TX_RTT")) %>% 
    left_join(df_map_distinct, by = c("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                                      "Sex", "CoarseAgeGroup", "Result", "indicator", "numeratordenom", "DSD_TA" = "Support Type"))
  
  # now pull mech codes from MSD ------------------
  
  library(Wavelength)
  library(curl)
  
  mechs <- pull_mech2() #run from mech script
  
  mech_xwalk <- mechs %>% 
    filter(operatingunit == "South Africa",
           mech_code %in% c(70310, 70287, 81902, 70290, 70301))
  
  match_mechs <- data_folder %>% 
    return_latest("match_fy22q3_facility-mech") %>% 
    read_xlsx()
  
  msd_mechs <- df_msd %>% 
    filter(funding_agency == "USAID",
           fiscal_year == 2022,
           mech_code %in% c(70310, 70287, 81902, 70290, 70301)) %>%
    count(sitename, facilityuid, mech_code, prime_partner_name)
  
  msd_mechs <- msd_mechs %>% 
    rbind(match_mechs)
  
  
  #join mech metadata
  ndoh_mapped_anova <- ndoh_mapped_anova %>% 
    left_join(msd_mechs, by = c("datim_uid" = "facilityuid")) %>% 
    left_join(mech_xwalk, by = c('mech_code'))
  

  
  
  # DISAGG COMBOS - troubleshoot
  #combinations msising
  #DSD_TA missing
  #facilities with DSD and Roving TA
  
  missing_disagg2 <-  ndoh_mapped_anova %>% 
    filter(is.na(dataElement),
           !is.na(District)) %>%
    select(indicator, DSD_TA, District, Facility, Sex, CoarseAgeGroup, `Test Result/Outcome/Duration`,
           Result, numeratordenom, dataElement)  
  
# IMPORT FILE 
  
  import_file_clean_anova <- ndoh_mapped_anova %>% 
    select(mech_code, mech_uid, Facility, datim_uid, dataElement, dataElement_uid,categoryOptionComboName, categoryOptionCombo_uid,
           Total, SubDistrict, District) %>% 
    rename(value = Total,
           #mech_uid = mechanism_uid,
           orgUnit_uid = datim_uid,
           orgUnitName = Facility)
  
  ANOVA_NEW_FY22Q3_import <- bind_rows(import_file_clean_anova, ANOVA_filtered)
  
  
  write_csv(ANOVA_NEW_FY22Q3_import, "Dataout/Partner Import Files/FY22Q3_ANOVA_import_adjusted20220804.csv")
  
  
  ndoh_mapped_anova %>% 
    filter(indicator == "TX_CURR") %>% 
    group_by(usaid_facility, numeratordenom) %>% 
    summarise(across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")
  
  
