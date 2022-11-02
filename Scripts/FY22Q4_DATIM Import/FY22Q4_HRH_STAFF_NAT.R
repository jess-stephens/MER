# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  HRH_STAFF_NAT FY22Q4 processing script
# REF ID:   eccce60c
# LICENSE:  MIT
# DATE:     2022-10-31
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
  library(tierdrop)

# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "eccce60c"

  #DSP mechs
  dsp_mech <- c("70310", "70287", "81902",
                "70301", "70290")

  # SI specific paths/functions
    load_secrets()
    folderpath <- "C:/Users/jstephens/Documents/South Africa/DMR/MER/MER/Data/q4_prep"

  #HRH_STAFF_NAT data on google drive
  g_id <- "1JnyR4W2pC_JMPWGvYWNcZHYgHL1Gh8PQZxZfdeVWfGc"

# IMPORT ----------------------------------------------------------------------

  #misaligned facility names
  df_misaligned <-  folderpath %>%
    return_latest("alignment") %>%
    readxl::read_excel()

  #import data
  df <- read_sheet(g_id, skip =2)

  #grab category names for aggregation
  df_cat <- read_sheet(g_id, skip = 1, range = "2:3") %>%
    slice(1)

#pivot
 df_cat <- df_cat %>%
    select(-c(1:3)) %>%
    pivot_longer(cols = everything(), names_to = "cat", values_to = "indicator")

 #clean up cat names
  df_cat$cat <- stringr::str_match(df_cat$cat, "^(.*?)\\.+(\\d+)$")[,2]

# MFL -------------------------------------------------------------------------

  #Load MFL as is
  q4_id <- "1G2u0Rw0EgsExfPDjKIXzzB31OLzAVhkdPb5yz1lOauU"

  mfl_new_df <- googlesheets4::read_sheet(q4_id, sheet = "MFL_FY22Q4")

  #tidy MFL
  df_fac <- mfl_new_df %>%
    dplyr::filter(!is.na(OU2name)) %>%
    janitor::clean_names() %>%
    dplyr::select(ou5name, ou5uid, datim_uid, new_ou5_code, tidyselect::starts_with("fy22")) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("fy22"), names_to = "period", values_to = "DSD_TA") %>%
    dplyr::mutate(period = stringr::str_sub(period, start = 1, end = 6) %>% toupper(),
                  new_ou5_code = as.character(new_ou5_code),
                  DSD_TA = ifelse(DSD_TA == "DSD+Roving TA", "DSD", DSD_TA)) %>%
    dplyr::rename(usaid_facility = ou5name,
                  # mech_code = mechanism_i_d,
                  # mech_uid = mechanism_uid,
                  # mech_name = partner
    ) %>%
    filter(!is.na(new_ou5_code),
           !(usaid_facility == "kz Mvoti Clinic" & is.na(DSD_TA))) %>%
    mutate(usaid_facility = recode(usaid_facility, "lp Matsotsosela Clinic" = "lp Matsotsosela clinic"))

# MECHS ----------------------------------------------------------------------

  mechs <-
    folderpath %>%
    glamr::return_latest("mechs") %>%
    # readxl::read_excel()
    # glamr::si_path() %>%
    # glamr::return_latest("mechs") %>%
    readr::read_csv() %>%
    dplyr::mutate(mech_code = as.character(mech_code)) %>%
    filter(operatingunit == "South Africa",
           mech_code %in% dsp_mech)


# PROCESSING ------------------------------------------------------------------

# PIVOT indicators long
df_clean <- df %>%
    pivot_longer(cols = `Medical Officer - Position Count`:`Other - HCW - FTE`,
                 names_to = "indicator",
                 values_to = "value") %>%
    left_join(df_cat, by = "indicator") %>%
    mutate(OrgUnit = str_replace(OrgUnit, "clinic", "Clinic")) %>% 
    filter(!str_detect(indicator, 'FTE'))

df_clean <- df_clean %>%
  left_join(df_misaligned %>% filter(!is.na(`MFL_contenders (FY22Q4)`)),by = c("OrgUnit" = "HRH_Facility_Name")) %>%
  mutate(OrgUnit = ifelse(!is.na(`MFL_contenders (FY22Q4)`), `MFL_contenders (FY22Q4)`, OrgUnit)) %>%
  select(-c(`MFL_contenders (FY22Q4)`, `Notes`))

# CHECK -----

# orgunit_uid - dal with misaligned facility names
hrh_facility <- unique(df_clean$OrgUnit)
mfl_facility <- unique(df_fac$usaid_facility)

misaligned_facilities <- setdiff(hrh_facility, mfl_facility)

#Join mech code, mech uid, datim uid, and DSD/TA ---------

df_mapped <- df_clean %>%
  mutate(PrimePartner = recode(PrimePartner, "Anova Health Institute" = "ANOVA HEALTH INSTITUTE",
                               "Broadreach" = "BROADREACH HEALTHCARE (PTY) LTD",
                               "Maternal, Adolscent and Child Health (MatCH)" = "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC",
                               "Right To Care, South Africa" = "RIGHT TO CARE",
                               "Wits Reproductive Health& HIV Institute"= "WITS HEALTH CONSORTIUM (PTY) LTD")) %>%
  left_join(mechs %>% select(-c(mech_name)), by = c("PrimePartner" = "primepartner")) %>%
  left_join(df_fac,  by = c("OrgUnit" = "usaid_facility"))



# dataElementuid ----------------------------------------------------

#Read in mapping file from google drive - map in datim element and catoptioncombo
# df_map_distinct <- googlesheets4::read_sheet(disagg_map_id, sheet = "HRH_STAFF_NAT")
disagg_map_id <- "1IJpv6rA2JTz7dt2HdoRSy_Ulo1hzow_xMBgBKcN3Fzs"
df_map_distinct <- googlesheets4::read_sheet(disagg_map_id, sheet = "HRH_STAFF_NAT")


df_final <- df_mapped %>%
  mutate(cat = recode(cat, "Other Health Care Workers (HCW)" = "Other",
                      "Lab" = "Laboratory")) %>%
  group_by(period, operatingunit, OrgUnit, datim_uid, PrimePartner, mech_code, mech_uid,cat) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(df_map_distinct, by = c("cat" = "categoryoptioncomboname")) %>%
  select(period, OrgUnit, datim_uid, dataelementname, dataelement_uid, cat, categoryoptioncombo_uid,
         PrimePartner, mech_code, mech_uid, value) %>%
  filter(OrgUnit != "South Africa")


# EXPORT -------------------------------------------------------------------

write_csv(df_final, "Data/q4_prep/FY22Q4_HRH_STAFF_NAT_import_20221102.csv")



# QC with MSD -------------------------------------------------------------------
SA_MSD<-gophr:: read_msd("C:/Users/jstephens/Documents/South Africa/DMR/MER/MER/Data/q4_prep/MER_Structured_Datasets_Site_IM_FY20-23_20220916_v2_2_South Africa.txt")%>% 
  filter(indicator == "HRH_STAFF_NAT") %>% 
  
SA_MSD_munge<-SA_MSD %>% 
  filter(fiscal_year == "2021",standardizeddisaggregate=="CadreCategory" ) %>% 
    select(sitename, qtr4, otherdisaggregate)

df_msd_qc <- df_final %>%
  left_join(SA_MSD_munge, by = c("OrgUnit" = "sitename", "cat"="otherdisaggregate")) %>% 
  rename(FY21Q4=qtr4) %>% 
  mutate( dif=FY21Q4-value)

write_csv(df_msd_qc, "Data/q4_prep/FY22Q4_HRH_STAFF_NAT_import_20221102_MSDcomparison.csv")
