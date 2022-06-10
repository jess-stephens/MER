# PROJECT:  SA-SI-MER
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY22Q2 - Direct Submission dedup issue
# LICENSE:  MIT
# DATE:     2022-06-10
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
data_folderpath <- "Data/"

q2 <- data_folderpath %>% 
  return_latest("FY22Q2USAID") %>% 
  read_csv()

duplicate_facilities <- data_folderpath %>% 
  return_latest("validation_results") %>% 
  read_xlsx(sheet = 2)

df_dup <- data_folderpath %>% 
  return_latest("validation_results") %>% 
  read_xlsx(sheet = 1)

#pull facilities with duplicates
fac_uid <- duplicate_facilities %>% 
  pull(orgUnit)

q2_dedup <- q2 %>% 
  filter(dataElement_uid == "ectUGvYjtaK"
         ,
         orgUnit_uid %in% fac_uid
         ) %>%
#  filter(orgUnit_uid == "AYL6GsAIXzA") %>% 
  # count(categoryOptionCombo_uid) %>% 
  # view() %>% 
  distinct() %>% 
  group_by(mech_uid, orgUnit_uid, dataElement_uid, categoryOptionCombo_uid) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") 

final_file_q2 <- q2 %>% 
  mutate(flag = ifelse(dataElement_uid == "ectUGvYjtaK"  & orgUnit_uid %in% fac_uid, TRUE, FALSE)) %>% 
  filter(flag == FALSE) %>% 
  bind_rows(q2_dedup) %>% 
  select(-c(flag))

write_csv(final_file_q2, "Dataout/FY22Q2USAID_DirectSubmission_Updated 61022.csv")
