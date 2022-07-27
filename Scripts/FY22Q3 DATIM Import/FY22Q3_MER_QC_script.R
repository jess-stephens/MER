# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Address QC tracking issues
# REF ID:   d0deeca2 
# LICENSE:  MIT
# DATE:     2022-07-21
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
  
  ref_id <- "d0deeca2"
  qc_tracker_id <- "18Fx3-TvRmEmlwHNmEGpNJcktkC6NK2xQ6kL-pr4HIds"

# IMPORT ------------------------------------------------------------------
  
  #Import QC tracker
  col_names <- array(read_sheet(qc_tracker_id, sheet = "QC Tracker", n_max = 1, col_names =  FALSE))
  df_qc <- read_sheet(qc_tracker_id, sheet = "QC Tracker", skip = 2, col_names = FALSE)
  
  colnames(df_qc) <- col_names
  
  df_error <- df_qc %>% 
    janitor::clean_names() %>%
    rename(check_tool = what_did_you_use_to_qc) %>% 
    select(1:9) %>%
    filter(data_errors == "Yes")
  
  #check missing disaggs
  
 missing_disagg <-  ndoh_mapped %>% 
    filter(is.na(dataElement),
           !is.na(District)) %>%
    select(indicator, DSD_TA, District, Facility, Sex, CoarseAgeGroup, `Test Result/Outcome/Duration`,
           Result, primepartner, numeratordenom, dataElement)
 
 
 
 
 missing_disagg <- missing_disagg %>% 
   select(indicator, Sex, CoarseAgeGroup, Result, `Test Result/Outcome/Duration`, numeratordenom, DSD_TA)
 
  
write_csv(missing_disagg, "Dataout/missing-disagg-combos.csv")
 
# check errors -------------------------------------------------------------------
  
#MER mapping file
mapping_df <- data_folder %>% 
  return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>% 
  read_xlsx()

#comprehensive combo names
comp_list <- data_folder %>% 
  return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>% 
  read_xlsx(sheet = "Sheet2", range = cell_cols("I:M"))


# need to replicate columns we join on:
  #indicator,Test results, Sex, Age, Result, DSD/TA, N/D

comp_list %>% 
  mutate(indicator = str_extract(dataelementname, "[^ (]+"),
         numeratordenom = str_extract(dataelementname, "[^,]+"),
         numeratordenom = str_sub(numeratordenom, start= -1)) %>% 
  filter(indicator %in% indic_test | indicator %in% c("TX_PVLS", "TB_STAT", "PrEP_NEW"),
         !str_detect(dataelementname, "KeyPop")) %>%
  group_by(dataelementname) %>% 
  mutate(DSD_TA = str_split(dataelementname, ", ") %>% unlist() %>% nth(2)) %>% 
  ungroup() %>%
  mutate(CoarseAgeGroup = ifelse(indicator %in% c("PrEP_CT"),
                                 str_extract(categoryoptioncomboname, "[^ ,]+"), NA),
         Sex = ifelse(indicator %in% c("PrEP_CT"),
                      str_extract(categoryoptioncomboname, "\\b\\w+$"), NA)) %>% 
  view()

str_split('PrEP_CT (N, DSD, Age/Sex): Receiving PrEP', ", ") %>% unlist() %>% nth(2)



