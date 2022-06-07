
# Title: South Africa Import Aggregation
# Author: Vanessa Da Costa
# Last updated: "May 6, 20212"
# Purpose:Combine Partner Q2 Imports

library(stringr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
library(readxl)
library(here)
library(purrr)
library(here)
library(data.table)
library(googlesheets4)
library(splitstackshape)
library(devtools)
require(tidyverse)
require(readxl)

here::here("South Africa")


df1<-read_excel("Q2 Import/ANOVA 70310_Data Import Template - FY22Q2.xlsx", sheet="import") 
df2<-read_excel("Q2 Import/BroadReach_70287_ Data Import file.xlsx", sheet="import") %>% 
  select(-c('Sub-District':'Column1')) 
  
df3<-read_excel("Q2 Import/MatCH_81902_Data Import Template - FY22Q2.xlsx", sheet="import") 
df4<-read_excel("Q2 Import/WRHI_ 70301_Data Import Template.xlsx", sheet="import") 
df5<-read_excel("Q2 Import/WRHI_80007 Data Import Template.xlsx", sheet="import") 



Final_Q2_USAID_SA<- bind_rows(df1, df2, df3, df4, df5) %>% 
  dplyr::mutate(`Period`="2022Q2") %>% 
  drop_na()

write.csv(Final_Q2_USAID_SA, paste0("Q2_USAID_SA_Import_", format(Sys.time(), "%d-%b-%Y"), ".csv"))

  