library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)

memory.limit(size=500000)


current_pd<-"FY22q2i"


import_files<-here("Data/Import")


ANOVA_file<-list.files(import_files,pattern="70310") 
Anova<-read_excel(here("Data/Import",ANOVA_file),
                    sheet="import")

BR_file<-list.files(import_files,pattern="70287") 
BR<-read_excel(here("Data/Import",BR_file),
                  sheet="import") %>% 
  select(-`Sub-District`,-District,-Column1)


MATCH_file<-list.files(import_files,pattern="81902") 
MATCH<-read_excel(here("Data/Import",MATCH_file),
               sheet="import")


RTC_file<-list.files(import_files,pattern = "70290")
RTC<-read_excel(here("Data/Import",RTC_file),
                sheet="import")


WRHI_file<-list.files(import_files,pattern = "70301")
WRHI<-read_excel(here("Data/Import",WRHI_file),
                sheet="import")



WRHI_KP_file<-list.files(import_files,pattern = "80007")
WRHI_KP<-read_excel(here("Data/Import",WRHI_KP_file),
                 sheet="import")


final<-bind_rows(Anova, BR, MATCH, RTC, WRHI, WRHI_KP)
  
# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"USAID_IMPORT",current_pd,".csv",sep="_")

write_csv(final, file.path(here("Dataout"),filename),na="")