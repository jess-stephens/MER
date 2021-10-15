library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(janitor)

memory.limit(size=500000)



# Globals
ind_ref<-pull(read_excel(here("Data", "indicator_ref.xlsx"),
              sheet="Financial"))


current_pd<-"FY21Q3"


#genie - Read in and munge -----------------------------------------------------
genie_files<-list.files(here("Data"),pattern="Genie")

genie<-here("Data",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>% 
  filter(fiscal_year %in% c("2021"),
         fundingagency=="USAID",
         standardizeddisaggregate=="Total Numerator",
         indicator %in% ind_ref) %>% 
  group_by(mech_code,award_number,indicator,standardizeddisaggregate,fiscal_year) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  reshape_msd("long")
  

#outlays -read in and munge ----------------------------------------------------
outlay_files<-list.files(here("Data"),pattern="Outlay")

outlays<-read_excel(here("Data",outlay_files),
                sheet="ingestion") %>% 
  clean_names() %>% 
  rename(mech_code=mech_id) %>% 
  select(-mechanism_name,-prime_partner) %>% 
  rename(outlays=q3_qutlays) %>% 
  gather(indicator,value,cop20_budget,outlays) %>% 
  mutate(period=current_pd,
         mech_code=as.character(mech_code))

# bind --------------------------------------------------------------------
final<-bind_rows(outlays,genie) %>% 
  rename_official() %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2)
  
  
  
# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"MER_Outlays", current_pd, "attributes.txt",sep="_")

write_tsv(final, file.path(here("Dataout"),filename,na=""))