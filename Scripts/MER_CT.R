library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)

memory.limit(size=500000)


#GLOBALS -----------------------------------------------------------------------------
msds<-here("MSD")
genies<-here("Genie")
processed<-here("Processed_Files")


# READ IN FILES ----------------------------------------------------------------
ind_ref<-pull(read_excel(here("ContextFiles", "indicator_ref.xlsx"),
              sheet="TX"))


#genie 
genie_files<-list.files(genies,pattern="PSNU")

genie<-here("Genie",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>% 
  filter(fiscal_year %in% c("2021"))



# MSD
msd_files<-list.files(msds,pattern="PSNU")

msd<-here("MSD",msd_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind)


#subset & merge ----------------------------------------------------------------
msd<-msd %>%
  filter(fiscal_year %in% c("2018","2019","2020"))

final<-rbind(genie,msd) %>% 
  filter(indicator %in% ind_ref)

rm(genie,msd)

# CONTEXT FILES IN -------------------------------------------------------------
dsp_lookback<-read_tsv(here("ContextFiles","DSP_attributes_2021-02-08.txt")) %>% 
  rename(agency_lookback=`Agency lookback`) %>% 
  select(-MechanismID)



# CONTEXT MERGE ----------------------------------------------------------------
final<-final %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  unite(DSPID,mech_code,short_name,sep="",remove=FALSE) %>% 
  mutate(HIGHBURDEN=case_when(
    psnu=="gp City of Johannesburg Metropolitan Municipality" ~ "YES",
    psnu=="gp City of Tshwane Metropolitan Municipality" ~ "YES",
    psnu=="gp Ekurhuleni Metropolitan Municipality" ~ "YES",
    psnu=="kz eThekwini Metropolitan Municipality" ~ "YES",
    psnu=="wc City of Cape Town Metropolitan Municipality" ~ "YES",
    TRUE ~ "NO"
  ),
  focus_district=case_when(
    snuprioritization %in% c("2 - Scale-Up: Aggressive",
                             "1 - Scale-Up: Saturation") ~ "YES",
    TRUE ~ "NO"
  ))


# transform --------------------------------------------------------------------
final<-final %>% 
  reshape_msd("long") %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)%>% 
  ungroup() %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2)
  
  
  
# Dataout ----------------------------------------------------------------------

filename<-paste("MER", Sys.Date(), "attributes.txt",sep="_")

write_tsv(final, file.path(here("MER/Dataout"),filename,na=""))