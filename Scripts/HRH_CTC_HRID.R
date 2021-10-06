library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)

memory.limit(size=500000)



# HRID ---- MUST BE XLSX, NOT XLSB
HRID_file<-list.files(here("Data"),pattern="HRID")

hrid<-read_excel(here("Data",HRID_file),
                 sheet="data")

# CTC
ctc_file<-list.files(here("Data"),pattern="CTC")

ctc<-read_excel(here("Data",ctc_file),
                sheet="RawData")


# MUNGE ----------------------------------------------------------------
hrid<-hrid %>%
  rename(mech_code=MechanismID,
         mech_name=ImplementingMechanismName) %>% 
  mutate(mech_code=as.character(mech_code)) %>% 
  gather(period,value,FY2017Q2:FY2021Q3) %>% 
  group_by_if(is.character) %>% 
  summarize(value = sum(value, na.rm = T))  %>% 
  ungroup() %>% 
  mutate(period = stringr::str_remove(period, "20")) %>% 
  rename_with(str_to_lower) %>% 
  select(-dsp,-dspid,-dsp_current,-siyenza,-prioritization,-highburden) %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) 

ctc<-ctc %>% 
  rename_with(str_to_lower) %>% 
  rename(fundingagency=agency,
         mech_code=`mechanism id`,
         mech_name=mechanismname,
         snu1=province,
         community=`sub-district`,
         cadre=`disaggregate cadre`,
         occ_classification=`occupational classification`,
         service_delivery_model=`service delivery model`,
         period=yearquarter,
         value_type=valuetype,
         last_refreshed=lastrefreshed,
         value=`sum of value`) %>% 
  mutate(period = stringr::str_remove(period, "20")) %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM"),
         indicator="CTC") 

  
  
# CONTEXT FILES IN -------------------------------------------------------------
dsp_lookback<-read_tsv(here("Data","DSP_attributes_2021-02-08.txt")) %>% 
  rename(agency_lookback=`Agency lookback`) %>% 
  select(-MechanismID)



# BIND FILES ----------------------------------------------------------------

final<-bind_rows(hrid,ctc) %>% 
  unite(DSPID,mech_code,short_name,sep="",remove=FALSE) %>%  
  mutate(highburden=case_when(
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
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)%>% 
  ungroup() %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2) %>% 
  left_join(dsp_lookback,by="DSPID") 
  


  
# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(), "FY21Q3_HRID_CTC_attributes.txt",sep="_")

write_tsv(final, file.path(here("Dataout"),filename,na=""))