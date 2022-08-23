library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(janitor)



# REFERENCE
current_q<-"FY22Q3"





# HRID ---- MUST BE XLSX, NOT XLSB
HRID_file<-list.files(here("Data"),pattern="Factview")

hrid<-read_excel(here("Data",HRID_file))




# MUNGE ----------------------------------------------------------------
hrid<-hrid %>%
  rename(mech_code=MechanismID,
         mech_name=ImplementingMechanismName) %>% 
  mutate(mech_code=as.character(mech_code)) %>% 
  gather(period,value,FY2017Q2:FY2022Q3) %>% 
  clean_names() %>% 
  group_by_if(is.character) %>% 
  summarize(value = sum(value, na.rm = T))  %>% 
  ungroup() %>% 
  mutate(period = stringr::str_remove(period, "20")) %>% 
  # rename_with(str_to_lower) %>% 
  # select(-dsp,-dspid,-dsp_current,-siyenza,-prioritization,-highburden) %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  rename(occ_classification=disaggregate,
         cadre=category_option_combo_name,
         service_delivery_model=other_disaggregate,
         snuprioritization=psnu_priority) %>% 
  mutate(CHW_like = case_when(occ_classification=="Community Development Workers"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Community Development Workers"& service_delivery_model=="Facility site - Roving" ~ "Yes",
                              occ_classification=="Community Development Workers"& service_delivery_model=="Facility site - Seconded" ~ "Yes",
                              occ_classification=="Community Development Workers"& service_delivery_model=="Facility site - Surge" ~ "Yes",
                              occ_classification=="Community Development Workers"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="Community health workers"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Community health workers"& service_delivery_model=="Facility site - Roving" ~ "Yes",
                              occ_classification=="Community health workers"& service_delivery_model=="Facility site - Seconded" ~ "Yes",
                              occ_classification=="Community health workers"& service_delivery_model=="Facility site - Surge" ~ "Yes",
                              occ_classification=="Community health workers"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="Community navigator"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Community navigator"& service_delivery_model=="Facility site - Roving" ~ "Yes",
                              occ_classification=="Community navigator"& service_delivery_model=="Facility site - Seconded" ~ "Yes",
                              occ_classification=="Community navigator"& service_delivery_model=="Facility site - Surge" ~ "Yes",
                              occ_classification=="Community navigator"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="Mentor Mothers"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Mentor Mothers"& service_delivery_model=="Facility site - Roving" ~ "Yes",
                              occ_classification=="Mentor Mothers"& service_delivery_model=="Facility site - Seconded" ~ "Yes",
                              occ_classification=="Mentor Mothers"& service_delivery_model=="Facility site - Surge" ~ "Yes",
                              occ_classification=="Mentor Mothers"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="Outreach Team Leader - OTL"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Outreach Team Leader - OTL"& service_delivery_model=="Facility site - Roving" ~ "Yes",
                              occ_classification=="Outreach Team Leader - OTL"& service_delivery_model=="Facility site - Seconded" ~ "Yes",
                              occ_classification=="Outreach Team Leader - OTL"& service_delivery_model=="Facility site - Surge" ~ "Yes",
                              occ_classification=="Outreach Team Leader - OTL"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="Lay Counselors"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Lay Counselors"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="Linkage Officer"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Linkage Officer"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="Personal Care Workers"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Personal Care Workers"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="Social Work and related professionals"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Social Work and related professionals"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="Youth workers"& service_delivery_model=="Community-based" ~ "Yes",
                              occ_classification=="Youth workers"& service_delivery_model=="Ward Based Outreach Team - WBOT" ~ "Yes",
                              occ_classification=="WBOT OTL" ~ "Yes",
                              occ_classification=="WBOT CHW" ~ "Yes",
                              TRUE ~ "No"))

  
  
# CONTEXT FILES IN -------------------------------------------------------------
dsp_lookback<-read_excel(here("Data","DSP_attributes_2022-05-17.xlsx")) %>% 
  rename(agency_lookback=`Agency lookback`) %>% 
  select(-MechanismID)



# BIND FILES ----------------------------------------------------------------

final<-hrid %>% 
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

filename<-paste(Sys.Date(), current_q, "HRID_CTC_attributes.txt",sep="_")

write_tsv(final, file.path(here("Dataout"),filename),na="")