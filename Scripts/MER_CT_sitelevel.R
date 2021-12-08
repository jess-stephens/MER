library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(janitor)
library(lubridate)
library(Wavelength)

memory.limit(size=500000)


current_pd<-"FY21Q4i"
myuser<-"gsarfaty_SA"


# READ IN FILES ----------------------------------------------------------------
ind_ref<-pull(read_excel(here("Data", "indicator_ref.xlsx"),
              sheet="TX"))


#genie 
genie_files<-list.files(here("Data"),pattern="Genie")

genie<-here("Data",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>% 
  filter(fiscal_year %in% c("2021"))



# MSD
msd_files<-list.files(here("Data"),pattern="Site")

msd<-here("Data",msd_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind)


final<-msd %>%
  filter(indicator %in% ind_ref,
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"))

  
#subset & merge ----------------------------------------------------------------
msd<-msd %>%
  filter(fiscal_year %in% c("2018","2019","2020"))

final<-rbind(genie,msd) %>% 
  filter(indicator %in% ind_ref)

rm(genie,msd)


# CONTEXT FILES IN -------------------------------------------------------------
dsp_lookback<-read_tsv(here("Data","DSP_attributes_2021-02-08.txt")) %>% 
  rename(agency_lookback=`Agency lookback`) %>% 
  select(-MechanismID)


att<-read_excel(here("Data","vwOrgunitStructureOU6.xlsx"),
                sheet="DATIM Facilities") %>% 
  clean_names() %>% 
  distinct(datim_facility_uid,org_unit_ownership,org_unit_rural_urban,org_unit_type) %>% 
  rename(facilityuid=datim_facility_uid)  


siyenza_usaid<-read_csv(here("Data","Siyenza start date and end dates_12_6_21.v2.csv"))
  
  
siyenza_cdc<-read_excel(here("Data","CDC_Siyenza_Start_End_Dates as of 2021-12-02.xlsx"))


sitelist<-pull_hierarchy("cDGPF739ZZr", myuser, mypwd(myuser)) %>%
  mutate(facility=str_sub(orgunit,4)) %>%
  select(orgunit,orgunituid) %>% 
  rename(facility=orgunit)


sitelist_long<-sitelist %>% 
  rename(facility_datim=facility)


# SIYENZA FORMAT ---------------------------------------------------------------

siyenza_usaid<-siyenza_usaid %>% 
  clean_names() %>% 
  select(mechanism_id,facility,siyenza_start_date,siyenza_end_date) %>% 
  mutate(siyenza_start_date=mdy(siyenza_start_date),
         siyenza_end_date=mdy(siyenza_end_date)) %>% 
  left_join(sitelist,by="facility") %>% 
  mutate(orgunituid=case_when(
    facility=="kz Turton CHC" ~ "ESLvoqIBQlH",
    facility=="mp Lillian Mambakazi CHC" ~ "GOyBEVUAuy0",
    facility=="mp Piet Retief Clinic" ~ "WVgSBCk9tRa",
    facility=="wc Du Noon CDC" ~ "vacsM70GnZT",
    facility=="wc Dr Ivan Toms Clinic" ~ "IJfITpeTTKZ",
    TRUE ~ orgunituid
  )) %>% 
  left_join(sitelist_long, by="orgunituid") %>% 
  unite(siyenza_id,mechanism_id,facility_datim,sep="_",remove=FALSE) %>% 
  distinct(orgunituid,facility_datim,siyenza_id,siyenza_start_date,siyenza_end_date) %>% 
  rename(facility=facility_datim) 
  
  
siyenza_cdc<- siyenza_cdc %>% 
  clean_names() %>% 
  select(mech_id,facility,siyenza_start_date,siyenza_end_date,facility_id_datim) %>% 
  unite(siyenza_id,mech_id,facility,sep="_",remove=FALSE) %>% 
  select(-mech_id) %>% 
  rename(orgunituid=facility_id_datim)


previous_USAID<-siyenza_usaid %>% 
  semi_join(siyenza_cdc,by="orgunituid") %>% 
  select(orgunituid) %>% 
  mutate(agency_transition_siyenza="YES")
  

siyenza_list<-bind_rows(siyenza_cdc,siyenza_usaid) %>% 
  left_join(previous_USAID,by="orgunituid") %>% 
  mutate(siyenza=case_when(
    siyenza_end_date >="9999-01-01"~ "current siyenza site",
    siyenza_end_date < "9999-01-01" ~ "former siyenza site",
    TRUE ~ "never a siyenza site"))


#output siyenza list
filename<-paste(Sys.Date(),"Siyenza_Site_List",current_pd,".txt",sep="_")

write_tsv(siyenza_list, file.path(here("Dataout"),filename,na=""))



# CONTEXT MERGE ----------------------------------------------------------------
final<-final %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  unite(DSPID,mech_code,short_name,sep="",remove=FALSE) %>% 
  unite(siyenza_id,mech_code,facility,sep="_",remove=FALSE) %>% 
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
  # group_by_if(is.character) %>% 
  # summarize_at(vars(results:targets),sum,na.rm=TRUE)%>% 
  # ungroup() %>% 
  # mutate(indicator2=indicator,
  #        value2=value) %>% 
  # spread(indicator2,value2) %>% 
  left_join(dsp_lookback,by="DSPID") %>% 
  left_join(siyenza_list,by="siyenza_id") %>% 
  left_join(att, by="facilityuid") %>% 
  mutate(partner_other=case_when(
    mech_code=="70301" ~ "WRHI-USAID",
    mech_code=="70306" ~ "WRHI FSW/TG",
    mech_code=="18483" ~ "WRHI-CDC",
    mech_code=="80007" ~ "Wits Prevention",
    mech_code=="17207" ~ "WRHI KP",
    mech_code=="17038" ~ "WRHI TB/HIV",
    mech_code=="17028" ~ "WRHI Prioity Populations",
    TRUE ~ primepartner))
  
  
  
# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"MER_CTX_Facility",current_pd,"attributes.txt",sep="_")

write_tsv(final, file.path(here("Dataout"),filename,na=""))