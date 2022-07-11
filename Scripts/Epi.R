library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(tameDP)
library(janitor)
library(Wavelength)


#credentials -------------------------------------------------------------------
myuser<-"gsarfaty_SA"


# Get DATIM Hierarchy from DATIM API via Wavelengh package ---------------------
hierarchy_snu<-pull_hierarchy("cDGPF739ZZr", myuser, askpass::askpass()) %>%
  filter(level==4) %>% 
  distinct(operatingunit,countryname,
           snu1,orgunituid) %>% 
  rename(country=countryname,
         snu1uid=orgunituid) %>% 
  mutate(operatingunituid="cDGPF739ZZr")


hierarchy_ou <- hierarchy_snu %>% 
  distinct(operatingunit,operatingunituid,country)



# Get Private Sector Adjusted PLHIV from COP22 Data pack & format using tameDP--
epi_path<-here("Data","Master_Data Pack_South Africa_PSNUxIM_v04.12 12h31 LATEST for Submission.xlsx")

epi_dp<-tame_dp(epi_path, type = "PLHIV") %>% 
  filter(indicator=="PLHIV") %>% 
  select(-cumulative) %>% 
  rename(period=fiscal_year,
         value=targets) %>% 
  mutate(source_name="Naomi Adjusted for COP22",
         period="FY22",
         period_type="cumulative",
         indicator="PLHIV_PSAdjusted",
         snuprioritization=case_when(
           str_detect(snuprioritization, "2") ~ "2 - Scale-Up: Aggressive",
           str_detect(snuprioritization, "1") ~ "1 - Scale-Up: Saturation",
           TRUE ~ snuprioritization
         ),
         short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM"))


#Thembisa Nat & Prov------------------------------------------------------------
thembisa<-read_excel(here("Data", "Thembisa 4.5 Tidy.xlsx"),
                     sheet="Thembisa_NationalProvincial")


thembisa_province<-thembisa %>% 
  clean_names() %>% 
  filter(!province=="South Africa") %>% 
  mutate(snu1=case_when(
           str_detect(province,"Eastern Cape") ~ "ec Eastern Cape Province",
           str_detect(province,"Free State") ~ "fs Free State Province",
           str_detect(province,"Gauteng") ~ "gp Gauteng Province",
           str_detect(province,"Natal") ~ "kz KwaZulu-Natal Province",
           str_detect(province,"Limp") ~ "lp Limpopo Province",
           str_detect(province,"Mpuma") ~ "mp Mpumalanga Province",
           str_detect(province,"Northern Cape") ~ "nc Northern Cape Province",
           str_detect(province,"North West") ~ "nw North West Province",
           str_detect(province,"Western Cape") ~ "wc Western Cape Province"
         )) %>% 
  left_join(hierarchy_snu,by="snu1") %>% 
  select(-province)


thembisa_ou<-thembisa %>% 
  clean_names() %>%
  filter(province=="South Africa") %>% 
  rename(country=province) %>%
  left_join(hierarchy_ou,by="country")


thembisa_ntlprov<-bind_rows(thembisa_ou,thembisa_province) %>% 
  mutate(standardizeddisaggregate=case_when(
    age_thembisa=="No age disagg" & sex == "No sex disagg"~ "Total",
    age_thembisa=="No age disagg" & !sex =="No sex disagg" ~ "Sex",
    !age_thembisa=="No age disagg" & sex=="No sex disagg" ~ "Age",
    !age_thembisa=="No age disagg" & !sex=="No sex disagg" ~ "Age/Sex"),
    source_name="Thembisa 4.5",
    period=year,
    period=substring(period,3),
    period=paste0("FY",period),
    period_type="cumulative") %>% 
  rename(ageasentered=age_thembisa)
 

rm(thembisa,thembisa_province,thembisa_ou)


#Thembisa Age Specific------------------------------------------------------------
thembisa_age<-read_excel(here("Data", "Thembisa 4.5 Tidy.xlsx"),
                     sheet="Thembisa_AgeSpecific")


thembisa_age_province<-thembisa_age %>% 
  clean_names() %>% 
  filter(!province=="South Africa") %>% 
  mutate(snu1=case_when(
    str_detect(province,"Eastern Cape") ~ "ec Eastern Cape Province",
    str_detect(province,"Free State") ~ "fs Free State Province",
    str_detect(province,"Gauteng") ~ "gp Gauteng Province",
    str_detect(province,"Natal") ~ "kz KwaZulu-Natal Province",
    str_detect(province,"Limp") ~ "lp Limpopo Province",
    str_detect(province,"Mpuma") ~ "mp Mpumalanga Province",
    str_detect(province,"Northern Cape") ~ "nc Northern Cape Province",
    str_detect(province,"North West") ~ "nw North West Province",
    str_detect(province,"Western Cape") ~ "wc Western Cape Province"
  )) %>% 
  left_join(hierarchy_snu,by="snu1") %>% 
  select(-province)


thembisa_age_ou<-thembisa_age%>% 
  clean_names() %>%
  filter(province=="South Africa") %>% 
  rename(country=province) %>%
  left_join(hierarchy_ou,by="country")


thembisa_age_combined<-bind_rows(thembisa_age_ou,thembisa_age_province) %>% 
  mutate(source_name="Thembisa 4.5",
         period=year,
         period=substring(period,3),
         period=paste0("FY",period),
         period_type="cumulative") %>% 
  select(age,age_to_50,age_to_65,everything()) %>% 
  gather(standardizeddisaggregate,ageasentered,age:age_to_65) %>% 
  mutate(standardizeddisaggregate=case_when(
    standardizeddisaggregate=="age" ~ "Age/SingleYear/Sex",
    standardizeddisaggregate=="age_to_50" ~ "Age/Sex",
    standardizeddisaggregate=="age_to_65" ~ "Age/Sex/HIVStatus",
  ))
  

rm(thembisa_age,thembisa_age_province,thembisa_age_ou)


final_thembisa<-bind_rows(thembisa_ntlprov,thembisa_age_combined) %>% 
  select(operatingunit,operatingunituid,country,snu1,snu1uid,
         indicator_category,indicator,standardizeddisaggregate,
         sex,ageasentered,year,period,period_type,
         value_type,value,source_name)


# COMBINE PROCESSED NAOMI & THEMBISA -------------------------------------------
final<-bind_rows(epi_dp,final_thembisa)


# DATAOUT ----------------------------------------------------------------------

# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"Naomi_Thembisa","processed.txt",sep="_")

write_tsv(final, file.path(here("Data"),filename,na=""))