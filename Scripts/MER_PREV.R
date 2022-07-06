library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)

memory.limit(size=500000)

current_pd<-"FY22Q2c"


# READ IN FILES ----------------------------------------------------------------
ind_ref<-pull(read_excel(here("Data", "indicator_ref.xlsx"),
              sheet="Prev"))


#genie 
genie_files<-list.files(here("Data"),pattern="PSNU_IM")

# genie<-here("Data",genie_files) %>% 
#   map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
#   # reduce(rbind) %>% 
#   # filter(fiscal_year %in% c("2021"))

final<-read_msd(here("Data",genie_files))

# MSD
# msd_files<-list.files(here("Data"),pattern="MER")
# 
# msd<-here("Data",msd_files) %>% 
#   map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
#   reduce(rbind)
# 
# 
# #subset & merge ----------------------------------------------------------------
# msd<-msd %>%
#   filter(fiscal_year %in% c("2018","2019","2020"))
# 
# final<-rbind(genie,msd) %>% 
#   filter(indicator %in% ind_ref)
# 
# rm(genie,msd)


# CONTEXT MERGE ----------------------------------------------------------------
final<-final %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM"),
         partner_dreams= case_when(mech_code %in% c("70301") & indicator %in% c("PrEP_NEW", "PrEP_CURR") ~ "Wits DSP",
                            mech_code %in% c("70287") & indicator %in% c("PrEP_NEW", "PrEP_CURR") ~ "Broadreach",
                            mech_code %in% c("70290") & indicator %in% c("PrEP_NEW", "PrEP_CURR") ~ "Right to Care",
                            mech_code %in% c("70310") & indicator %in% c("PrEP_NEW", "PrEP_CURR") ~ "ANOVA",
                            mech_code %in% c("80007") & indicator %in% c("PrEP_NEW", "PrEP_CURR") ~ "Wits Prevention",
                            mech_code %in% c("81891") & indicator %in% c("PrEP_NEW", "PrEP_CURR") ~ "Shout it Now",
                            mech_code %in% c("81902") & indicator %in% c("PrEP_NEW", "PrEP_CURR") ~ "MatCH",
                            mech_code %in% c("83013") & indicator %in% c("PrEP_NEW", "PrEP_CURR") ~ "TB/HIV Care",
                            mech_code %in% c("80002") & indicator %in% c("OVC_SERV", "OVC_HIVSTAT") ~ "NACOSA OVC",
                            mech_code %in% c("80008") & indicator %in% c("OVC_SERV", "OVC_HIVSTAT") ~ "NACOSA GBV",
                            mech_code %in% c("81904") & indicator %in% c("OVC_SERV", "OVC_HIVSTAT") ~ "Dept of Social Development",
                            TRUE ~ "NOT DREAMS PARTNER")) %>% 
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
  ),
  DREAMS_Districts=case_when(
    dreams=="Y" & psnu =="gp City of Johannesburg Metropolitan Municipality" ~ "original DREAMS district",
    dreams=="Y" & psnu =="gp Ekurhuleni Metropolitan Municipality" ~ "original DREAMS district",
    dreams=="Y" & psnu =="kz eThekwini Metropolitan Municipality" ~ "original DREAMS district",
    dreams=="Y" & psnu == "kz uMgungundlovu District Municipality" ~ "original DREAMS district",
    dreams == "Y" ~ "DREAMS expansion district",
    TRUE ~ "not a DREAMS district"))


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

filename<-paste(Sys.Date(),"MER_Prevention",current_pd,"attributes.txt",sep="_")

write_tsv(final, file.path(here("Dataout"),filename,na=""))