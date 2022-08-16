library(tidyverse)
library(gophr)
library(here)
library(readxl)
library(glamr)



# Function - get most recent file -----------------------------------
latest_file = function(fpattern, fpath) {
  f = list.files(pattern=fpattern, path=fpath, full.names=TRUE)
  f = file.info(f)
  rownames(f)[which.max(f$mtime)] 
}

# READ IN DATA -----------------------------------------------------------------

# read mer
mer<-here("Dataout")
mer_file<-latest_file("CTX",mer)

df_mer<-read_tsv(mer_file) %>% 
  select(-c(HTS_INDEX:TX_TB_D_POS)) %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW","TX_CURR","TX_NET_NEW"),
         standardizeddisaggregate=="Total Numerator",
         DSP=="Yes",
         agency_lookback=="USAID")
  


# read monthly
hfr_file<-list.files(here("Data"),pattern="nonmer")


df_hfr<-read_tsv(here("Data",hfr_file)) %>% 
  mutate(mech_code=as.character(mech_code),
         indicator=case_when(
           indicator=="TX_CURR_28" ~ "TX_CURR",
           TRUE ~ indicator)) %>% 
  select(-snu1) %>% 
  rename(prime_partner_name=primepartner,
         snu1=snu) %>%
  select(mech_code,mech_name,prime_partner_name,
         snu1,psnu,facility,facilityuid,period,
         table,mon_yr,value,period_type,
         standardizeddissaggreate)

# munge ------------------------------------------------------------------------

df_hfr_curr<-df_hfr %>% 
  filter(mon_yr >"2021-04",
         indicator %in% c("TX_CURR_28")) %>% 
  select(!`# Facilities for which patients were provided offsite ART services`:uLTFU) %>% 
  select(-distribution_channel,-distribution_methods) %>% 
  rename(standardizeddisaggregate=disaggregate) %>% 
  mutate(indicator=case_when(
    indicator=="TX_CURR_28" ~ "TX_CURR",
    TRUE ~ indicator),
    table="HFR",
    period="FY21Q3",
    period_type="results",
    mech_code=as.character(mech_code)) 

  
df_hfr_cumulative<-df_hfr %>%
  filter(mon_yr >"2021-03",
         indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW")) %>% 
  select(!`# Facilities for which patients were provided offsite ART services`:uLTFU) %>% 
  select(-distribution_channel,-distribution_methods) %>% 
  rename(standardizeddisaggregate=disaggregate) %>% 
  mutate(table="HFR",
    period="FY21Q3",
    period_type="results",
    mech_code=as.character(mech_code))



df_mer_2<-df %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW","TX_CURR","TX_NET_NEW"),
         standardizeddisaggregate=="Total Numerator",
         DSP=="Yes",
         agency_lookback=="USAID")


fy22_T<-df_fy22 %>% 
  filter(fiscal_year=="2022",
         fundingagency=="USAID",
         indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW","TX_CURR","TX_NET_NEW"),
         standardizeddisaggregate=="Total Numerator")



# merge ------------------------------------------------------------------------
df_mer_merge<-bind_rows(df_mer_2,fy22_T) %>% 
  reshape_msd(clean=TRUE)



final<-bind_rows(df_mer_merge,df_hfr_curr,df_hfr_cumulative)



write_tsv(final,here("Processed_files/MER_HFR","2021-05-31_HFRmonthly_MER.txt"),na="")