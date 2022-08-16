library(extrafont)
library(tidyverse)
library(gophr)
library(janitor)
library(here)
library(scales)
library(patchwork)
library(formattable)
library(gt)
library(readxl)
library(glamr)
library(Wavelength)



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


df<-read_tsv(mer_file)



#PEB munge------------------------------------------------------

PEB_Viral_Load<-df %>% 
  filter(indicator %in% c("HTS_TST_POS","TX_NEW","TX_NET_NEW","TX_CURR","TX_PVLS"),
         standardizeddisaggregate %in% c("Total Numerator","Total Denominator"),
         # fiscal_year %in% c("2019","2020","2021"),
         funding_agency=="USAID",
         DSP=="Yes") %>%
  mutate(indicator = ifelse(numeratordenom == "D",
                            paste0(indicator, "_D"), indicator)) %>% 
  clean_psnu() %>% 
  # reshape_msd(clean = TRUE) %>% 
  select(operatingunit:value,DSP:partner_other) %>% 
  mutate(period=paste(period,period_type)) 

  



write_tsv(PEB_Viral_Load,here("Dataout","PEB_FY22Q2i_v1.0.txt"),na="")


