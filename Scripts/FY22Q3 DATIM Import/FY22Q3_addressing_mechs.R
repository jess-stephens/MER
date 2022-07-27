# PROJECT:  SA-SI-MER
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Q3 - address Mech uid joins
# LICENSE:  MIT
# DATE:   2022-07-11
# UPDATED: 2022-07-11
# NOTE: run before the processing script

library(Wavelength)
library(curl)

mechs <- pull_mech2()

mech_xwalk <- mechs %>% 
  filter(operatingunit == "South Africa",
         mech_code %in% c(70310, 70287, 81902, 70290, 70301))


# PULL MECH -----------------------------------------------------------------
pull_mech2 <- function(usaid_only = TRUE, ou_sel = NULL, folderpath_output = NULL){
  
  package_check("curl")
  
  stopifnot(curl::has_internet())
  
  #url for SQL View stored as a csv
  sql_view_url <- "https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv"
  
  #pull from DATIM
  df <- readr::read_csv(sql_view_url,
                        col_types = readr::cols(.default = "c"))
  
  #fitler for OU
  if(!is.null(ou_sel))
    df <- dplyr::filter(df, ou %in% c(ou_sel))
  
  #filter for USAID mechs if specified
  if(usaid_only == TRUE)
    df <- dplyr::filter(df, agency == "USAID")
  
  #rename variables to match MSD
  df <- df %>%
    dplyr::select(operatingunit = ou,
                  fundingagency = agency,
                  mech_code = code,
                  mech_uid = uid,
                  primepartner = partner,
                  mech_name = mechanism)
  
  #remove mech_code from mech_name
  df <- df %>%
    dplyr::mutate(mech_name = stringr::str_remove(mech_name, "0000[0|1] |[:digit:]+ - "))
  
  #remove award information from mech_name
  df <- df %>%
    dplyr::mutate(mech_name = stringr::str_remove(mech_name, "^(720|AID|GHAG|U2GP).* - "))
  
  #export
  hfr_export(df, folderpath_output, type = "mechanisms")
  
  return(df)
}

