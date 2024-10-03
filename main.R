library(readr)
library(tidycensus)
library(jsonlite)
library(httr)
library(stringr)
library(lubridate)
library(janitor)
source("return_tibble.R")
source("check_vars.R")
source("factor_levels.R")
source("get_var_metadata.R")
source("extract_times.R")

library(tidyverse)
source("get_pums.R")


xnum_vars <- "PWGTP,GASP"
# xnum_vars <- NULL
# xchr_vars <- "HISPEED"
xchr_vars <- "SEX,JWAP,JWDP,JWMNP"
geography <- "state"
# user_vars <- "SEX,FER,HHL,PWGTP,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
# user_vars <- "SEX,PWGTP,MAR,HISPEED"
# user_vars <- "SEX"
key <- "bdb1f6ff2e4982a1da64cd526532aa92dca5581c"
state <- "05"  # Arkansas
year <- 2022

pums_data <- get_PUMS(geography,xnum_vars,xchr_vars,key,year,state)
print(pums_data)
print(unique(pums_data$api_data$JWAP))
