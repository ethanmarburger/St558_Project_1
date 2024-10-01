library(dplyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(httr)
source("return_tibble.R")
source("get_pums.R")
source("check_vars.R")
source("factor_levels.R")
source("get_var_metadata.R")


xnum_vars <- "GASP"
# xnum_vars <- NULL
# xchr_vars <- "HISPEED"
xchr_vars <- NULL
geography <- "state"
# user_vars <- "SEX,FER,HHL,PWGTP,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
# user_vars <- "SEX,PWGTP,MAR,HISPEED"
# user_vars <- "SEX"
key <- "bdb1f6ff2e4982a1da64cd526532aa92dca5581c"
state <- "05"  # Arkansas
year <- 2022

pums_data <- get_PUMS(geography,xnum_vars,xchr_vars,key,year,state)
print(pums_data)
