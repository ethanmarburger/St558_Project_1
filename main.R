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


geography <- "state"
user_vars <- "SEX,FER,HHL,PWGTP,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
# user_vars <- "SEX,PWGTP,MAR,HISPEED"
key <- "bdb1f6ff2e4982a1da64cd526532aa92dca5581c"
state <- "05"  # Arkansas
year <- 2022

pums_data <- get_PUMS_minimal(geography,user_vars,key,year,state)
print(pums_data)