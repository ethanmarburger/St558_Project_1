library(dplyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(httr)

#store key
# census_api_key("bdb1f6ff2e4982a1da64cd526532aa92dca5581c", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

get_PUMS <- function(geography, num_vars = NULL, chr_vars = NULL , key = sys.getenv("CENSUS_API_KEY"), 
                             year = 2022, state = "05",county = NULL, zcta = NULL, 
                             survey = "acs1", show_call = FALSE)
{
  #munge var lists
  if(is.null(num_vars)){
    num_vars <- c("PWGTP", "AGEP") 
  }
  if(is.null(chr_vars)){
    chr_vars <- c("SEX")
  }

  user_vars <-c(num_vars, chr_vars)
  
  
  
  #get var metadata
  var_metadata <- get_var_metadata(user_vars)
  
  #get vars string for API call after returning metadata
  api_var_string <- paste(var_metadata$varname,collapse = ",")
  
  #check vars values in range
  var_check <- check_vars(user_vars,geography,year)
  # If check_vars returns a message (missing vars), stop the function
  if (!isTRUE(var_check)) {
    stop(var_check)  # Stops and prints the error message from check_vars
  }
  
  #get base URL
  baseURL <- paste("https://api.census.gov/data",
                   as.character(year), "acs",
                   survey, "pums", sep = "/")
  
  
  paramsURL <- paste0("?get=", api_var_string, "&for=", geography, ":", state, "&key=", key)
  apiURL <- (paste0(baseURL,paramsURL))
  
  #check
  print(apiURL)
  
  response <- httr::GET(apiURL)
  
  # Convert API response to a tibble |>
  #set factor levels and convert data types
  
  parsed_data <- return_tibble(response$content) 
  # |> 
  #   clean_names()
  # Set factor levels and types for the tibble
  
  parsed_data <- set_factor_levels_types(parsed_data, var_metadata)
  # df <- jwap_tibbs |>
  #     mutate(
  #       time_parsed = map(jwap_tibbs$jwap, extract_times),  # Extract times
  #       midpoint = map_chr(time_parsed, calculate_midpoint)  # Calculate midpoints
  #     )

  #return a list with two tibbles
  api_data <- list(api_data = parsed_data, api_metadata = var_metadata)
  
  return(api_data)
}

#Test-9001
xnum_vars <- "JWAP,JWDP,JWMNP"
# xnum_vars <- NULL
xchr_vars <- "HISPEED"
# xchr_vars <- NULL
# geography <- "state"
# user_vars <- "SEX,FER,HHL,PWGTP,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
# user_vars <- "SEX,PWGTP,MAR,HISPEED"
# user_vars <- "SEX"
# key <- "bdb1f6ff2e4982a1da64cd526532aa92dca5581c"
# state <- "05"  # Arkansas
# year <- 2022

pums_data <- get_PUMS(geography,xnum_vars,xchr_vars,key,year,state)
# print(pums_data)
