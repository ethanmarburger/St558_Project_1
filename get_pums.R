library(dplyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(httr)

#store key
census_api_key("bdb1f6ff2e4982a1da64cd526532aa92dca5581c", install = TRUE, overwrite = TRUE)


get_PUMS <- function(geography, user_vars, key=sys.getenv("CENSUS_API_KEY"), 
                             year=2022, state = "*",county = NULL, zcta = NULL, 
                             survey="acs1", show_call = FALSE)
{
  #get check_vars metadata
  var_check <- check_vars(geography,year)
  # If check_vars returns a message (missing vars), stop the function
  if (!isTRUE(var_check)) {
    stop(var_check)  # Stops and prints the error message from check_vars
  }
  
  #get base URL
  baseURL <- paste("https://api.census.gov/data",
                   as.character(year), "acs",
                   survey, "pums", sep = "/")
  
  #get parameters from function args
  paramsURL <- paste0("?get=", user_vars, "&for=", geography, ":", state, "&key=", key)
  apiURL <- (paste0(baseURL,paramsURL))
  
  #check
  print(apiURL)
  
  response <- httr::GET(apiURL)
  
  # Convert API response to a tibble |>
  #set factor levels and convert data types
  var_metadata <- get_var_metadata(user_vars)
  parsed_data <- return_tibble(response$content) 
  # |> 
  #   clean_names()
  
  #return a list with two tibbles
  api_data <- list(api_data = parsed_data, api_metadata = var_metadata)
  
  # Set factor levels and types for the tibble
  parsed_data <- set_factor_levels_types(parsed_data, var_metadata)
  return(api_data)
}

#Test-9001


