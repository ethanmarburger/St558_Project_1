library(dplyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(httr)


#check vars and return a tibble containing metadata
# check_vars <- function(var_list){
#   spec_vars <- tibble(varname = c("PWGTP","AGEP","GASP","GRPIP","JWAP","JWDP",
#                                   "JWMNP","FER","HHL","HISPEED","JWTRNS","SCH",
#                                   "SCHL","SEX"),
#                       type = c("num","num","num","num","num","num","num","chr"
#                                ,"chr","chr","chr","chr","chr","chr"),
#                       required = c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
#                                    FALSE,FALSE,FALSE,FALSE,FALSE,TRUE),
#                       dt = c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,
#                              FALSE,FALSE,FALSE,FALSE) 
#   )
#   
#   
#   
#   #unlist user specified vars to vector
#   var_list <- unlist(str_split(var_list,","))
#   
#   required_vars <- spec_vars |>
#     filter(required == TRUE) |>
#     pull(varname)
#   
#   missing_vars <- required_vars[!(required_vars %in% var_list)]
#   
#   if (length(missing_vars) > 0){
#     return( paste("Missing required vars in function call: ", paste(missing_vars, collapse = ", ")))
#   }
#   else{
#     var_tibbs <- spec_vars |> filter(varname %in% var_list)
#     return(var_tibbs)
#   }
# }


#return API query as tibble
return_tibble <- function (content)
{
  parsed_data <- as_tibble(fromJSON(rawToChar(content)))
  
  #set column names from first row
  col_names <- parsed_data[1,]
  parsed_data <- parsed_data[-1,]
  parsed_data <- setNames(parsed_data, col_names) 
  return(parsed_data)  
}


get_PUMS_minimal <- function(geography, user_vars, key=sys.getenv("CENSUS_API_KEY"), 
                             year=2022, state = "*",county = NULL, zcta = NULL, 
                             survey="acs1", show_call = FALSE)
{
  #get check_vars metadata
  var_check <- check_vars(user_vars)
  # If check_vars returns a message (missing vars), stop the function
  if (is.character(var_check)) {
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

geography <- "state"
user_vars <- "SEX,FER,HHL,PWGTP,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
# user_vars <- "SEX,PWGTP,MAR,HISPEED"
key <- "bdb1f6ff2e4982a1da64cd526532aa92dca5581c"
state <- "05"  # Arkansas
year <- 2022

pums_data <- get_PUMS_minimal(geography,user_vars,key,year,state)
print(pums_data)
