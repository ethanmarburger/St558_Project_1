library(dplyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(httr)


#check vars and return a tibble containing metadata
check_vars <- function(var_list){
  spec_vars <- tibble(varname = c("PWGTP","AGEP","GASP","GRPIP","JWAP","JWDP",
                                  "JWMNP","FER","HHL","HISPEED","JWTRNS","SCH",
                                  "SCHL","SEX"),
                      type = c("num","num","num","num","num","num","num","chr"
                               ,"chr","chr","chr","chr","chr","chr"),
                      required = c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                                   FALSE,FALSE,FALSE,FALSE,FALSE,TRUE),
                      dt = c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,
                             FALSE,FALSE,FALSE,FALSE) 
  )
  
  
  
  #unlist user specified vars to vector
  var_list <- unlist(str_split(var_list,","))
  
  required_vars <- spec_vars |>
    filter(required == TRUE) |>
    pull(varname)
  
  missing_vars <- required_vars[!(required_vars %in% var_list)]
  
  if (length(missing_vars) > 0){
    return( paste("Missing required vars in function call: ", paste(missing_vars, collapse = ", ")))
  }
  else{
    var_tibbs <- spec_vars |> filter(varname %in% var_list)
    return(var_tibbs)
  }
}


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

#get factor levels and set types
set_factor_levels_types <- function(var_tibbs, data_tibble) {
  # Process character variables and set them as factors with levels
  chr_vars <- var_tibbs %>% filter(type == "chr") %>% pull(varname)
  
  # Loop through chr_vars and fetch levels for factor variables
  data_tibble <- data_tibble %>%
    mutate(across(all_of(chr_vars), ~ {
      var_name <- cur_column()  # Get the current column name
      url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums/variables/", var_name, ".json")
      
      response <- GET(url)
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text", encoding = "UTF-8"))
        if (!is.null(data$values) && "item" %in% names(data$values)) {
          levels <- data$values$item
          return(factor(.x, levels = levels))
        } else {
          warning(paste("Levels for", var_name, "not found in API response"))
          return(.x)  # Return unchanged column
        }
      } else {
        warning(paste("Failed to fetch data for", var_name, ": HTTP status", status_code(response)))
        return(.x)  # Return unchanged column
      }
    }))
  
  # Process numeric variables and convert them to numeric
  num_vars <- var_tibbs %>% filter(type == "num") %>% pull(varname)
  
  data_tibble <- data_tibble %>%
    mutate(across(all_of(num_vars), as.numeric))
  
  return(data_tibble)
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
  
  # Convert API response to a tibble
  parsed_data <- return_tibble(response$content)
  
  # Set factor levels and types for the tibble
  parsed_data <- set_factor_levels_types(var_check, parsed_data)
  
  return(parsed_data)
  
}
  
#Test-9001

geography <- "state"
user_vars <- "SEX,PWGTP,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
# user_vars <- "SEX,PWGTP,MAR,HISPEED"
key <- "bdb1f6ff2e4982a1da64cd526532aa92dca5581c"
state <- "05"  # Arkansas
year <- 2022

pums_data <- get_PUMS_minimal(geography,user_vars,key,year,state)
print(pums_data)
