install.packages("tidycensus")
install.packages("jsonlite")
install.packages("httr")

library(dplyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(httr)

#store key
census_api_key("bdb1f6ff2e4982a1da64cd526532aa92dca5581c", install = TRUE, overwrite = TRUE)

#from Project 1 documentation
# pums_URL <- "https://api.census.gov/data/2022/acs/acs1/pums?get=SEX,PWGTP,MAR&for=state:37&SCHL=24"

# From Lesson
# test_PUMS_GET <- httr::GET(pums_URL)
# str(test_PUMS_GET, max.level = 1)
# parsed_PUMS <- fromJSON(rawToChar(test_PUMS_GET$content))
# test_PUMS_tibble <- as_tibble(parsed_PUMS)
# test_PUMS_tibble |>
#   select(everything())


get_PUMS <- function(geography, optional_vars, key=sys.getenv("CENSUS_API_KEY"), year=2022,
                     state = "*",county = NULL, zcta = NULL, survey="acs1", show_call = FALSE){
  #get base URL
  baseURL <- paste("https://api.census.gov/data",
                   as.character(year), "acs",
                   survey, "pums", sep = "/")
  
  #get parameters from function args
  paramsURL <- paste0("?get=", optional_vars, "&for=", geography, ":", state, "&key=", key)
  apiURL <- (paste0(baseURL,paramsURL))
  
  #check
  print(apiURL)
  
  response <- httr::GET(apiURL)
  
  #Data Checks
  if (status_code(response) == 200){
    #specify vars to return
    allowed_vars <- c("PWGTP","MAR","AGEP","GASP","GRPIP","JWAP","JWDP","JWMNP","FER","HHL","HISPEED","JWAP","JWDP","JWTRNS","SCH","SCHL","SEX")
    optional_vars <- unlist(strsplit(optional_vars, ","))
    if (!all(optional_vars %in% allowed_vars)) {
      stop("User provided variable list only allows for variables: PWGTP,MAR,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP,FER,HHL,HISPEED,JWAP,JWDP,JWTRNS,SCH,SCHL,and SEX")
    }
    else{
      return_tibble(response$content)
    }
  }
  else {
    stop("Something wrong with query: ", status_code(response))
  }
  
}

return_tibble <- function (content)
{
  parsed_data <- as_tibble(fromJSON(rawToChar(content)))
  
  #set column names from first row
  col_names <- parsed_data[1,]
  parsed_data <- parsed_data[-1,]
  parsed_data <- setNames(parsed_data, col_names) 
  return(parsed_data)  
}

#Test-9001

geography <- "state"
user_vars <- "SEX,PWGTP,MAR,HISPEED"
key <- "bdb1f6ff2e4982a1da64cd526532aa92dca5581c"
state <- "05"  # Arkansas
year <- 2022

pums_data <- get_PUMS(geography,user_vars,key,year,state)
print(pums_data)

#-------------------------------------------------------------------------------

# Function to specify multiple years of survey data (with other variables) and then combine the data into one final tibble

PUMS_multi_year <- function(geography, optional_vars, years,
                            key = Sys.getenv("CENSUS_API_KEY"),
                            state = "*", county = NULL, zcta = NULL,
                            survey = "acs1", show_call = FASLE) {
  
  # Empty list to store data for each year
  list_data <- list()
  
  # Looping through each year and calling the get_PMS function
  for (year in years) {
    year_data <- get_PUMS(geography = geography,
                          optional_vars = optional_vars,
                          key = key,
                          year = year,
                          state = state,
                          county = county,
                          zcta = zcta,
                          survey = survey,
                          show_call = show_call)
    
    # Adding new column to data
    year_data <- year_data |>
      mutate(year = as.character(year))
    
    # Adding data for year to our empty list
    list_data[[as.character(year)]] <- year_data
  }
  
  # Combing all yearly data into a tibble
  years_tibble <- bind_rows(list_data)
  
  # Added a "census" class as later instructed
  class(years_tibble) <- c("census", class(years_tibble))
  
  return(years_tibble)
}

# Testing PUMS_multi_year function

years <- c(2022, 2021) # 2020 does not exist

PUMS_multi_year_test <- PUMS_multi_year(geography, user_vars, years, key, state)
print(PUMS_multi_year_test)

print(tail(PUMS_multi_year_test)) # Checking for the 2021 variable

#------------------------------------------------------------------------------

# Writing a Generic Function for Summarizing

#Run these in your console:
plot.function #what is used for a class = function
getS3method("plot","data.frame") #what is used for a class = data frame
plot

# Added additional class of "census" to years_tibble in PUMS_multi_year function

# Census summary function

summary.census <- function(tibble_class_census, 
                           numeric_variables = NULL, 
                           categorical_variables = NULL) {
  
  # Selecting numerical variables
  numeric_vector <- names(select_if(tibble_class_census, is.numeric))
  
  weight_vector <- NULL
  weight_vector <-  as.numeric(tibble_class_census$PWGTP)
  
  # Selecting categorical variables
  categorical_variables <- names(select_if(tibble_class_census, 
                                           is.character))
  
  # List to store census summary results 
  census_summary_results <- list()
  
  # Function to obtain weighted mean
  weighted_mean <- function(numeric_vector, weight_vector = NULL) {
    if (!is.null(weight_vector)) {
      return(sum(numeric_vector * weight_vector, na.rm = TRUE) / 
               sum(weight_vector, na.rm = TRUE))
    } else {
      return(mean(numeric_vector, na.rm = TRUE))
    }
  }
  
  # Function to find weighted standard deviation
  weighted_sd <- function(numeric_vector, weight_vector = NULL) {
    sample_mean <- weighted_mean(numeric_vector, weight_vector)
    if (!is.null(weight_vector)) {
      return(sqrt(sum(weight_vector * (numeric_vector^2), na.rm = TRUE) / 
                    sum(weight_vector, na.rm = TRUE) - sample_mean^2))
    } else {
      return(sd(numeric_vector, na.rm = TRUE))
    }
  }
  
  # Numeric summarizer
  for (num_variables in numeric_variables) {
    var_data <- as.numeric(tibble_class_census[[num_variables]])
    mean_val <- weighted_mean(var_data, weight_vector)
    sd_val <- weighted_sd(var_data, weight_vector)
    
    # Storing results in empty list
    census_summary_results[[num_variables]] <- list(mean = mean_val, sd = sd_val)
  }
  
  # Categorical summarizer 
  for (cat_variables in categorical_variables) {
    counts <- table(census_summary_results[[cat_variables]])
    census_summary_results[[cat_variables]] <- counts
  }
  
  return(census_summary_results)
} 

# Testing Summarizer Function

numeric_variables <- c("PWGTP", "state", "year", "HISPEED")
categorical_variables <- c("SEX", "MAR")

census_summary <- summary.census(tibble_class_census = PUMS_multi_year_test, 
                                 numeric_variables = numeric_variables, 
                                 categorical_variables = categorical_variables)


print(census_summary)
