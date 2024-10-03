# library(readr)
# library(tidyverse)
# library(tidycensus)
# library(jsonlite)
# library(httr)

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
  jwmnptst <- return_tibble(response$content)
  # |>
  #   clean_names()
  # Set factor levels and types for the tibble

  tst <- set_factor_levels_types(parsed_data, var_metadata)
  
  parsed_data <- set_factor_levels_types(parsed_data, var_metadata)
  
  # df <- parsed_data$JWAP |>
  #     mutate(
  #       time_parsed = map(JWAP, extract_times),  # Extract times
  #       midpoint = map_chr(time_parsed, calculate_midpoint)  # Calculate midpoints
  #     )
  datetime_var_chk <- c("JWAP", "JWDP", "JWMNT")
  
  for (varname in datetime_var_chk){
    if (varname == "JWAP"){
      parsed_data <- parsed_data |>
        mutate(
          JWAP = as.character(JWAP),
          time_parsed = map(JWAP, extract_times),  # Extract times into a list of parsed times
          first_24 = map_chr(time_parsed, "first_24"),  # Extract first 24-hour time
          second_24 = map_chr(time_parsed, "second_24"),  # Extract second 24-hour time
          midpoint = map_chr(time_parsed, calculate_midpoint)) |>
        rename(jwap_t = midpoint) |>
        select(-first_24, -second_24 , -time_parsed)
    }
    else if (varname == "JWDP"){
      parsed_data <- parsed_data |>
        mutate(
          JWDP = as.character(JWDP),
          time_parsed = map(JWDP, extract_times),  # Extract times into a list of parsed times
          first_24 = map_chr(time_parsed, "first_24"),  # Extract first 24-hour time
          second_24 = map_chr(time_parsed, "second_24"),  # Extract second 24-hour time
          midpoint = map_chr(time_parsed, calculate_midpoint)) |>
        rename(jwdp_t = midpoint) |>
        select(-first_24, -second_24 , -time_parsed)
    }
    else if (varname == "JWMNP"){
      parsed_data <- parsed_data |>
        # filter(JWMNP == as.character("0")) |>
        mutate(
          jwmnp_c = as.character(JWMNP),
          jwmnp_n = suppressWarnings(as.numeric(jwmnp_c)),  # Suppress warnings and handle NA conversion
          jwmnp_mins = ifelse(is.na(jwmnp_n), NA, jwmnp_n)
        ) |>
        select(-jwmnp_c, -jwmnp_n)
    }
}
  # munge dates
  # x <- extract_times(response$JWAP)
  #return a list with two tibbles
  api_data <- list(api_data = parsed_data, api_metadata = var_metadata, jwmnptst = jwmnptst, tst = tst)

  return(api_data)
}

#Test-9001
# xnum_vars <- "AGEP, GASP, GRPIP, JWAP, JWDP, JWMNP"
# xnum_vars <- NULL
# xchr_vars <- "FER, HHL, HISPEED, JWTRNS, SCH, SCHL, SEX"
# xchr_vars <- NULL
# geography <- "state"
# user_vars <- "SEX,FER,HHL,PWGTP,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
# user_vars <- "SEX,PWGTP,MAR,HISPEED"
# user_vars <- "SEX"
# key <- "bdb1f6ff2e4982a1da64cd526532aa92dca5581c"
# state <- "05"  # Arkansas
# year <- 2022