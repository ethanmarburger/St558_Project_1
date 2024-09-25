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
    allowed_vars <- c("PWGTP","MAR","AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP","FER","HHL","HISPEED","JWAP","JWDP","JWTRNS","SCH","SCHL","SEX")
    optional_vars <- unlist(strsplit(optional_vars, ","))
    if (!all(optional_vars %in% allowed_vars)) {
      stop("User provided variable list only allows for variables: PWGTP,MAR,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP,FER,HHL,HISPEED,JWAP,JWDP,JWTRNS,SCH,SCHL,and SEX")
    }
    else{
      
      parsed_data <- as_tibble(fromJSON(rawToChar(response$content)))

      #set column names from first row
      col_names <- parsed_data[1,]
      parsed_data <- parsed_data[-1,]
      parsed_data <- setNames(parsed_data, col_names) 
      return(parsed_data)
    }
  }
  else {
    stop("Something wrong with query: ", status_code(response))
  }

}


#Test-9001
geography <- "state"
user_vars <- "SEX,PWGTP,MAR,HISPEED"
key <- "bdb1f6ff2e4982a1da64cd526532aa92dca5581c"
state <- "37"  # North Carolina
year <- 2022

pums_data <- get_PUMS(geography,user_vars,key,year,state)
print(pums_data)

