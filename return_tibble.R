
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

#get factor levels
get_pums_factor_levels <- function(var_name){
  #get base URL
  url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums/variables/",var_name,".json")
  response <- GET(url)
  
  data <- fromJSON(content(response,"text"))
  return(data$values$item)
  # return(var_name$values$item)
}

get_PUMS_minimal <- function(geography, optional_vars, key=sys.getenv("CENSUS_API_KEY"), 
                             year=2022, state = "*",county = NULL, zcta = NULL, 
                             survey="acs1", show_call = FALSE)
{
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
  
  return_tibble(response$content)
  
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
