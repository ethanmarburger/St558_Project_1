#get factor levels
get_pums_factor_levels <- function(var_name){
  #get base URL
  url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums/variables/",var_name,".json")
  response <- GET(url)
  
  data <- fromJSON(content(response,"text"))
  return(data$values$item)
  # return(var_name$values$item)
}

x <- get_pums_factor_levels("SEX")

# https://api.census.gov/data/2022/acs/acs1/pums/variables.json