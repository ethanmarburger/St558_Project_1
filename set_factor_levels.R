library(httr)
#get factor levels
get_pums_factor_levels <- function(var_names){
  #initialize list
  levels_list <- list()
  #get base URL
  for (var_name in var_names){
    url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums/variables/",var_name,".json")
    response <- GET(url)
    if (status_code(response) == 200) {
      data <- fromJSON(content(response,"text"))
      levels <- data$values$item
      levels_list[[var_name]] <- levels
    }
  }
  
  return(levels_list)
  # return(var_name$values$item)
}

x <- get_pums_factor_levels(c("SEX","PWGTP"))

# https://api.census.gov/data/2022/acs/acs1/pums/variables.json