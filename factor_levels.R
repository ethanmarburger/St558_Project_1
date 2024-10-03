#get factor levels
get_pums_factor_levels <- function(var_names){
  print(var_names)
  #initialize list
  levels_list <- list()
  var_names <- unlist(strsplit(var_names,","))
  print(var_names)
  #get base URL
  for (var_name in var_names){
    url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums/variables/",var_name,".json")
    print(url)
    response <- GET(url)
    if (status_code(response) == 200) {
      data <- fromJSON(content(response,"text"))
      levels <- data$values$item
      levels_list[[var_name]] <- levels
    }
  }
  return(levels_list)
}

#get factor levels and set types
set_factor_levels_types <- function(data_tibble, var_tibbs) {
  # Process character variables and set them as factors with levels
  chr_vars <- var_tibbs|>
    filter(type == "chr") |>
    pull(varname)
  # glimpse(data_tibble)
  # Loop through chr_vars and fetch levels for factor variables
  data_tibble <- data_tibble |>
    mutate(across(all_of(chr_vars), ~ {
      var_name <- cur_column()  # Get the current column name
      url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums/variables/", var_name, ".json")

      response <- GET(url)
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text", encoding = "UTF-8"))
        
        if (!is.null(data$values) && "item" %in% names(data$values)) {
          levels <- names(data$values$item)
          labels <- data$values$item
          return(factor(.x, levels = levels, labels = labels))
          
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
  num_vars <- var_tibbs |>
    filter(type == "num") |>
    pull(varname)

  data_tibble <- data_tibble |> 
    mutate(across(all_of(num_vars), as.numeric))

  return(data_tibble)
}



#Checks

# x <- get_pums_factor_levels("JWMNP")
# devdata_tibbs <- pums_data$api_data
# glimpse(devdata_tibbs)
# devvar_tibbs <- pums_data$api_metadata
# glimpse(devvar_tibbs)
# devx <- set_factor_levels_types(devdata_tibbs, devvar_tibbs)

# print(unique(devx$JWAP))
# print(unique(devx$SEX))
# print(unique(devx$FER))
