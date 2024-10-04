#return API query as tibble
return_tibble <- function (content)
{
  parsed_data <- as_tibble(fromJSON(rawToChar(content)))
  
  #set column names from first row
  col_names <- parsed_data[1,]
  parsed_data <- parsed_data[-1,]
  parsed_data <- setNames(parsed_data, col_names) 
  
  #save data to disk
  # write_csv(parsed_data, "raw_api.csv")
  return(parsed_data)  
}