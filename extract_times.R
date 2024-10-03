library(stringr)
library(dplyr)
library(lubridate)
#extract times

#JWMNP
# jwmnp <- dt_data
# unique(jwmnp)
# 
# jwmnp <- jwmnp |>
#   mutate(jwmnp_dt = duration(JWMNP, "minutes"))
# 
# jwmnp_look <- jwmnp |>
#   filter(jwmnp_dt > duration(0, "minutes")) |>
#   # glimpse()
#   is.Date(jwmnp$jwmnp_dt)
#   is.duration(jwmnp$jwmnp_dt)

#jwap
extract_times <- function(jwap_value){
  # Handle N/A and missing cases
  if (str_detect(as.character(jwap_value), "N/A") | 
      is.na(jwap_value) |
      str_detect(as.character(jwap_value), "NA")) {
        return(list(first_24 = "NA", second_24 = "NA"))
  }
  
  # Split the string into parts (e.g., "9:40 p.m. to 9:44 p.m.")
  parts <- str_split(as.character(jwap_value), " ")[[1]]
  first_time <- parts[1]
  first_ampm <- str_replace_all(parts[2], "\\.", "")
  second_time <- parts[4]
  second_ampm <- str_replace_all(parts[5], "\\.", "")
  
  # Convert both times to 24-hour format
  first_24 <- convert_to_24_hour(first_time, first_ampm)
  second_24 <- convert_to_24_hour(second_time, second_ampm)
  
  return(list(first_24 = first_24, second_24 = second_24))
}

convert_to_24_hour <- function(time_str, ampm_str) {
  # Clean up the AM/PM string by removing periods
  full_time_str <- paste(time_str, ampm_str)  # Combine time and cleaned AM/PM
  time_parsed <- parse_date_time(full_time_str, "I:M p")  # Parse AM/PM time
  return(format(time_parsed, "%H:%M"))  # Convert to 24-hour format
  
}

calculate_midpoint <- function(time_parsed){
  second_24 <- as.POSIXct(time_parsed$second_24, format = "%H:%M")
  first_24 <- as.POSIXct(time_parsed$first_24, format = "%H:%M")
  
  #handle NA
  if(is.na(second_24) | is.na(first_24)) {
    return(NA)
  }
  
  midpoint <- first_24 + (second_24 - first_24)/2
  # print(format(midpoint, "%H:%M"))
  return(format(midpoint, "%H:%M"))
}


#Testing
# df <- pums_data$api_data |>
#   mutate(
#     time_parsed = map(JWAP, extract_times),  # Extract times into a list of parsed times
#     first_24 = map_chr(time_parsed, "first_24"),  # Extract first 24-hour time
#     second_24 = map_chr(time_parsed, "second_24"),  # Extract second 24-hour time
#     midpoint = map_chr(time_parsed, calculate_midpoint)  # Calculate midpoint between times
#   )
 # filtered_data <- pums_data$api_data |>
   # filter(str_detect(as.character(JWAP), "N/A") | is.na(JWAP) | str_detect(as.character(JWAP), "NA"))|>
   # filter(!(as.character(JWAP) %in% (c("N/A","NA", "", " ")))) |>
   # slice_head(n=100)
   
 
 
 # N/A (not a worker; worker who worked from home)
 
 
 
 # Join the patterns into a single regex using the OR operator "|"
 
 # Filter rows where JWAP column does not contain any of the patterns
 