library(stringr)
library(dplyr)

#extract times

#JWMNP
jwmnp <- dt_data
unique(jwmnp)

jwmnp <- jwmnp |>
  mutate(jwmnp_dt = duration(JWMNP, "minutes"))

jwmnp_look <- jwmnp |>
  filter(jwmnp_dt > duration(0, "minutes")) |>
  # glimpse()
  is.Date(jwmnp$jwmnp_dt)
is.duration(jwmnp$jwmnp_dt)

#jwap
extract_times <- function(jwap_string){
  print(jwap_string)
  
  #handle N/A cases in jwap
  if(str_detect(jwap_string, "N/A")){
    return(list(first_24 = "NA", second_24 = "NA"))
  }
  
  parts <- str_split(jwap_string, " ")[[1]]
  first_time <- parts[1]
  #parse_date_time function requires "pm" or "am". data contains "p.m." or "a.m."
  first_ampm <- str_replace_all(parts[2],"\\.","")
  second_time <- parts[4]
  second_ampm <- str_replace_all(parts[5],"\\.","")
  
  if(first_ampm == "pm" | first_ampm == "am"){
    first_24 <- convert_to_24_hour(first_time,first_ampm)
  }
  if(second_ampm == "pm" | second_ampm == "am"){
    second_24 <- convert_to_24_hour(second_time,second_ampm)
  }
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
  print(format(midpoint, "%H:%M"))
  return(format(midpoint, "%H:%M"))
}


#Testing
# jwap_tibbs <- tibble(jwap = c("9:40 p.m. to 9:44 p.m.", "10:20 a.m. to 10:24 a.m.", "N/A (not a worker; worker who worked from home)"))
# w <- calculate_midpoint(extract_times(jwap_tibbs$jwap))

# time_parsed <- map(jwap_tibbs$jwap, extract_times)
# midpt <- map_chr(time_parsed, calculate_midpoint)

# df <- jwap_tibbs |>
#   mutate(
#     time_parsed = map(jwap_tibbs$jwap, extract_times),  # Extract times
#     midpoint = map_chr(time_parsed, calculate_midpoint)  # Calculate midpoints
#   )
