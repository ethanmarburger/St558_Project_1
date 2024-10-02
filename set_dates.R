set_dates <- function(api_data, api_metadata){
  #filter date variables
  dt_vars <- api_metadata |>
    filter(api_metadata$dt == TRUE)
    
  #convert to datetimes
  
  #return
}

dt_data <- pums_data$api_data
dt_meta <- pums_data$api_metadata

#work on the calculation first
library(stringr)
library(dplyr)
#jwap
jwap_tibbs <- tibble(jwap = c("9:40 p.m. to 9:44 p.m.", "10:20 a.m. to 10:24 a.m.", "N/A (not a worker; worker who worked from home)"))
glimpse(jwap_tibbs)
jwap1 <- "9:40 p.m. to 9:44 p.m."

jt <- jwap_tibbs |>     
  mutate(
    parts = str_split(jwap, " "),
    first_two = paste(parts[[1]][1]),
    second = paste(parts[[1]][4])
  )
jt$parts[[1]]

jwap_tibbs <- tibble(jwap = c("9:40 p.m. to 9:44 p.m.", "10:20 a.m. to 10:24 a.m.", "N/A (not a worker; worker who worked from home)"))

#convert to 24 hour
convert_to_24_hour <- function(time_str) {
  
  time_parsed <- parse_date_time(time_str, "I:M p")  # Parse AM/PM time
  format(time_parsed, "%H:%M")  # Convert to 24-hour format
}
# parts <- str_split(jwap_tibbs$jwap, " ")[[1]]
# first_time <- paste(strsplit(parts[1:2], " "), collapse= " ")
# print(first_time)

#extract times
extract_times <- function(jwap_string){
  print(jwap_string)
  parts <- str_split(jwap_string, " ")[[1]]
  first_time <- parts[1]
  first_ampm <- str_trim(parts[2])
  second_time <- parts[4]
  second_ampm <- str_trim(parts[5])
  #why does this work
  first_time_ampm <- paste(parts[1], parts[2])
  #but not this?
  # first_time_ampm <- paste(first_time, first_ampm)
  
  if(first_ampm == "p.m."){
    # first_24 <- paste(strsplit(parts[1:2], " "), collapse = "")
    # first_24 <- convert_to_24_hour(first_time)
  }
}

extract_times(jwap_tibbs$jwap)
# 
# parts <- str_split(jwap1, " ")[[1]]
# first <- hm(parts[1])
# second <- hm(parts[4])
# middies <- as.numeric(first + (second - first)/2)


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
# is.Date(jwmnp$jwmnp_dt)
# is.duration(jwmnp$jwmnp_dt)




#then fiture out flow control
# dt_vars <- dt_meta |>
#   filter(dt_meta$dt == TRUE) |>
# 
# dt_df <- dt_data |>
#   filter(dt_vars %in% dt_data)
# |>
  # mutate(jwap_dt = do some work)
# print(dt_vars)
# x <- set_dates(dt_data,dt_meta)
# 
