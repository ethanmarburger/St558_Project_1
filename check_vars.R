# year <- 2008
check_vars <- function(user_vars, geography, year)
{
  year_range <- seq(2010,2022, by = 1)
  if (!geography %in% c("all","region","division","state")){
    stop(paste(geography), " is not an allowed value for the geography variable. Options are: all, region, division, state")
  } 
  if (!year %in% year_range){
    stop(paste(year), " is not an allowed value for the year variable. Options are between 2010 - 2022")
  } 
  else if (year == 2020){
    stop(paste(year), " is not available from census.gov")
  }
  else{
    return(TRUE)
  }
}