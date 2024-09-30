#get metadata from specification for user provided variables
#metadata can be used for further checks, type conversion, and factor leveling
#get_var_metadata currently checks for required variables in a user provided 
#function call, but no other checks

get_var_metadata <- function(var_list){
  spec_vars <- tibble(varname = c("PWGTP","AGEP","GASP","GRPIP","JWAP","JWDP",
                                  "JWMNP","FER","HHL","HISPEED","JWTRNS","SCH",
                                  "SCHL","SEX"),
                      type = c("num","num","num","num","num","num","num","chr"
                               ,"chr","chr","chr","chr","chr","chr"),
                      required = c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                                   FALSE,FALSE,FALSE,FALSE,FALSE,TRUE),
                      dt = c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,
                             FALSE,FALSE,FALSE,FALSE) 
                      )
  #unlist user specified / API required vars to vector
  var_list <- unlist(str_split(var_list,","))
  
  #return required varnames as a vector to use in quality checks
  required_vars <- spec_vars |>
    filter(required == TRUE) |>
    pull(varname)
  
  #if required vars NOT in user provided list, collect them
  missing_vars <- required_vars[!(required_vars %in% var_list)]
  
  #return tibble for varnames in user provided list. This is the lookup table to
  #be used for checking variable types and factor levels
  if (length(missing_vars) > 0){
    stop(paste("Missing required vars in function call: ", paste(missing_vars, collapse = ", ")))
  }
  else{
    var_tibbs <- spec_vars |> 
      filter(varname %in% var_list) 
    # |>
    #   clean_names()
    return(var_tibbs)
  }
}


#Test-9002
# user_vars <- "PWGTP,SEX,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
# x <- get_var_metadata(user_vars)
# print(x)