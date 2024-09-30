# spec_vars <- tibble(varname = c("PWGTP","AGEP","GASP","GRPIP","JWAP","JWDP",
#                                 "JWMNP","FER","HHL","HISPEED","JWTRNS","SCH",
#                                 "SCHL","SEX"),
#                     type = c("num","num","num","num","num","num","num","chr"
#                              ,"chr","chr","chr","chr","chr","chr"),
#                     required = c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
#                                  FALSE,FALSE,FALSE,FALSE,FALSE,TRUE),
#                     dt = c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,
#                            FALSE,FALSE,FALSE,FALSE))
#                     


check_vars <- function(var_list){
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
  #unlist user specified vars to vector
  var_list <- unlist(str_split(var_list,","))
  
  required_vars <- spec_vars |>
    filter(required == TRUE) |>
    pull(varname)
  
  missing_vars <- required_vars[!(required_vars %in% var_list)]
  
  if (length(missing_vars) > 0){
    return(paste("Missing required vars in function call: ", paste(missing_vars, collapse = ", ")))
  }
  else{
    var_tibbs <- spec_vars |> filter(varname %in% var_list)
    return(var_tibbs)
  }
}

user_vars <- "PWGTP,SEX,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
x <- check_vars(user_vars)
print(x)

