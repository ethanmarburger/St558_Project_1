spec_vars <- tibble(varname = c("PWGTP","AGEP","GASP","GRPIP","JWAP","JWDP","JWMNP","FER","HHL","HISPEED","JWAP","JWDP","JWTRNS","SCH","SCHL","SEX"),
                    type = c("num","num","num","num","num","num","num","chr","chr","chr","chr","chr","chr","chr","chr","chr"))

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
  var_list <- unlist(str_split(var_list,","))
  
  in_var_list <- spec_vars |>
    filter(varname %in% var_list)
  
  )
}

user_vars <- "SEX,PWGTP,AGEP,GASP,GRPIP,JWAP,JWDP,JWMNP"
x <- convert_types(user_vars)


