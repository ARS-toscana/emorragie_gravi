# create D3_TD_condition
# for condition in TD_variables_condition define din 5_variable_lists
# contains the time-dependent evolution of the binary variable condition. Only changes of status are recorded. UoO are the persons in the source population and are observed between entry_cohort and exit_cohort
# data model: person_id date value_of_variable



# author: Rosa Gini and Sabrina Giometto

# v 0.1 25 Nov 2025



#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_TD_condition"
  TD_variables_condition <- c("VAR_AMI","VAR_FAT")
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import

...

# processing

# processing <- 
# 
# ################################
# # clean
# 
# tokeep <- c("person_id","date","value_of_variable")
# 
# processing <- processing[, ..tokeep]
# 
# setorderv(
#   processing,
#    c("person_id","date")
# )
# 
# 
# #########################################
# # save
# 
# outputfile <- processing
# 
# nameoutput <- "D3_bleeding_events"
# nameoutputext <- paste0(nameoutput,".rds")
# assign(nameoutput, outputfile)
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
