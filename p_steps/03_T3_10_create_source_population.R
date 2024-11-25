########################################################%##
#                                                          #
####  COMPUTE D3_source_population
  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_source_population"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import input datasets

processing <- readRDS(file.path(thisdirinput, "D4_persons_with_MoI"))

episodes <- readRDS(file.path(thisdirinput, "D3_episodes_of_treatment"))
bleeding <- readRDS(file.path(thisdirinput, "D3_bleeding_events")) 

# compute the dataset

# processing <- ..

# ################################
# # clean
# 
# tokeep <- c("person_id")
# 
# processing <- processing[, ..tokeep]
# 
# setorderv(
#   processing,
#    c("person_id")
# )
# 
# 
# #########################################
# # save
# 
# outputfile <- processing
# 
# nameoutput <- "D3_source_population"
# nameoutputext <- paste0(nameoutput,".rds")
# assign(nameoutput, outputfile)
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
# 
# 
# # compare output with ground truth with integer dates
# # Define the reference date
# ref_date <- as.Date("2015-01-01")
# 
# # Calculate the difference in days and replace dates
# vectordates <- c("start_date","end_date")
# for (variable in vectordates) {
#   processing[, (variable) := as.integer(get(variable) - ref_date)]
# }
# #
# # fwrite(processing,file = "C:/temp/temp.csv")
