########################################################%##
#                                                          #
####  COMPUTE D5_results_from_analysis
####
#                                                          #
########################################################%##


# authors: Rosa Gini, Ersilia Lucenteforte

# v 0.1 28 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D5_results_from_analysis"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# import input datasets

input <- readRDS(file.path(thisdirinput, "D4_analytical_dataset.rds"))

# analysis

# processing <- ...
  
# ################################
# # clean
# 
# tokeep <- c(...)
# 
# results <- results[, ..tokeep]
# 
# setorderv(
#   results, c(...)
# )
# 
# 
# #########################################
# # save
# 
# outputfile <- results
# 
# nameoutput <- "D5_results_from_analysis"
# nameoutputext <- paste0(nameoutput,".rds")
# assign(nameoutput, outputfile)
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
