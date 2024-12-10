########################################################%##
#                                                          #
####  CREATE D4_analytical_dataset  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 0.1 10 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D4_analytical_dataset"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import input datasets

cohort <- readRDS(file.path(thisdirinput, "D3_study_population.rds"))
events <- readRDS(file.path(thisdirinput, "D3_PERSONS.rds"))

# ..

# ################################
# # clean
# 
# tokeep <- c()
# 
# processing <- processing[, ..tokeep]
# 
# setorderv(
#   processing,
#   c("gender", "ageband","month")
# )


# #########################################
# # save
# 
# outputfile <- processing
# 
# nameoutput <- "D4_analytical_dataset"
# nameoutputext <- paste0(nameoutput,".rds")
# assign(nameoutput, outputfile)
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

