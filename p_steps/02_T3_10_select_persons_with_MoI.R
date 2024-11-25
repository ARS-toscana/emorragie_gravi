########################################################%##
#                                                          #
####  COMPUTE D4_select_persons_with_MoI
  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D4_persons_with_MoI"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import input datasets

processing <- readRDS(file.path(thisdirinput, "D3_selection_criteria_from_PERSON_to_persons_with_MoI.rds"))

# compute the dataset

processing <- CreateFlowChart(
  dataset = processing,
  listcriteria = c("not_in_registry","no_gender_or_birthdate",	"no_episode_of_treatment_overlapping_the_study_observation_period"),
  flowchartname = "Flowchart_exclusion_criteria")

fwrite(Flowchart_exclusion_criteria, paste0(direxp, "D5_Flowchart_exclusion_criteria.csv"))

processing <- data.table::as.data.table(processing)

################################
# clean

tokeep <- c("person_id")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D4_persons_with_MoI"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
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
