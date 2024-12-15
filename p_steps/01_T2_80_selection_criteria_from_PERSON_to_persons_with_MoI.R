########################################################%##
#                                                          #
####  COMPUTE D3_selection_criteria_from_PERSON_to_persons_with_MoI
  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_selection_criteria_from_PERSONS_to_persons_with_MoI"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import input datasets

dispensings <- readRDS(file.path(thisdirinput, "D3_dispensings_DOACs.rds"))
episodes <- readRDS(file.path(thisdirinput, "D3_episodes_of_treatment.rds"))
persons <- readRDS(file.path(thisdirinput, "D3_PERSONS.rds"))

# compute the dataset

processing <- unique(dispensings[,.(person_id)])
processing<- merge(processing,persons[,not_in_registry := 0], all.x = T, by = "person_id")
processing <- processing[is.na(not_in_registry), not_in_registry := 1]
processing <- processing[, no_gender_or_birthdate := fifelse(not_in_registry == 1 | is.na(gender) | is.na(birth_date),1,0)]

# select episodes that overlap the study period
episodes <- episodes[start_date <= study_end_date & end_date >= study_start_date,]

# keep only persons who have an episode overlapping the study period
persons_with_episode <- unique(episodes[,.(person_id)])
persons_with_episode[,no_episode_of_treatment_overlapping_the_study_observation_period := 0]
processing <- merge(processing,persons_with_episode, all.x = T, by = "person_id")
processing <- processing[is.na(no_episode_of_treatment_overlapping_the_study_observation_period), no_episode_of_treatment_overlapping_the_study_observation_period := 1]
################################
# clean

tokeep <- c("person_id","not_in_registry","no_gender_or_birthdate",	"no_episode_of_treatment_overlapping_the_study_observation_period")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_selection_criteria_from_PERSON_to_persons_with_MoI"
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
