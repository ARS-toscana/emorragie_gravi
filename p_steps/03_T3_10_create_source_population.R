########################################################%##
#                                                          #
####  COMPUTE D3_source_population
####
#                                                          #
########################################################%##


# author: Rosa Gini


# v 1.1 10 Dec 2024

# bleeding episodes are now retrieved from conceptsets directly

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

processing <- readRDS(file.path(thisdirinput, "D4_persons_with_MoI.rds"))

episodes <- readRDS(file.path(thisdirinput, "D3_episodes_of_treatment.rds"))

load(file.path(thisdirinput, "bleeding_narrow.RData"))

bleeding <- bleeding_narrow
setnames(bleeding,c("ID","DATE"),c("person_id","date"))

persons <- readRDS(file.path(thisdirinput, "D3_PERSONS.rds"))
# compute the dataset

processing <- merge(processing,episodes, all = F, by = "person_id")

# remove days of bleeding and 30 days after

# bleeding <- bleeding[event == "bleeding_broad", ]

bleeding[, end_d := date + 30]

bleeding <- CreateSpells(
  dataset = bleeding,
  id = "person_id" ,
  start_date = "date",
  end_date = "end_d"
)

bleeding[, entry_spell_category := entry_spell_category + 1 ]

processing <- GenerateTDDataset(datasets = list(processing,bleeding),
                                UoO_vars = c("person_id","person_id"),
                                start_d_vars = c("start_date","entry_spell_category"),
                                end_d_vars = c("end_date","exit_spell_category"),
                                keep_auxiliary_variables = F,
                                TD_variables = list(list("treatment"),list("num_spell")),
                                keep_periods_observed_by = "first"
)

processing <- processing[is.na(num_spell),]

# add birthdate and gender

processing <- merge(processing,persons, all.x = T, by = "person_id")

# names

setnames(processing,c("start_date","end_date"),c("entry_cohort","exit_cohort"))

################################
# clean

tokeep <- c("person_id","gender","birth_date","entry_cohort","exit_cohort", "treatment")

processing <- processing[, ..tokeep]

setorderv(
  processing,
  c("person_id", "entry_cohort")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_source_population"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

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
