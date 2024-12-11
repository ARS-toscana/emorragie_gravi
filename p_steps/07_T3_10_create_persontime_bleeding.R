########################################################%##
#                                                          #
####  COMPUTE D4_persontime_bleeding  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.0 20 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D4_persontime_bleeding"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import input datasets

cohort <- readRDS(file.path(thisdirinput, "D3_source_population.rds"))
events <- readRDS(file.path(thisdirinput, "D3_bleeding_events.rds"))

eventsbroad <- copy(events)
eventsbroad[,event := "bleeding_broad"]

events <- events[event == "bleeding_narrow",]
events <-   rbind(events,eventsbroad,fill= T)
# ..

cohort <- cohort[exit_cohort > entry_cohort,]

processing  <- CountPersonTime(
  Dataset_events = events,
  Dataset = cohort,
  Person_id = "person_id",
  Start_study_time = gsub('-', '', as.character(study_start_date)),
  End_study_time = gsub('-', '', as.character(study_end_date)),
  Start_date = "entry_cohort",
  End_date = "exit_cohort",
  Birth_date = "birth_date",
  Strata = c("gender"),
  Name_event = "event",
  Date_event = "date",
  Age_bands = Agebands_countpersontime,
  Increment = "month",
  Outcomes_nrec = c("bleeding_narrow","bleeding_broad"),
  Rec_period = 30,
  Unit_of_age = "year",
  include_remaning_ages = T,
  Aggregate = T
)

# processing[, Persontime := NULL]

setnames(processing, c( "Ageband"), c("ageband"))

for (event in c("none","bleeding_narrow","bleeding_broad")) {
  namept <- fifelse(event == "none","Persontime",paste0("Persontime_",event))
  namepy <- fifelse(event == "none","PY",paste0("PY_",event))
  processing[, (namepy) := get(namept) / 365.25]
}

################################
# clean

tokeep <- c("gender", "ageband", "month",  "Persontime","PY", "Persontime_bleeding_narrow","PY_bleeding_narrow", "bleeding_narrow_b", "Persontime_bleeding_broad","PY_bleeding_broad", "bleeding_broad_b")

processing <- processing[, ..tokeep]

setorderv(
  processing,
  c("gender", "ageband","month")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D4_persontime_bleeding"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

# 
# # # compare output with ground truth with integer dates
# # # Define the reference date
# # ref_date <- as.Date("2015-01-01")
# # 
# # # Calculate the difference in days and replace dates
# # vectordates <- c("start_d","end_d")
# # for (variable in vectordates) {
# #   processing[, (variable) := as.integer(get(variable) - ref_date)]
# # }
# # 
# # fwrite(processing,file = "C:/temp/temp.csv")
