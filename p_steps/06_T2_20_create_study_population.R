########################################################%##
#                                                          #
####  CREATE D3_study_population  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.0 10 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_study_population"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import

cohort <- readRDS(file.path(thisdirinput, "D3_source_population.rds"))
events <- readRDS(file.path(thisdirinput, "D3_bleeding_events.rds"))
persons <- readRDS(file.path(thisdirinput, "D3_PERSONS.rds"))
obsperiods <- readRDS(file.path(thisdirinput, "D3_clean_spells.rds"))
obsperiods <- obsperiods[is_the_study_spell == 1,]

# process: cohort is excluding already 30 days after all events 

processing <- merge(cohort,events, by = "person_id", all = F)
processing <- processing[date >= study_start_date & date >= entry_cohort & date <= exit_cohort,]
processing <- processing[,.(person_id,birth_date,gender,date,event)]
processing <- merge(processing,persons[,.(person_id,death_date)], by = "person_id", all = F)
processing <- merge(processing,obsperiods, by = "person_id", all = F)
processing <- processing[date >= entry_spell_category & date <= exit_spell_category,]
processing[, end_followup_d := pmin(exit_spell_category, date + 30)]

setnames(processing,"date","date_bleeding")


################################
# clean

tokeep <- c("person_id","birth_date","gender","death_date","date_bleeding","event","end_followup_d")

processing <- processing[, ..tokeep]

setorderv(
  processing,
  c("person_id","date_bleeding")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_study_population"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
