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
obsperiods <- readRDS(file.path(thisdirinput, "D3_clean_spells.rds"))
obsperiods <- obsperiods[is_the_study_spell == 1,]

# process: cohort is excluding already 30 days after all events 

processing <- merge(cohort,events, by = "person_id", all = F)
processing <- processing[date >= entry_cohort & date <= exit_cohort,]
processing <- processing[,.(person_id,birthdate,sex,date,event)]
processing <- merge(processing,obsperiods, by = "person_id", all = F)
processing <- processing[date >= entry_spell_category & date <= exit_spell_category,]
processing[, end_followup_d := pmin(exit_spell_category, date + 30)]

# set names


setnames(processing,"date","date_bleeding")
processing <- processing[event == "bleeding_narrow", type_bleeding := "narrow"]
processing <- processing[event == "bleeding_possible", type_bleeding := "possible"]

# period

# end_date_period <- list()
# end_date_period[["1"]] <- ymd(20190708)
# end_date_period[["2"]] <- ymd(20200305)
# end_date_period[["3a"]] <- ymd(20200305)
# end_date_period[["3b"]] <- ymd(20210621)
# 
# data[, period := NA_character_]
# data[date_bleeding <= end_date_period[["1"]], period := "1"]
# data[is.na(period) & date_bleeding <= end_date_period[["2"]], period := "2"]
# data[is.na(period) & date_bleeding <= end_date_period[["3a"]], period := "3a"]
# data[is.na(period) & date_bleeding <= end_date_period[["3b"]], period := "3b"]
# data[is.na(period),  period := "3c"]  


# processing <- GenerateTDDataset(datasets = list(processing,obsperiods),
#                                 UoO_vars = c("person_id","person_id"),
#                                 start_d_vars = c("start_date","entry_spell_category"),
#                                 end_d_vars = c("end_date","exit_spell_category"),
#                                 keep_auxiliary_variables = F,
#                                 TD_variables = list(list("treatment"),list("in_study")),
#                                 keep_periods_observed_by = "both"
# )


# 
# ################################
# # clean
# 
# tokeep <- c("person_id","date","event")
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
# nameoutput <- "D3_study_population"
# nameoutputext <- paste0(nameoutput,".rds")
# assign(nameoutput, outputfile)
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
