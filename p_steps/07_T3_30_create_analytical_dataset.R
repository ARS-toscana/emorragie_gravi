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

processing <- readRDS(file.path(thisdirinput, "D3_study_population.rds"))


# set names

processing <- processing[event == "bleeding_narrow", type_bleeding := "narrow"]
processing <- processing[event == "bleeding_possible", type_bleeding := "possible"]


# period

end_date_period <- list()
end_date_period[["1a"]] <- ymd(20200229)
end_date_period[["1b"]] <- ymd(20210531)
end_date_period[["1c"]] <- ymd(20210831)
end_date_period[["2"]] <- ymd(20230731)

processing[, period := NA_character_]
processing[is.na(period) & date_bleeding <= end_date_period[["1a"]], period := "1a"]
processing[is.na(period) & date_bleeding <= end_date_period[["1b"]], period := "1b"]
processing[is.na(period) & date_bleeding <= end_date_period[["1c"]], period := "1c"]
processing[is.na(period) & date_bleeding <= end_date_period[["2"]], period := "2"]
processing[is.na(period),  period := "3"]

# age

processing[, age := age_fast(birth_date,date_bleeding)]

# outcome

processing[,outcome_DEATH := fifelse(!is.na(death_date) & death_date >= date_bleeding & death_date <= end_followup_d,1,0)]

################################
# clean

tokeep <- c("person_id","sex","age","date_bleeding","type_bleeding","period","outcome_DEATH" )

processing <- processing[, ..tokeep]

setorderv(
  processing,
  c("person_id", "date_bleeding")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D4_analytical_dataset"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

