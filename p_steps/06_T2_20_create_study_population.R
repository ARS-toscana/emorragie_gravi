########################################################%##
#                                                          #
####  CREATE D3_study_population  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.3 16 Dec 2024

# aggregated 2+ number of previous bleedings, restricted to previous 3 years


# v 1.2 15 Dec 2024

# add number of previous bleedings

# v 1.1 11 Dec 2024

# limit study population to study start and end date

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
persons <- readRDS(file.path(thisdirinput, "D3_PERSONS.rds"))

# process

# limit events to those happening when the person is in the source population

processing <- merge(cohort,events, by = "person_id", all = F)
processing <- processing[date >= entry_cohort & date <= exit_cohort,]

# rename date

setnames(processing,"date","date_bleeding")

# remove cases that happen outside of the periods when the person is in the source population

processing <- processing[ date_bleeding >= study_start_date & date_bleeding <= study_end_date,]

# count previous bleedings

temp <- merge(processing[,.(person_id,date_bleeding)],events, by = "person_id", all = F, allow.cartesian = T)

temp <- temp[date < date_bleeding & date >= date_bleeding - 3 * 365,]

temp[, dist := as.integer(date_bleeding - date)]

temp <- temp[,.(number_previous_bleedings = .N, days_since_most_recent_bleeding = min(dist)), by = .(person_id,date_bleeding)]

temp <- temp[ number_previous_bleedings > 2,number_previous_bleedings := 2] 

processing <- merge(processing,temp, by = c("person_id","date_bleeding"), all.x = T)

processing <- processing[is.na(number_previous_bleedings), number_previous_bleedings:= 0 ]

# period

processing[, period := NA_character_]
processing[is.na(period) & date_bleeding <= end_date_period[["1a"]], period := "1a"]
processing[is.na(period) & date_bleeding <= end_date_period[["1b"]], period := "1b"]
processing[is.na(period) & date_bleeding <= end_date_period[["1c"]], period := "1c"]
processing[is.na(period) & date_bleeding <= end_date_period[["2"]], period := "2"]
processing[is.na(period) & date_bleeding <= end_date_period[["3"]],  period := "3"]
processing <- processing[!is.na(period),]


# clean


# remove cases that happen outside of the observation period

processing <- merge(processing,obsperiods[,.(person_id,entry_spell_category, exit_spell_category)], by = "person_id", all = F)
processing <- processing[date_bleeding >= entry_spell_category & date_bleeding <= exit_spell_category,]

# add death

processing <- merge(processing,persons[,.(person_id,death_date)], by = "person_id", all = F)

# age

processing[, age := age_fast(birth_date,date_bleeding)]

#ageband

agebands <- c(0, 18, 40, 60, 80, 150)

processing[, ageband := cut(age, 
                            breaks = agebands, 
                            include.lowest = TRUE, 
                            right = FALSE, # Left-closed intervals
                            labels = c("0-17", "18-39", "40-59", "60-79", "80+"))]


# time of end of follow up (never used)

processing[, end_followup_d := pmin(exit_spell_category, date_bleeding + 30)]



################################
# clean

tokeep <- c("person_id","birth_date","gender","age","ageband","death_date","period","date_bleeding","event","end_followup_d","number_previous_bleedings","days_since_most_recent_bleeding")

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
