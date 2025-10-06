########################################################%##
#                                                          #
####  CREATE D4_analytical_dataset  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 0.91 17 Dec 2024

# added distance between date bleeding and date of death

# v 0.9 16 Dec 2024

# add all outcomes

# v 0.2 10 Dec 2024

# restrict to study period

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
  thisdiroutput <- direxp
}

# import input datasets

processing <- readRDS(file.path(thisdirinput, "D3_study_population.rds"))
D3_study_population_with_prob_exp <- readRDS(file.path(thisdirinput, "D3_study_population_with_prob_exp.rds"))
outcomes <- readRDS(file.path(thisdirinput, "D3_study_outcomes.rds"))

tokeep <- c("person_id", "date_bleeding", "prob_exp")
D3_study_population_with_prob_exp <- D3_study_population_with_prob_exp[, ..tokeep]

# add info on individual probability of exposure
processing <- D3_study_population_with_prob_exp[processing, on = .(person_id, date_bleeding)]


# set names

processing <- processing[event == "bleeding_narrow", type_bleeding := "narrow"]
processing <- processing[event == "bleeding_possible", type_bleeding := "possible"]


# period

processing[, period := NA_character_]
processing[is.na(period) & date_bleeding <= end_date_period[["1a"]], period := "1a"]
processing[is.na(period) & date_bleeding <= end_date_period[["1b"]], period := "1b"]
processing[is.na(period) & date_bleeding <= end_date_period[["1c"]], period := "1c"]
processing[is.na(period) & date_bleeding <= end_date_period[["2"]], period := "2"]
processing[is.na(period) & date_bleeding <= end_date_period[["3"]],  period := "3"]
processing <- processing[!is.na(period),]

# outcome death

processing[,outcome_DEATH := fifelse(!is.na(death_date) & death_date >= date_bleeding & death_date <= end_followup_d,1,0)]

# distance

processing <- processing[ outcome_DEATH == 1, days_DEATH := as.integer( death_date - date_bleeding)]


# other outcomes

temp <- merge(processing[,.(person_id,date_bleeding)],outcomes,by = "person_id", all = F)
temp <- temp[date >= date_bleeding & date <= date_bleeding + 30,]
temp <- unique(temp[,.(person_id,date_bleeding,event)])
temp <- dcast(temp, person_id + date_bleeding ~ event, 
                   fun.aggregate = length, 
                   value.var = "event")

listconceptsets <- c("AMI", "IS", "VTE", "TIA", "PE", "DIC")

for (concept_id in listconceptsets) {
  if (concept_id %in% names(temp)) {
    setnames(temp,concept_id,"vartemp")
    temp <- temp[is.na(vartemp), vartemp := 0]
    setnames(temp,"vartemp",paste0("outcome_",concept_id))
    
  }else{
    temp[,(paste0("outcome_",concept_id)) := 0]
  }
}

processing <- merge(processing,temp, by = c("person_id","date_bleeding"), all.x = T)

for (concept_id in listconceptsets) {
    setnames(processing,paste0("outcome_",concept_id),"vartemp")
    processing <- processing[is.na(vartemp), vartemp := 0]
    setnames(processing,"vartemp",paste0("outcome_",concept_id))
}

# thombotic

processing[, outcome_THROM := pmax(outcome_AMI, outcome_IS, outcome_VTE, outcome_TIA, outcome_PE)]

# composite

processing[, outcome_comp := pmax(outcome_THROM, outcome_DEATH)]

# episode_id

setorderv(
  processing,
  c("person_id", "date_bleeding")
)

processing[,episode_id := seq_len(.N)]


################################
# clean

tokeep <- c("episode_id", "person_id", "gender", "ageband", "age", "date_bleeding", "type_bleeding", "period", "number_previous_bleedings","outcome_AMI", "outcome_IS", "outcome_VTE", "outcome_TIA", "outcome_PE", "outcome_DIC", "outcome_THROM","outcome_DEATH", "days_DEATH" ,"outcome_comp", paste0("covariate_",as.character(1:26)))

tokeep <- c("episode_id", "person_id", "gender", "ageband", "age", "date_bleeding", "type_bleeding", "prob_exp", "period", "number_previous_bleedings","outcome_AMI", "outcome_IS", "outcome_VTE", "outcome_TIA", "outcome_PE", "outcome_DIC", "outcome_THROM","outcome_DEATH", "days_DEATH","outcome_comp")


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
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

#########################################

# describe D4_analytical_dataset

descriptive_table <- list(
  
  "Distribution of type_bleeding" = table(D4_analytical_dataset$type_bleeding),
  "Distribution of exposure to AA" = table(D4_analytical_dataset$prob_exp),
  "Distribution of exposure to AA by type_bleeding" = table(D4_analytical_dataset$type_bleeding, D4_analytical_dataset$prob_exp)
  
)

saveRDS(descriptive_table, file = file.path(thisdiroutput,"descriptive_D4_analytical_dataset.rds"))
