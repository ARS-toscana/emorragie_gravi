########################################################%##
#                                                          #
####  COMPUTE D3_episodes_of_treatement
  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_episodes_of_treatment"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import input datasets

dispensings <- readRDS(file.path(thisdirinput, "D3_dispensings_DOACs.rds"))
obsperiods <- readRDS(file.path(thisdirinput, "D3_clean_spells.rds"))

# create episodes of continuous treatment with each MoI

for (j in c(1,2)) {
  temp <- dispensings[label == j,]
  temp <- temp[is.na (duration) , duration := 30]
  temp <- compute.treatment.episodes(temp,
                                           ID.colname= "person_id",
                                           event.date.colname= "date",
                                           event.duration.colname= "duration",
                                           medication.class.colname= "label",
                                           carryover.within.obs.window = TRUE, # carry-over into the OW
                                           carry.only.for.same.medication = TRUE, # & only for same type
                                           medication.change.means.new.treatment.episode = TRUE, # & type change
                                           maximum.permissible.gap = 30, # & a gap longer than 30 days
                                           maximum.permissible.gap.unit = "days"
  )
  
  setnames(temp,c("episode.start","episode.end"),c("start_date","end_date"))
  
  temp <- as.data.table(temp)
  temp <- temp[,.(person_id,start_date,end_date)]
  # add 30 days at the end
  temp[, end_date := end_date + 29]
  temp[,(paste0("treatment_",j)) := 1]
  nameepisodes <- paste0("episodes_",j)
  assign(nameepisodes,temp)
}

# create overlaps across periods of treatment

processing <- GenerateTDDataset(datasets = list(episodes_1,episodes_2),
                                UoO_vars = c("person_id","person_id"),
                                start_d_vars = c("start_date","start_date"),
                                end_d_vars = c("end_date","end_date"),
                                keep_auxiliary_variables = F,
                                TD_variables = list(list("treatment_1"),list("treatment_2")),
                                keep_periods_observed_by = "either"
)

# classify periods of treatemnt with one, the other, or both MoIs

processing <- processing[treatment_1 == 1, treatment := 1]
processing <- processing[treatment_2 == 1, treatment := 2]
processing <- processing[treatment_1 == 1 & treatment_2 == 1, treatment := 3]
processing <- processing[,.(person_id,start_date,end_date,treatment)]

# stop period when a different DOAC is dispensed

different_DOAC <- dispensings[label == 3,.(person_id,date)]
to_remove <- merge(processing,different_DOAC,allow.cartesian = T, all.x = T, by = "person_id")
to_remove <- to_remove[!is.na(date) & date >= start_date & date <= end_date,]
to_remove <- to_remove[,.(mindate = min(date)),by = .(person_id, start_date,end_date)]
processing <- merge(processing,to_remove, all.x = T, by = c("person_id","start_date", "end_date"))
processing[, end_date := pmin(end_date,mindate -1 , na.rm = T)]
processing <- processing[,.(person_id,start_date,end_date,treatment)]

# remove periods that do not overlap the individual-level study period

obsperiods <- obsperiods[is_the_study_spell == 1,]
obsperiods <- obsperiods[,.(person_id,entry_spell_category,exit_spell_category)]
obsperiods[,in_study := 1]
# 
# 
# processing <- merge(processing,obsperiods,all.x = T, by = c("person_id"))
# 
# processing <- processing[ end_date >= study_start_date & !is.na(exit_spell_category) & end_date >= exit_spell_category &  !is.na(entry_spell_category) & start_date >= exit_spell_category,]

processing <- GenerateTDDataset(datasets = list(processing,obsperiods),
                                UoO_vars = c("person_id","person_id"),
                                start_d_vars = c("start_date","entry_spell_category"),
                                end_d_vars = c("end_date","exit_spell_category"),
                                keep_auxiliary_variables = F,
                                TD_variables = list(list("treatment"),list("in_study")),
                                keep_periods_observed_by = "both"
)


################################
# clean

tokeep <- c("person_id","start_date", "end_date", "treatment")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id","start_date")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_episodes_of_treatment"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))


# compare output with ground truth with integer dates
# Define the reference date
ref_date <- as.Date("2015-01-01")

# Calculate the difference in days and replace dates
vectordates <- c("start_date","end_date")
for (variable in vectordates) {
  processing[, (variable) := as.integer(get(variable) - ref_date)]
}
#
# fwrite(processing,file = "C:/temp/temp.csv")
