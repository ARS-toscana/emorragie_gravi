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

processing <- readRDS(file.path(thisdirinput, "D3_dispensings_DOACs.rds"))

# ...


processing <- processing[label == 1 | label == 2,]

processing <- compute.treatment.episodes(processing,
                                  ID.colname= "person_id",
                                  event.date.colname= "date",
                                  event.duration.colname= "duration",
                                  medication.class.colname= "label",
                                  carryover.within.obs.window = TRUE, # carry-over into the OW
                                  carry.only.for.same.medication = TRUE, # & only for same type
                                  medication.change.means.new.treatment.episode = TRUE, # & type change
                                  maximum.permissible.gap = 30, # & a gap longer than 30 days
                                  maximum.permissible.gap.unit = "days" # , # unit for the above (days)
                                  
                                  # followup.window.start = 0, # 2-years FUW starts at earliest event
                                  # followup.window.start.unit = "days",
                                  # followup.window.duration = 365 * 2,
                                  # followup.window.duration.unit = "days",
#                                  date.format = "%m/%d/%Y"
)

setnames(processing,c("episode.start","episode.end"),c("start_date","end_date"))

processing <- as.data.table(processing)

# ################################
# # clean
# 
# tokeep <- c("person_id","start_date", "end_date", "treatment")
# 
# processing <- processing[, ..tokeep]
# 
# setorderv(
#   processing,
#   c("gender", "ageband","month")
# )
# 
# 
# #########################################
# # save
# 
# outputfile <- processing
# 
# nameoutput <- "D3_episodes_of_treatment"
# nameoutputext <- paste0(nameoutput,".rds")
# assign(nameoutput, outputfile)
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))


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
