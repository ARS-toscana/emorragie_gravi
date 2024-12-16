########################################################%##
#                                                          #
####  COMPUTE D3_study_outcomes
####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.0 16 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_study_outcomes"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import

persons_with_MoI <- readRDS(file.path(thisdirinput, "D4_persons_with_MoI.rds"))

listconceptsets <- c("AMI", "IS", "VTE", "TIA", "PE", "DIC")

processing <- data.table()

for (concept_id in listconceptsets) {
  load(file.path(thisdirinput, paste0(concept_id,".RData")))
  setnames(get(concept_id),c("ID","DATE"),c("person_id","date"))
  temp <- merge(get(concept_id),persons_with_MoI, by = "person_id", all = F)
  temp <- temp[date >= study_start_date & date <= study_end_date,]
  temp[, event := concept_id]
  processing <- rbind(processing,temp, fill = T)
}

processing <- processing[meaning == "emergency_room_diagnosis" | meaning == "hospital_main_diagnosis" | (meaning == "hospital_sec_diagnosis"  & pres == 0),]

processing <- unique(processing[, .(person_id,date,event)])


# processing

################################
# clean

tokeep <- c("person_id","date","event")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id","date")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_study_outcomes"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
