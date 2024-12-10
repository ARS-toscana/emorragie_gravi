# author: Rosa Gini

# v 1.2 10 Dec 2024

# update definition of narrow based on cpmponent strategy

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_bleeding_events"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import

load(file.path(thisdirinput, "bleeding_narrow.RData"))

# processing 

processing <- bleeding_narrow

processing <- processing[(meaning == "hospital_main_diagnosis") | (meaning == "hospital_sec_diagnosis" & pres == 1) | (meaning == "emergency_room_diagnosis" & esito %in% list_outcomesER_severe ), event := "bleeding_narrow"]
processing <- processing[is.na(event), event := "bleeding_possible"]

setnames(processing,c("ID","DATE"),c("person_id","date"))

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

nameoutput <- "D3_bleeding_events"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
