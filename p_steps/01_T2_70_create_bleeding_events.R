# author: Rosa Gini

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
load(file.path(thisdirinput, "bleeding_broad.RData"))

# processing 

processing <- bleeding_narrow

processing[, event := "bleeding_narrow"]

processing <- rbind(processing, bleeding_broad, fill = T)

processing <- processing[is.na(event), event := "bleeding_broad"]

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
