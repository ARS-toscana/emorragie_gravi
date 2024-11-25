# author: Rosa Gini

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_PERSONS"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

processing <- fread(file.path(thisdirinput,"ANAGRAFE_ASSISTITI.csv"))


setorderv(processing, c("id", "data_inizioass"))

processing <- processing[, is_the_most_recent_info := as.integer(seq(.N) == .N), by = id]

processing <- processing[is_the_most_recent_info == 1, ]

setnames(processing,old = c("id"),new = c("person_id"))

processing <- processing[sesso == 1, gender := "M"]
processing <- processing[sesso == 2, gender := "F"]

processing[, birth_date := as.Date(datanas)]
processing[, death_date := as.Date(datadec)]

################################
# clean

tokeep <- c("person_id", "gender", "birth_date","death_date")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_PERSONS"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
