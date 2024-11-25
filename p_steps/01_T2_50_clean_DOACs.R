########################################################%##
#                                                          #
####  COMPUTE D3_dispensings_DOACs
  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_dispensings_DOACs"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import input datasets

load(file.path(thisdirinput, "Apixaban.RData"))
load(file.path(thisdirinput, "Rivaroxaban.RData"))
load(file.path(thisdirinput, "OtherDOACs.RData"))

# process        

processing <- Apixaban[, label := 2]
processing <- rbind(processing, Rivaroxaban, fill = T)
processing <- processing[is.na(label), label := 1]
processing <- processing[is.na(pezzi), pezzi := as.integer(1)]

processing <- processing[,.(ID, aic,DATE,pezzi,label)]

processing <- processing[,AIC := as.character(aic)]
processing <- processing[,len := nchar(AIC)]

processing <- processing[len == 8 ,AIC := paste0("0",AIC)]


processing <- merge(processing,duration_MoI, by= "AIC", all.x = T)

processing[, duration := pezzi * number_of_pills * conversion]

# add other DOACs

processing <- rbind(processing, OtherDOACs, fill = T)
processing <- processing[is.na(label), label := 3]

processing[, COD_ATC5 := ""]

setnames(processing,c("ID","DATE"),c("person_id","date"))

################################
# clean

tokeep <- c("person_id",
            "date",
            "AIC",
            "COD_ATC5",
            "label",
            "duration")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id","date")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_dispensings_DOACs"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

#
# # compare output with ground truth with integer dates
# # Define the reference date
# ref_date <- as.Date("2015-01-01")
#
# # Calculate the difference in days and replace dates
# vectordates <- c("start_date","end_date")
# for (variable in vectordates) {
#   processing[, (variable) := as.integer(get(variable) - ref_date)]
# }
# #
# # fwrite(processing,file = "C:/temp/temp.csv")
