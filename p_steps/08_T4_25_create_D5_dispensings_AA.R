########################################################%##
#                                                          #
####  CREATE D5_dispensings_AA  ####
#                                                          #
########################################################%##


# author: Rosa Gini


# v 1.0 12 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D5_dispensings_AA"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- direxp
}

# import

processing <- fread(file.path(thisdirinput, "fes.csv"))
processing <- processing[atc == "V03AB38",]

processing <- processing[ data >= study_start_date & data <= study_end_date,]


processing[, Nvialsday := pezzi * 4]
processing[, Ndispday := 1]

processing <- processing[consegna != 1, Nvialsday := - Nvialsday]
processing <- processing[consegna != 1, Ndispday := - Ndispday]

processing[, year := year(data)]
processing[, month := month(data)]

setorderv(
  processing,
  c("year","month")
)

fwrite(processing,"C:/temp/temp.csv")

processing <- processing[,.(Nvials = sum(Nvialsday), Ndisp = sum(Ndispday)), by = .(year,month)]

################################
# clean

tokeep <- c("year","month","Nvials","Ndisp")

processing <- processing[, ..tokeep]
setorderv(
  processing,
   c("year","month")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D5_dispensings_AA"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))



