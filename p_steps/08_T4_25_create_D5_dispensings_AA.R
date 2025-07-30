########################################################%##
#                                                          #
####  CREATE D5_dispensings_AA  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.1 30 JUl 2025

# input taken from D3_dispensings_AA

# v 1.0 12 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D5_dispensings_AA"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# import

processing <- readRDS(file.path(thisdirinput, "D3_dispensings_AA.rds"))

setorderv(
  processing,
  c("year","month")
)

# fwrite(processing,"C:/temp/temp.csv")

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



