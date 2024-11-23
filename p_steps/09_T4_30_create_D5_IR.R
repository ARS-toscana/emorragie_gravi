########################################################%##
#                                                          #
####  COMPUTE D5_IR  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 0.1 22 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D5_IR"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# import input datasets

processing <- readRDS(file.path(thisdirinput, "D4_Cube_persontime_bleeding.rds"))

for (event in c("bleeding_narrow","bleeding_broad")) {
  name_cols <- paste0(c("IR", "lb", "ub"),"_",event)
  processing[, (name_cols) := exactPoiCI(processing, paste0(event,"_b"), paste0("PY_",event), conversion_factor = 1, per = 100)]  
}



################################
# clean

tokeep <- c("Gender_LabelValue", "Gender_LevelOrder", "Age_LabelValue", "Age_LevelOrder", "Time_LabelValue", "Time_LevelOrder", "bleeding_narrow_b", "IR_bleeding_narrow", "lb_bleeding_narrow", "ub_bleeding_narrow",   "bleeding_broad_b", "IR_bleeding_broad", "lb_bleeding_broad", "ub_bleeding_broad")


processing <- processing[, ..tokeep]

setorderv(
  processing,
  c("Gender_LevelOrder", "Age_LevelOrder","Time_LevelOrder","Time_LabelValue")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D5_IR"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

