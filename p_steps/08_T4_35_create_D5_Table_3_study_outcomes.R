########################################################%##
#                                                          #
####  COMPUTE D5_study_outcomes  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 0.1 13 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D5_Table_3_study_outcomes"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# import input datasets

processing <- readRDS(file.path(thisdirinput, "D4_Cube_study_outcomes.rds"))

# outcomes available

outcome_vars <- grep("^outcome", names(processing), value = TRUE)

# select

processing <- processing[Gender_LevelOrder== 99 & Age_LevelOrder == 99 &(TypeBleeding_LabelValue== "narrow" | TypeBleeding_LevelOrder == 99 ),]

# generate percentages

processing[, paste0("p_", outcome_vars) := lapply(.SD, function(x) round(100 * x / N, 1)), .SDcols = outcome_vars]

# bleeding

processing[, type_bleeding := fifelse(TypeBleeding_LabelValue == "narrow",TypeBleeding_LabelValue,"broad")]

################################
# clean

toremove <- c("Age_LevelOrder","Age_LabelValue", "Gender_LevelOrder","Gender_LabelValue","Period_LevelOrder","TypeBleeding_LevelOrder","TypeBleeding_LabelValue")


processing <- processing[, !toremove, with = FALSE]
# 
# setorderv(
#   processing,
#   c("Gender_LevelOrder", "Age_LevelOrder","Time_LevelOrder","Time_LabelValue")
# )
# 
# 
#########################################
# save

outputfile <- processing

nameoutput <- "D5_Table_3_study_outcomes"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

