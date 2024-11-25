########################################################%##
#                                                          #
####  COMPUTE D4_Cube_persontime_bleeding  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 0.1 22 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D4_Cube_persontime_bleeding"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import input datasets

processing <- readRDS(file.path(thisdirinput, "D4_persontime_bleeding.rds"))

# assign the levels of each dimension
assigned_levels <- vector(mode="list")
assigned_levels[["Age"]] <- c("ageband","ageband_agg")
assigned_levels[["Gender"]] <- c("gender")
assigned_levels[["Time"]] <- c("month","quarter","year")

processing[, ageband_agg := fifelse(
  ageband %in% c("0-17"), "0-17",
  fifelse(ageband %in% c("18-39", "40-59"), "18-59", "60+")
)]

# # assign the order of each dimension
# 
# assigned_order_vector <- vector(mode="list")
# assigned_order_vector[["Age"]][["ageband"]] <- Agebands_labels
# assigned_order_vector[["Age"]][["ageband_agg"]] <- Agebands_large_labels

# generate missing level variables
processing[, year := substr(month, 1, 4)]
processing[, quarter :=  paste0(year, "-Q", ceiling(as.integer(substr(month, 6, 7)) / 3))]

assigned_order <- vector(mode="list")

names(processing)
processing <- Cube(input = processing,
               dimensions = c("Age","Gender","Time"),
               levels = assigned_levels,
               measures = c("Persontime", "PY", "Persontime_bleeding_narrow", "PY_bleeding_narrow", "bleeding_narrow_b", "Persontime_bleeding_broad", "PY_bleeding_broad", "bleeding_broad_b"),
               computetotal = c("Age","Gender") #,
               # rule_from_numeric_to_categorical = assigned_rule,
               # summary_threshold = 100,
               # order = assigned_order_vector
)

cols_to_change_name <- names(processing)[grepl("_sum$", names(processing))]
setnames(processing, cols_to_change_name, gsub("_sum$", "", cols_to_change_name))

################################
# clean

tokeep <- c("Gender_LabelValue", "Gender_LevelOrder", "Age_LabelValue", "Age_LevelOrder", "Time_LabelValue", "Time_LevelOrder", "Persontime","PY", "Persontime_bleeding_narrow","PY_bleeding_narrow", "bleeding_narrow_b", "Persontime_bleeding_broad","PY_bleeding_broad", "bleeding_broad_b")


processing <- processing[, ..tokeep]

setorderv(
  processing,
  c("Gender_LevelOrder", "Age_LevelOrder","Time_LevelOrder","Time_LabelValue")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D4_Cube_persontime_bleeding"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

