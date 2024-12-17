########################################################%##
#                                                          #
####  COMPUTE D4_Cube_study_outcomes  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 0.1 13 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D4_Cube_study_outcomes"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import input datasets

processing <- readRDS(file.path(thisdirinput, "D4_analytical_dataset.rds"))

# create the list of outcomes calculated already

outcome_vars <- grep("^outcome", names(processing), value = TRUE)

# number
           
processing[, N := 1]              

# assign the levels of each dimension
assigned_levels <- vector(mode="list")
assigned_levels[["Age"]] <- c("ageband","ageband_agg")
assigned_levels[["Gender"]] <- c("gender")
assigned_levels[["Period"]] <- c("period")
assigned_levels[["TypeBleeding"]] <- c("type_bleeding")

processing[, ageband_agg := fifelse(
  ageband %in% c("0-17"), "0-17",
  fifelse(ageband %in% c("18-39", "40-59"), "18-59", "60+")
)]

# assign the order of each dimension


assigned_order <- vector(mode="list")

names(processing)
processing <- Cube(input = processing,
               dimensions = c("Age","Gender","Period","TypeBleeding"),
               levels = assigned_levels,
               measures = c("N", outcome_vars),
               computetotal = c("Age","Gender","Period","TypeBleeding"),
               statistics = list(
                 list(c("sum") ,c("N", outcome_vars)),
                 # list(c("median","q1","q3"),c("days_DEATH"))
                 list(c("median"),c("days_DEATH"))
               )
               
)

cols_to_change_name <- names(processing)[grepl("_sum$", names(processing))]
setnames(processing, cols_to_change_name, gsub("_sum$", "", cols_to_change_name))

################################
# clean

# setorderv(
#   processing,
#   c("Gender_LevelOrder", "Age_LevelOrder","Period_LevelOrder","Time_LabelValue")
# )


#########################################
# save

outputfile <- processing

nameoutput <- "D4_Cube_study_outcomes"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

