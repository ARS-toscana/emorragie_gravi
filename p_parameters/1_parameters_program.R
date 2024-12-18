# #set directory with input data

dirinput <- paste0(thisdir,"/i_input/")
dirinput <- "P:/osservatori/epidemiologia/FARMACOEPI/OTGC/emorraggie_gravi/CDM_instances/241125/"
dirtest <- file.path(thisdir,"i_test")
dircodelist <- file.path(thisdir,"p_parameters","archive_parameters")

batch_size_countprevalence <- 20000000
# 
# set_and_create_dir <- function(x) {
#   x <- paste0(thisdir, x)
#   dir.create(file.path(x), showWarnings = F)
#   return(x)
# }

# # set other directories
# diroutput <- set_and_create_dir("/g_output/")
# dirtemp <- set_and_create_dir("/g_intermediate/")
# dirconceptsets <- set_and_create_dir("/g_intermediate/conceptset_datasets/")
# # diritemsets <- set_and_create_dir("/g_intermediate/itemset_datasets/")
# # dirpromptsets <- set_and_create_dir("/g_intermediate/promptset_datasets/")
# direxp <- set_and_create_dir("/g_export/")
# dirD6 <- set_and_create_dir("/g_export/Formatted tables/")
# dirfig <- set_and_create_dir("/g_export/Figures/")
# dirmacro <- set_and_create_dir("/p_macro/")
# dirpargen <- set_and_create_dir("/g_parameters/")
# # direvents <- set_and_create_dir("/g_intermediate/events/")
# # dircomponents <- set_and_create_dir("/g_intermediate/components/")
# # dirTD <- set_and_create_dir("/g_intermediate/TD/")

set_and_create_dir <- function(x) {
  # x <- paste0(thisdir, x)
  dir.create(file.path(x), showWarnings = F)
  return(x)
}

diroutput <- set_and_create_dir(paste0(dirinput,"/g_output/"))
dirtemp <- set_and_create_dir(paste0(dirinput,"/g_intermediate/"))
dirconceptsets <- set_and_create_dir(paste0(dirinput,"/g_intermediate/conceptset_datasets/"))
# diritemsets <- set_and_create_dir("/g_intermediate/itemset_datasets/")
# dirpromptsets <- set_and_create_dir("/g_intermediate/promptset_datasets/")
direxp <- set_and_create_dir(paste0(dirinput,"/g_export/"))
dirD6 <- set_and_create_dir(paste0(dirinput,"/g_export/Formatted tables/"))
dirfig <- set_and_create_dir("/g_export/Figures/")
dirmacro <- paste0(thisdir,"/p_macro/")
dirpargen <- set_and_create_dir(paste0(dirinput,"/g_parameters/"))
# direvents <- set_and_create_dir("/g_intermediate/events/")
# dircomponents <- set_and_create_dir("/g_intermediate/components/")
# dirTD <- set_and_create_dir("/g_intermediate/TD/")


rm(set_and_create_dir)

# load packages
read_library <- function(...) {
  x <- c(...)
  invisible(lapply(x, library, character.only = TRUE))
}

list.of.packages <- c("MASS", "haven", "tidyverse", "lubridate", "AdhereR", "stringr", "purrr", "readr", "dplyr",
                      "survival", "rmarkdown", "ggplot2","scales", "data.table", "qpdf", "parallel", "readxl", "gtsummary",
                      "labelled", "huxtable", "metafor", "markdown", "R.utils", "RcppAlgos", "qs","zoo","knitr", "kableExtra", 
                      "officer", "writexl","pdftools","png","grid","gridExtra","cowplot", "bit64", "splines", "pbs", "lme4", 
                      "Matrix", "clubSandwich", "lmtest", "sandwich", "openxlsx", "broom", "broom.mixed", "sjPlot", "performance")


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))

rm(read_library, new.packages, list.of.packages)

# load macros
source(paste0(dirmacro, "CreateConceptSetDatasets_v20.R"))
source(paste0(dirmacro, "CreateItemsetDatasets_v03.R"))
source(paste0(dirmacro, "MergeFilterAndCollapse_v5.R"))
source(paste0(dirmacro, "CreateSpells_v15.R"))
source(paste0(dirmacro, "CreateFlowChart.R"))
source(paste0(dirmacro, "CountPersonTimeV14.0.R"))
source(paste0(dirmacro, "df_to_list_of_list.R"))
source(paste0(dirmacro, "list_of_list_to_df.R"))
source(paste0(dirmacro, "dsr.R"))
source(paste0(dirmacro, "launch_step.R"))
source(paste0(dirmacro, "CompareListsOfCodes.R"))
source(paste0(dirmacro, "CountPrevalence.R"))
source(paste0(dirmacro, "Cube.R"))
source(paste0(dirmacro, "test utility functions.R"))
source(paste0(dirmacro, "GenerateTDDataset_v0.921.R"))
source(paste0(dirmacro, "ApplyComponentStrategy_v15.R"))
source(paste0(dirmacro, "CreateFigureComponentStrategy_v4.R"))

#other parameters

# 
# #---------------------------------------
# # understand which datasource the script is querying
# 
# CDM_SOURCE<- fread(paste0(dirinput,"CDM_SOURCE.csv"))
# 
# thisdatasource <- as.character(CDM_SOURCE[1,3])
# 


### TESTING parameters

# dataset names
list_of_all_datasets <- c()

# folder for generation of each dataset
folders_to_be_tested <- c("")

# ###################################################################
# # CREATE EMPTY FILES
# ###################################################################
# 
# files <- sub('\\.csv$', '', list.files(dirinput))
# 
# if (!any(str_detect(files,"^SURVEY_ID"))) {
#   print("Creating empty SURVEY_ID since none were found")
#   fwrite(data.table(person_id = character(0), survey_id = character(0), survey_date = character(0),
#                     survey_meaning = character(0)),
#          paste0(dirinput, "SURVEY_ID", ".csv"))
# }
# 
# if (!any(str_detect(files,"^SURVEY_OBSERVATIONS"))) {
#   print("Creating empty SURVEY_OBSERVATIONS since none were found")
#   fwrite(data.table(person_id = character(0), so_date = character(0), so_source_table = character(0),
#                     so_source_column = character(0), so_source_value = character(0), so_unit = character(0),
#                     survey_id = character(0)),
#          paste0(dirinput, "SURVEY_OBSERVATIONS", ".csv"))
# }
# 
# if (!any(str_detect(files,"^MEDICINES"))) {
#   print("Creating empty MEDICINES since none were found")
#   fwrite(data.table(person_id = character(0), medicinal_product_id = integer(0),
#                     medicinal_product_atc_code = character(0), date_dispensing = integer(0),
#                     date_prescription = logical(0), disp_number_medicinal_product = numeric(0),
#                     presc_quantity_per_day = logical(0), presc_quantity_unit = logical(0),
#                     presc_duration_days = logical(0), product_lot_number = logical(0),
#                     indication_code = logical(0), indication_code_vocabulary = logical(0),
#                     meaning_of_drug_record = character(0), origin_of_drug_record = character(0),
#                     prescriber_speciality = logical(0), prescriber_speciality_vocabulary = logical(0),
#                     visit_occurrence_id = character(0)),
#          paste0(dirinput, "MEDICINES_FED", ".csv"))
# }
# 
# rm(files)
# 
# #############################################
# #SAVE METADATA TO direxp
# #############################################
# 
# file.copy(paste0(dirinput,'/METADATA.csv'), direxp, overwrite = T)
# file.copy(paste0(dirinput,'/CDM_SOURCE.csv'), direxp, overwrite = T)
# file.copy(paste0(dirinput,'/INSTANCE.csv'), direxp, overwrite = T)
# 
# #############################################
# #SAVE to_run.R TO direxp
# #############################################
# 
# file.copy(paste0(thisdir,'/to_run.R'), direxp, overwrite = T)
# 


#############################################
#FUNCTION TO COMPUTE AGE
#############################################

# TODO check agebands
# Agebands = c(-1, 1, 4, 11, 17, 29, 39, 49, 59, 69, 79, Inf)
# Agebands_labels = c("0-1","2-4","5-11","12-17","18-29", "30-39", "40-49","50-59","60-69", "70-79","80+")
# names(Agebands) <- Agebands_labels
# 
# Agebands_cube = c(0, 2, 5, 12, 18, 30, 40, 50, 60, 70, 80, Inf)


# pop.eustat <- fread(paste0(thisdir,"/p_parameters/archive_parameters/ESP_ageband_pop.csv"))
# pop.eustat[, c("start_ageband", "end_ageband") := lapply(tstrsplit(Ageband, "-"), as.integer)][, Ageband := NULL]
# pop.eustat[, n_agebands := end_ageband - start_ageband + 1]
# pop.eustat <- pop.eustat[rep(1:.N, n_agebands)][, Indx := 1:.N, by = start_ageband]
# pop.eustat[, Indx := Indx - 1][, age := start_ageband + Indx][, c("start_ageband", "end_ageband", "Indx") := NULL]
# pop.eustat[, pop := pop / n_agebands][, n_agebands := NULL]
# pop.eustat[, Ageband := cut(age, Agebands, Agebands_labels)][, age := NULL]
# pop.eustat <- pop.eustat[, .(pop = sum(pop)), by = Ageband][, Ageband := as.character(Ageband)]

age_fast = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}


# Define a function to calculate the date occurring j months after a specified date
get_date_for_month <- function(date_start, j) {
  # Calculate the target date by adding the specified number of months to the birthdate
  target_date <- add_with_rollback(date_start, months(j))
  return(target_date)
}

##################################################
# Function to mask numeric columns with a threshold
mask_variables_per_threshold <- function(dataset, listvar, threshold) {
  # Step 1: Check if the variables in listvar exist in dataset and are numeric
  for (var in listvar) {
    if (!(var %in% names(dataset))) {
      stop(paste("Error: Variable", var, "not found in dataset"))
    }
    if (!is.numeric(dataset[[var]])) {
      if (all(is.na(dataset[[var]]))) {
        # If the variable contains only missing values, allow the function to continue
        warning(paste("Variable", var, "contains only missing values, converting to character and setting check_var to 0"))
        dataset[, (var) := as.character(get(var))]  # Convert to character
        dataset[, (paste0(var, "_check")) := 0]  # Set the check variable to 0
      } else {
        stop(paste("Error: Variable", var, "is not numeric and does not contain only missing values"))
      }
    } else if (all(is.na(dataset[[var]]))) {
      # If numeric with all missing values, convert to character and create check_var
      warning(paste("Numeric variable", var, "contains only missing values, converting to character and setting check_var to 0"))
      dataset[, (var) := as.character(get(var))]  # Convert to character
      dataset[, (paste0(var, "_check")) := 0]  # Set the check variable to 0
    }
  }
  
  # Step 2: For each variable, generate a new check variable and mask the original variable
  for (var in listvar) {
    if (!all(is.na(dataset[[var]]))) {
      # Only apply the check if the variable is not entirely NA
      check_var <- paste0(var, "_check")
      dataset[, (check_var) := ifelse(get(var) > 0 & get(var) < threshold, 1, 0)]
      
      # Convert the variable into string and mask the values
      dataset[, (var) := ifelse(get(check_var) == 1, paste0("< ", threshold), as.character(get(var)))]
    }
  }
  
  # Return the modified dataset
  return(dataset)
}



# Helper function to format numbers with thousand separator
# format_with_comma <- function(x) {
#   format(x, big.mark = ",", scientific = FALSE)
# }

format_with_comma <- function(x) {
  if (!is.numeric(x)) {
    return(x)  # Return the character itself if x is a character
  }
  # Format numeric values with commas
  format(x, big.mark = ",", scientific = FALSE)
}

`%not in%` = Negate(`%in%`)

substrRight <- function(x, n){
  char_x <- nchar(x)
  substr(x, char_x - n + 1, char_x)
}

find_last_monday <- function(tmp_date, monday_week) {
  
  tmp_date <- as.Date(lubridate::ymd(tmp_date))
  
  while (tmp_date %not in% monday_week) {
    tmp_date <- tmp_date - 1
  }
  return(tmp_date)
}

find_first_monday_year <- function(tmp_date, monday_week) {
  
  tmp_date <- as.Date(lubridate::ymd(tmp_date))
  
  while (tmp_date %not in% monday_week) {
    tmp_date <- tmp_date + 1
  }
  return(tmp_date)
}

correct_difftime <- function(t1, t2, t_period = "days") {
  return(difftime(t1, t2, units = t_period) + 1)
}

calc_precise_week <- function(time_diff) {
  # correction in case a person exit the same date it enter
  time_diff <- fifelse(time_diff == 1, time_diff + 1, time_diff)
  weeks_frac <- time_length(time_diff - 1, "week")
  fifelse(weeks_frac%%1==0, weeks_frac, floor(weeks_frac) + 1)
}

exactPoiCI <- function (df, X, PT, conf.level = 0.95, conversion_factor = 365.25, per = 1000) {
  alpha <- 1 - conf.level
  IR <- df[, get(X)]
  upper <- df[, 0.5 * qchisq((1-(alpha/2)), 2*(get(X)+1))]
  lower <- df[, 0.5 * qchisq(alpha/2, 2*get(X))]
  temp_list <- lapply(list(IR, lower, upper), `/`, df[, get(PT)/conversion_factor])
  temp_list <- lapply(temp_list, `*`, per)
  temp_list <- lapply(temp_list, function(x) {fifelse(x == Inf, 0, x)})
  return(lapply(temp_list, round, 1))
}

exactNormCI <- function (df, X, PT, conf.level = 0.95, conversion_factor = 365.25) {
  alpha <- 1 - conf.level
  IR <- df[, get(X)]
  upper <- df[, 0.5 * qnorm((1-(alpha/2)), 2*(get(X)+1))]
  lower <- df[, 0.5 * qnorm(alpha/2, 2*get(X))]
  temp_list <- lapply(list(IR, lower, upper), `/`, df[, get(PT)/conversion_factor])
  temp_list <- lapply(temp_list, `*`, 100000)
  temp_list <- lapply(temp_list, function(x) {fifelse(x == Inf, 0, x)})
  return(lapply(temp_list, round, 1))
}
