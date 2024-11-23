rm(list=ls(all.names=TRUE))

#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)


# create the dataset 

namedataset <- "D4_persontime_bleeding"

dates <- seq.Date(from = as.Date("2015-01-01"), to = as.Date("2024-08-01"), by = "month")
month_list <- format(dates, "%Y-%m")

gender_list <- c("F","M")

ageband_list <- c("0-17", "18-39", "40-59", "60-79", "80+")

data <- CJ(
  month = month_list,
  gender = gender_list,
  ageband = ageband_list
)

ageband_values <- c("0-17" = 100, 
                    "18-39" = 200, 
                    "40-59" = 500, 
                    "60-79" = 1000, 
                    "80+" = 2000)

data[, PY := ageband_values[ageband] + rnorm(.N, mean = 0, sd = 5)]
data[, Persontime :=  round(PY * 365.25)]
data[, PY := Persontime/365.25]

# events
# 
# set.seed(1346)
# data[, bleeding_narrow_b:= sample(0:3, .N, replace = TRUE) ]
# library(data.table)

# Create the base data.table with all combinations
base <- CJ(
  month = month_list,
  gender = gender_list,
  ageband = ageband_list
)

# ageband_weights <- list(
#   "0-17"  = c(0.4, 0.3, 0.2, 0.1),  # Less likely to get 3
#   "18-39" = c(0.3, 0.3, 0.2, 0.2),
#   "40-59" = c(0.2, 0.2, 0.3, 0.3),
#   "60-79" = c(0.1, 0.2, 0.3, 0.4),
#   "80+"   = c(0.05, 0.15, 0.3, 0.5)  # Most likely to get 3
# )
# set.seed(12345) 
# data[, bleeding_narrow_b := {
#   probs <- ageband_weights[[ageband]]
#   sample(0:3, .N, replace = TRUE, prob = probs)
# }, by = ageband]


ageband_IR <- c(
  "0-17"  = 0.001,  
  "18-39" = 0.005,
  "40-59" = 0.02,
  "60-79" = 0.05,
  "80+"   = 0.08
)

set.seed(12345) 
data[, bleeding_narrow_b := round(ageband_IR[ageband] * PY * abs(rnorm(.N, mean = 1, sd = 1)))]
data[, Persontime_bleeding_narrow := Persontime - bleeding_narrow_b * 15]
data[, PY_bleeding_narrow := Persontime_bleeding_narrow/365.25]

set.seed(32346) 
data[, bleeding_broad_b := round(bleeding_narrow_b * (1 + abs(rnorm(.N, mean = 0.2, sd = 1))))]
data[, Persontime_bleeding_broad := Persontime - bleeding_broad_b * 15]
data[, PY_bleeding_broad := Persontime_bleeding_broad/365.25]


tokeep <- c("gender", "ageband", "month",  "Persontime","PY", "Persontime_bleeding_narrow","PY_bleeding_narrow", "bleeding_narrow_b", "Persontime_bleeding_broad","PY_bleeding_broad", "bleeding_broad_b")

data <- data[, ..tokeep]

setorderv(
  data,
  c("gender", "ageband","month")
)


saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))



# list of datasets

# listdatasets <- c("D3_bleeding_events","D3_source_population")
# 
# # dates variables 
# 
# listdates <- list()
# listdates[["D3_bleeding_events"]] <- "date"
# listdates[["D3_source_population"]] <- c("birthdate", "entry_cohort","exit_cohort")
# 
# # date baseline
# 
# baseline <- vector(mode="list")
# for (namedataset in listdatasets) {
#   for (datevar in listdates[[namedataset]]) {
#     baseline[[namedataset]][[datevar]] <- as.Date(lubridate::ymd(20150101))
#   }
# }
# 
# 
# # load datasets
# 
# 
# for (namedataset in listdatasets){
#   # data <- fread(paste0(thisdir, "/", namedataset, ".csv") )
#   # data[, (listdates[[namedataset]]) := lapply(.SD, lubridate::ymd), .SDcols = listdates[[namedataset]]]
#   data <- as.data.table(readxl::read_excel((paste0(thisdir, "/", namedataset, ".xlsx") )))
#   for (datevar in listdates[[namedataset]]) {
#     if (!is.na(baseline[[namedataset]][[datevar]])){
#     #  data[, (datevar) := as.Date(get(datevar), origin = "1970-01-01") + as.numeric(baseline[[namedataset]][[datevar]])]
#       data[, (datevar) := as.Date(get(datevar) + baseline[[namedataset]][[datevar]])]
#     }else{
#       data <- data[, (datevar) := lubridate::ymd(get(datevar))]
#       
#     }
#   }
#   
#   assign(namedataset,data)
#   saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
# }
# 
