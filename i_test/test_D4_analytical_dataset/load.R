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

baselinedate <- 20190101

# list of datasets

listdatasets <- c("D3_study_population","D3_study_outcomes")

# dates variables 

listdates <- list()
listdates[["D3_study_outcomes"]] <- "date"
listdates[["D3_study_population"]] <- c("birth_date", "death_date","date_bleeding","end_followup_d")

# date baseline

baseline <- vector(mode="list")
for (namedataset in listdatasets) {
  for (datevar in listdates[[namedataset]]) {
    baseline[[namedataset]][[datevar]] <- as.Date(lubridate::ymd(baselinedate))
  }
}


# load datasets


for (namedataset in listdatasets){
  # data <- fread(paste0(thisdir, "/", namedataset, ".csv") )
  # data[, (listdates[[namedataset]]) := lapply(.SD, lubridate::ymd), .SDcols = listdates[[namedataset]]]
  data <- as.data.table(readxl::read_excel((paste0(thisdir, "/", namedataset, ".xlsx") )))
  for (datevar in listdates[[namedataset]]) {
    if (!is.na(baseline[[namedataset]][[datevar]])){
    #  data[, (datevar) := as.Date(get(datevar), origin = "1970-01-01") + as.numeric(baseline[[namedataset]][[datevar]])]
      data[, (datevar) := as.Date(get(datevar) + baseline[[namedataset]][[datevar]])]
    }else{
      data <- data[, (datevar) := lubridate::ymd(get(datevar))]
      
    }
  }
  
  assign(namedataset,data)
  saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
}

