rm(list=ls(all.names=TRUE))


baselinedate <- 20180101


#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

# list of datasets

listdatasetsRData <- c("AMI", "IS", "VTE", "TIA", "PE","DIC")

listdatasets <- c(listdatasetsRData,"D4_persons_with_MoI")

# dates variables 

listdates <- list()

for (ds in listdatasetsRData){
  listdates[[ds]] <- c("DATE")
  }


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
    if (nrow(data) == 0){
      data[,person_id := as.character(person_id)]
    }
  }
  if ("medicinal_product_atc_code" %in% names(data)) {
    setnames(data,"medicinal_product_atc_code","codvar")
    
  }
  assign(namedataset,data)
  if (namedataset %in% listdatasetsRData){
    save(data, file = file.path(thisdir, paste0(namedataset,".RData")), list = namedataset)
  }else{
  saveRDS(data, file = file.path(thisdir, paste0(namedataset,".rds")))
  }
}

