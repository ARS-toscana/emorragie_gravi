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

# study period

study_start_date <-  ymd(20180101)
study_end_date <- ymd(20231231)


start_date_period <- list()
end_date_period <- list()



end_date_period <- list()
end_date_period[["1a"]] <- ymd(20200229)
end_date_period[["1b"]] <- ymd(20210531)
end_date_period[["1c"]] <- ymd(20210831)
end_date_period[["2"]] <- ymd(20230731)
end_date_period[["3"]] <- study_end_date


start_date_period[["1a"]] <- study_start_date
start_date_period[["1b"]] <- end_date_period[["1a"]] + 1
start_date_period[["1c"]] <- end_date_period[["1b"]] + 1
start_date_period[["2"]] <- end_date_period[["1c"]] + 1
start_date_period[["3"]] <- end_date_period[["2"]] + 1



# name of the dataset to be generated
namedataset <- "D3_study_population"

# set size of the dataset

df_size <- 10000

# create base 
data <- data.table::data.table(episode_id = 1:df_size #, 
                               
)

# person_id 

data[,person_id := paste0("000000",as.character(seq_len(.N)))]
data[, person_id := paste0("P",substr(person_id, nchar(person_id) - 6, nchar(person_id)))]

# gender

set.seed(1234)
data[, gender := as.character(sample(1:2, df_size, replace = TRUE, prob = c(.5,.5)))]
data[,gender := fifelse(gender == "1","M","F") ]

# ageband

set.seed(5234)
agebands <- c("0","18","40","60","80")
data[, ageband := sample(agebands, df_size, replace = TRUE, prob = c(.001,.01,.2,.4, 1 - (.001 + .01 + .2 + .4)))]
data[, ageband := as.factor(ageband)]

# date

set.seed(5243)
data[, date := round(runif(.N, min = 0, max = as.numeric(study_end_date - study_start_date)))]
data[,date := as.Date(ymd(study_start_date) + date)]

setnames(data,"date","date_bleeding")


# period


data[, period := NA_character_]
data[date_bleeding <= end_date_period[["1a"]], period := "1a"]
data[is.na(period) & date_bleeding <= end_date_period[["1b"]], period := "1b"]
data[is.na(period) & date_bleeding <= end_date_period[["1c"]], period := "1c"]
data[is.na(period) & date_bleeding <= end_date_period[["2"]], period := "2"]
data[is.na(period),  period := "3"]  
# event

set.seed(7243)
data[, narrow := fifelse(runif(.N, min = 0, max = 1) > .6, 1, 0 )]

data[, event := fifelse(narrow == 1, "bleeding_narrow", "bleeding_possible" )]

###########
# covariates

# intercept

data[, b := 0]

# initialize b

coeff <- list()

# dependency on ageband

coeff[["0"]] <- log(.1)
coeff[["18"]] <- log(.2)
coeff[["40"]] <- log(.5)
coeff[["60"]] <- log(2)
coeff[["80"]] <- log(5)

for (age in agebands) {
  data[, b := b + coeff[[age]] * fifelse(ageband == age,1,0)]
}

data[, prob := 1/(1 + exp(-b))]

for (j in 1:26) {
  seedout <- 534782 + j
  set.seed(seedout)
  data[, cov := as.integer(runif(.N) < prob)]
  setnames(data,"cov",paste0("covariate_",j))
}

# number of previous bleedings

data[, number_previous_bleedings := 0]


# add few duplicate cases

set.seed(457378)
datan <- data[ runif(.N) > .98 ,]

datan[, date_bleeding_new := date_bleeding + round(runif(1, min = 50, max = 500))]

datan[, days_since_most_recent_bleeding := as.integer(date_bleeding_new - date_bleeding)]

datan[,date_bleeding := NULL]
setnames(datan,"date_bleeding_new","date_bleeding")

datan[, period := NA_character_]
datan[date_bleeding <= end_date_period[["1a"]], period := "1a"]
datan[is.na(period) & date_bleeding <= end_date_period[["1b"]], period := "1b"]
datan[is.na(period) & date_bleeding <= end_date_period[["1c"]], period := "1c"]
datan[is.na(period) & date_bleeding <= end_date_period[["2"]], period := "2"]
datan[is.na(period),  period := "3"]  

datan[, number_previous_bleedings := 1]

data <- rbind(data,datan, fill = T)

# some other cases have past episodes

seedout <- 34561
set.seed(seedout)
data[, number_previous_bleedings := fifelse (runif(.N) > .98,number_previous_bleedings + 1, number_previous_bleedings)]

data <- data[number_previous_bleedings > 0 & is.na(days_since_most_recent_bleeding), days_since_most_recent_bleeding := 500]

# define age

agebands <- c(agebands, "100")
agebands_numeric <- as.integer(c(agebands, 100))
data[, ageband_num := as.integer(agebands[as.integer(ageband)])]  # 
data[, age := {
  lower <- ageband_num
  upper <- agebands_numeric[match(lower, agebands_numeric) + 1] - 1
  sample(lower:upper, size = .N, replace = TRUE)
}, by = ageband]



# append and clean

tokeep <- c("episode_id", "person_id", "gender", "ageband", "age", "date_bleeding", "event", "period", "number_previous_bleedings", "days_since_most_recent_bleeding", paste0("covariate_",as.character(1:26)))

data <- data[, ..tokeep]


saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
