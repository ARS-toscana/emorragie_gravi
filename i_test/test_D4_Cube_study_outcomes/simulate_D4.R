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
namedataset <- "D4_analytical_dataset"

# set size of the dataset

df_size <- 10000

# create base 
data <- data.table::data.table(episode_id = 1:df_size #, 
                               # person_id
                               # gender
                               # ageband                         
                               # date_bleeding
                               # type_bleeding
                               # period
                               # outcome_AMI
                               # outcome_IS
                               # outcome_VTE
                               # outcome_TIA
                               # outcome_PE
                               # outcome_DIC
                               # outcome_DEATH
                               # covariate_1
                               # …
                               # covariate_26
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

# date_bleeding

set.seed(5243)
data[, date_bleeding := round(runif(.N, min = 0, max = as.numeric(study_end_date - study_start_date)))]
data[,date_bleeding := as.Date(ymd(study_start_date) + date_bleeding)]


# period


data[, period := NA_character_]
data[date_bleeding <= end_date_period[["1a"]], period := "1a"]
data[is.na(period) & date_bleeding <= end_date_period[["1b"]], period := "1b"]
data[is.na(period) & date_bleeding <= end_date_period[["1c"]], period := "1c"]
data[is.na(period) & date_bleeding <= end_date_period[["2"]], period := "2"]
data[is.na(period),  period := "3"]  

# type_bleeding

set.seed(7243)
data[, narrow := fifelse(runif(.N, min = 0, max = 1) > .6, 1, 0 )]

data[, type_bleeding := fifelse(narrow == 1, "narrow", "possible" )]

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


###############
# outcomes

# intercept

data[, b := 0]

# initialize b

coeff <- list()

# dependency on ageband

coeff[["0"]] <- log(.01)
coeff[["18"]] <- log(.1)
coeff[["40"]] <- log(.5)
coeff[["60"]] <- log(2)
coeff[["80"]] <- log(5)

for (age in agebands) {
  data[, b := b + coeff[[age]] * fifelse(ageband == age,1,0)]
}

# dependency on narrow

data[, b := b + log(2) * narrow]

# dependency on covariate

for (j in 1:26) {
  seedout <- 734782 + j
  set.seed(seedout)
  a <- runif(1, min = 1, max = 1.05)
  data[, b := b + log(a) * get(paste0("covariate_",j))]
}

# dependency on period (exposure)

coeff[["1a"]] <- log(2)
coeff[["1b"]] <- log(.5)
coeff[["1c"]] <- log(2)
coeff[["2"]] <- log(1)
coeff[["3"]] <- log(1)

for (per in unlist(unique(data[,.(period)]))) {
  data[, b := b + coeff[[per]] * fifelse(period == per,1,0)]
}


# assign probability of outcome

data[, prob := 1/(1 + exp(1 - b))]

#exploratory outcome

set.seed(25184)
data[, outcome := as.integer(runif(.N) < prob)]

# fit the model and inspect it to make sure it works as expected

listofvar <- c("ageband", "narrow", "period", paste0("covariate_",1:26))
predictor_formula <- paste(listofvar, collapse = " + ")
full_formula <- paste("outcome", "~", predictor_formula)
model_formula <- as.formula(full_formula)
model <- glm(model_formula, data = data, family = binomial())
odds_ratios <- exp(coef(model))
se_log_odds <- sqrt(diag(vcov(model)))
confint_low <- exp(coef(model) - 1.96 * se_log_odds)
confint_high <- exp(coef(model) + 1.96 * se_log_odds)
p_values <- summary(model)$coefficients[, 4]
summary_table <- data.frame(
  OddsRatio = odds_ratios,
  CI_Low = confint_low,
  CI_High = confint_high,
  P_Value = p_values
)
print(summary_table)

# outcome_DEATH (competitive with "outcome_AMI", "outcome_IS", "outcome_TIA")

seedout <- 65783
set.seed(seedout)
data[, outcome_DEATH := as.integer(runif(.N) < prob)]

seedout <- 157831
set.seed(seedout)
data[, days_DEATH := round(rpois(.N, lambda = 2))] # rpois(.N, lambda = 5)
data[, days_DEATH := pmin(days_DEATH, 30)]

# competitive outcome

data[, b := b + log(.8) * outcome_DEATH]


# assign probability of outcome

data[, prob := 1/(1 + exp(1 - b))]

j <- 1
for (outcomet in c("outcome_AMI", "outcome_IS", "outcome_VTE", "outcome_TIA", "outcome_PE", "outcome_DIC")) {
  seedout <- 342782 + j
  set.seed(seedout)
  data[, outcome := as.integer(runif(.N) < prob)]
  setnames(data,"outcome",outcomet)
  j <- j + 1
}

# add few duplicate cases

set.seed(457378)
datan <- data[ runif(.N) > .98 & outcome_DEATH == 0,]

datan[, date_bleeding := date_bleeding + round(runif(1, min = 50, max = 500))]

seedout <- 34566
set.seed(seedout)
datan[, outcome_DEATH := as.integer(runif(.N) < prob)]
datan[, period := NA_character_]
datan[date_bleeding <= end_date_period[["1a"]], period := "1a"]
datan[is.na(period) & date_bleeding <= end_date_period[["1b"]], period := "1b"]
datan[is.na(period) & date_bleeding <= end_date_period[["1c"]], period := "1c"]
datan[is.na(period) & date_bleeding <= end_date_period[["2"]], period := "2"]
datan[is.na(period),  period := "3"]  


data <- rbind(data,datan)

data[,episode_id := seq_len(.N)]


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

tokeep <- c("episode_id", "person_id", "gender", "ageband", "age", "date_bleeding", "type_bleeding", "period", "outcome_AMI", "outcome_IS", "outcome_VTE", "outcome_TIA", "outcome_PE", "outcome_DIC", "outcome_DEATH", "days_DEATH", paste0("covariate_",as.character(1:26)))

data <- data[, ..tokeep]


saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
