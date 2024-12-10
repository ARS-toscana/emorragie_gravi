# #set dates

study_start_date <-  ymd(20180101)
study_end_date <- ymd(20240630)

baselinedate_components <- ymd(20190101)

# 
# instance_creation <- ymd(CDM_SOURCE[1,"date_creation"])
# recommended_end_date <- ymd(CDM_SOURCE[1,"recommended_end_date"])
# study_end_date <- min(study_end_date, instance_creation, recommended_end_date, na.rm = T)


# periods

start_date_period <- list()
end_date_period <- list()

start_date_period[["1a"]] <- study_start_date
end_date_period[["1a"]] <- ymd(20200308)

start_date_period[["1b"]] <- end_date_period[["1a"]] + 1
end_date_period[["1b"]] <- ymd(20210621)

start_date_period[["1c"]] <- end_date_period[["1b"]] + 1
end_date_period[["1c"]] <- ymd(20210831)

start_date_period[["2"]] <- end_date_period[["1c"]] + 1
end_date_period[["2"]] <- ymd(20230731)

start_date_period[["3"]] <- end_date_period[["2"]] + 1
end_date_period[["3"]] <- study_end_date

# days for CreateSpells

days <- 365

# agebands

Agebands_countpersontime = c(0, 17, 39, 59 ,79)
Agebands_labels <- c("0-17", "18-39", "40-59", "60-79", "80+")
names(Agebands_countpersontime) <- Agebands_labels

Agebands_large = c(0, 18, 60)
Agebands_large_labels = c("0-17","18-59","60+")
names(Agebands_large) <- Agebands_large_labels


# duration of apixaban and rivaroxaban

duration_MoI <- as.data.table(readxl::read_excel(file.path(thisdir,"p_parameters","archive_parameters","duration_MoI.xlsx")))

