# #set dates

study_start_date <-  ymd(20150101)
study_end_date <- ymd(20240630)

instance_creation <- ymd(CDM_SOURCE[1,"date_creation"])
recommended_end_date <- ymd(CDM_SOURCE[1,"recommended_end_date"])
study_end_date <- min(study_end_date, instance_creation, recommended_end_date, na.rm = T)


# periods

start_date_period <- list()
end_date_period <- list()

start_date_period[["1"]] <- study_start_date
end_date_period[["1"]] <- ymd(20190708)

start_date_period[["2a"]] <- end_date_period[["1"]] + 1
end_date_period[["2a"]] <- ymd(20200305)

start_date_period[["2b"]] <- end_date_period[["2a"]] + 1
end_date_period[["2b"]] <- ymd(20210621)

start_date_period[["2c"]] <- end_date_period[["2b"]] + 1
end_date_period[["2c"]] <- ymd(20230731)

start_date_period[["3"]] <- end_date_period[["2c"]] + 1
end_date_period[["3"]] <- study_end_date

# days for CreateSpells

days <- ifelse(thisdatasource %in% c("ARS","TEST"), 180, 1)

# agebands

Agebands_countpersontime = c(0, 17, 39, 59 ,79)
Agebands_labels <- c("0-17", "18-39", "40-59", "60-79", "80+")
names(Agebands_countpersontime) <- Agebands_labels

Agebands_large = c(0, 18, 60)
Agebands_large_labels = c("0-17","18-59","60+")
names(Agebands_large) <- Agebands_large_labels

# duration of apixaban and rivaroxaban

duration_MoI <- as.data.table(readxl::read_excel(file.path(thisdir,"p_parameters","archive_parameters","duration_MoI.xlsx")))

