########################################################%##
#                                                          #
####  CREATE D5_Table_2_descr_study_pop  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.2 18 Dec 2024

# added total

# v 1.1 16 Dec 2024

# added number of previous bleedings and median days since most recent

# v 1.0 11 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D5_Table_2_descr_study_pop"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# import


for (type in c("narrow","broad")) {
  cohort <- readRDS(file.path(thisdirinput, "D3_study_population.rds"))
  
  if (type == "narrow") {
    cohort <- cohort[event == "bleeding_narrow",]
  }
  
  total <- copy(cohort)
  total[,period := "AllPeriod"]
  
  cohort <- rbind(cohort, total)
  
  base_table2 <- cohort[, .(n = .N), by = period]
  temp <- cohort[gender == "F", .(n_gender_F = .N), by = period]
  base_table2 <- merge(base_table2,temp,by = "period", all.x = T)
  base_table2[,p_gender_F := round(100 * n_gender_F/n,1)]
  
  temp <- cohort[, .(mean_age = round(mean(age),1), sd_age = round(sd(age),1)), by = period]
  base_table2 <- merge(base_table2,temp,by = "period", all.x = T)
  
  
  for (val in c("0-17","18-39","40-49","60-79","80+")) {
    temp <- cohort[ageband == val, .(n_temp = .N), by = period]
    base_table2 <- merge(base_table2,temp,by = "period", all.x = T)
    base_table2[,p_temp := round(100 * n_temp/n,1)]
    base_table2[is.na(p_temp),n_temp := 0]
    base_table2[is.na(p_temp),p_temp := 0]
    data.table::setnames(base_table2,c("n_temp","p_temp"),c(paste0("n_ageband_",val),paste0("p_ageband_",val)))
  }
  
  # range dates
  
  temp <- cohort[, .(min_date = min(date_bleeding), max_date = max(date_bleeding)), by = period]
  base_table2 <- merge(base_table2,temp,by = "period", all.x = T)
  
  # previous episodes
  
  temp <- cohort[, .(N = .N) , by = c("period","number_previous_bleedings")]
  
  temp <- dcast(temp, period ~ number_previous_bleedings, 
                     value.var = "N", fill = 0)
  
  setnames(temp, c("0","1","2"), paste0("number_previous_bleedings_",c("0","1","2") ))
  
  base_table2 <- merge(base_table2,temp,by = "period", all.x = T)
  
  for (val in c("0","1","2")) {
    setnames(base_table2, paste0("number_previous_bleedings_",val ), "n_temp")
    base_table2[,p_temp := round(100 * n_temp/n,1)]
    base_table2[is.na(p_temp),n_temp := 0]
    base_table2[is.na(p_temp),p_temp := 0]
    data.table::setnames(base_table2,c("n_temp","p_temp"),c(paste0("n_previous_bleedings_",val),paste0("p_previous_bleedings_",val)))
  }
    
  # days since most recent episode 
  
  temp <- cohort[number_previous_bleedings > 0, .(median_days_since_most_recent = quantile(days_since_most_recent_bleeding, probs = 0.5, na.rm = TRUE),
                                                   q1_days_since_most_recent = quantile(days_since_most_recent_bleeding, probs = 0.25, na.rm = TRUE),
                                                   q3_days_since_most_recent = quantile(days_since_most_recent_bleeding, probs = 0.75, na.rm = TRUE)
                                                   ) , by = c("period")]
  
  base_table2 <- merge(base_table2,temp,by = "period", all.x = T)
  
  # case narrow
  
  if (type == "broad") {
    temp <- cohort[event == "bleeding_narrow", .(n_temp = .N), by = period]
    base_table2 <- merge(base_table2,temp,by = "period", all.x = T)
    base_table2[,p_temp := round(100 * n_temp/n,1)]
    base_table2[is.na(n_temp),p_temp := 0]
    base_table2[is.na(n_temp),n_temp := 0]
    data.table::setnames(base_table2,c("n_temp","p_temp"),c("n_narrow","p_narrow"))
  }
  
  
  # ################################
  # # clean
  # 
  # tokeep <- c("person_id","birth_date","gender","death_date","date_bleeding","event","end_followup_d")
  # 
  # processing <- processing[, ..tokeep]
  # 
  # setorderv(
  #   processing,
  #    c("person_id","date_bleeding")
  # )
  # 
  # 
  #########################################
  # save
  
  outputfile <- base_table2
  
  nameoutput <- paste0("D5_Table_2_descr_study_pop_",type)
  nameoutputext <- paste0(nameoutput,".rds")
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
  
}

