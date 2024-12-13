########################################################%##
#                                                          #
####  CREATE D5_Table_2  ####
#                                                          #
########################################################%##


# author: Rosa Gini


# v 1.0 11 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D5_Table_2"
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
  
  temp <- cohort[, .(min_date = min(date_bleeding), max_date = max(date_bleeding)), by = period]
  base_table2 <- merge(base_table2,temp,by = "period", all.x = T)
  
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
  
  nameoutput <- paste0("D5_Table_2_",type)
  nameoutputext <- paste0(nameoutput,".rds")
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
  
}

