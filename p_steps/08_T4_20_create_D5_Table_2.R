########################################################%##
#                                                          #
####  CREATE D5_Table_2  ####
#                                                          #
########################################################%##


# author: Rosa Gini


# v 0.1 11 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D5_Table_2"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import


for (type in c("narrow","broad")) {
  cohort <- readRDS(file.path(thisdirinput, "D3_study_population.rds"))
  
  if (type == "narrow") {
    cohort <- cohort[type_bleeding == "narrow",]
  }
  
  base_table2 <- cohort[, .(n = .N), by = period]
  temp <- cohort[gender == "F", .(n_gender_F = .N), by = period]
  base_table2 <- merge(base_table2,temp,by = "period")
  base_table2[,p_gender_F := round(100 * n_gender_F/n,1)]
  
  for (val in unlist(unique(cohort[,.(ageband)]))) {
    temp <- cohort[ageband == val, .(n_temp = .N), by = period]
    base_table2 <- merge(base_table2,temp,by = "period")
    base_table2[,p_temp := round(100 * n_temp/n,1)]
    data.table::setnames(base_table2,c("n_temp","p_temp"),c(paste0("n_ageband_",val),paste0("p_ageband_",val)))
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
  # #########################################
  # # save
  # 
  # outputfile <- base_table2
  # 
  # nameoutput <- paste0("D5_Table_2_",type)
  # nameoutputext <- paste0(nameoutput,".rds")
  # assign(nameoutput, outputfile)
  # saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
  
}

