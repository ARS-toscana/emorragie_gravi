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
  thisdiroutput <- dirtemp
}

# import


for (type in c("narrow","broad")) {
  tab <- readRDS(file.path(thisdirinput, "D3_study_population.rds"))
  if (type == "narrow") {
    tab <- tab[type_bleeding == "narrow",]
  }
  
    # ...
  
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
  # outputfile <- processing
  # 
  # nameoutput <- paste0("D5_Table_2_",type)
  # nameoutputext <- paste0(nameoutput,".rds")
  # assign(nameoutput, outputfile)
  # saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
  
}

