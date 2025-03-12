########################################################%##
#                                                          #
####  CREATE D6_Table_2  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.1 16 Dec 2024

# added number of previous bleedings and median days since most recent

# v 1.0 12 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D6_Table_2_descr_study_pop"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}

# import


for (type in c("narrow","broad")) {
  tab_nice <- readRDS(file.path(thisdirinput, paste0("D5_Table_2_descr_study_pop_",type,".rds")))
  
 
  s = 1
  
  row_header_1 = c(
    "N")
  
  tab_nice[,(paste0("cell_",s)) := as.character(n) ]
  s = s + 1
  
  row_header_1 = c(row_header_1, "Donne (N, %)")
  
  tab_nice[,(paste0("cell_",s)) := paste0(as.character(n_gender_F), " (",as.character(p_gender_F), "%)") ]
  s = s + 1
  
  row_header_1 = c(row_header_1, "Età (media, ds)")

  tab_nice[,(paste0("cell_",s)) := paste0(as.character(mean_age), " (",as.character(sd_age), ")") ]
  s = s + 1

  row_header_1 = c(row_header_1, "Classe di età (N,%)")
  
  tab_nice[,(paste0("cell_",s)) := " " ]
  s = s + 1
  
  for (val in c("0-17","18-39","40-49","60-79","80+")) {
    row_header_1 = c(row_header_1, val)
    data.table::setnames(tab_nice,c(paste0("n_ageband_",val),paste0("p_ageband_",val)),c("n_temp","p_temp") )
    tab_nice[,(paste0("cell_",s)) := paste0(as.character(n_temp), " (",as.character(p_temp), "%)") ]
    data.table::setnames(tab_nice,c("n_temp","p_temp"),c(paste0("n_ageband_",val),paste0("p_ageband_",val)) )
    s = s + 1
    
  }
  
  row_header_1 = c(row_header_1, "Range di date (min-max)")
  
  tab_nice[,(paste0("cell_",s)) := paste0(as.character(min_date), " - ",as.character(max_date)) ]
  s = s + 1
  
  row_header_1 = c(row_header_1, "Numero di sanguinamenti nei 3 anni precedenti (N, %)")
  
  tab_nice[,(paste0("cell_",s)) :=  " " ]
  s = s + 1
  
  for (val in c("0","1","2")) {
    valdet <- fifelse(val == "2","2+",val)
    row_header_1 = c(row_header_1, valdet)
    tab_nice[,(paste0("cell_",s)) := paste0(as.character(get(paste0("n_previous_bleedings_",val) )), " (",as.character(get(paste0("p_previous_bleedings_",val) )),"%)")]
    s = s + 1  
  }
  
  row_header_1 = c(row_header_1, "Giorni dal più recente sanguinamento (Mediana, range IQ)")
  
  tab_nice[,(paste0("cell_",s)) :=  paste0(as.character(median_days_since_most_recent)," (",as.character(q1_days_since_most_recent),"-",as.character(q3_days_since_most_recent),")" )]
  s = s + 1
  
  if (type == "broad") {
    row_header_1 = c(row_header_1, "Definizione narrow")
    
    tab_nice[,(paste0("cell_",s)) := paste0(as.character(n_narrow), " (",as.character(p_narrow), "%)") ]
    s = s + 1
  }

    
  
  tokeep <- c("period",names(tab_nice)[grep("^cell_",names(tab_nice))])
  
  tab_nice <- tab_nice[, ..tokeep]
  
  
  # Reshape
  tab_nice <- melt(tab_nice, id.vars = "period", measure.vars = patterns("^cell_"), variable.name = "cell", value.name = "value")
  
  
  
  # Cast to one column per 'period'
  tab_nice <- dcast(tab_nice, cell ~ period, value.var = "value")
  
  # transform cell into a number
  tab_nice[, cell := gsub("cell_", "", cell)]
  tab_nice[, cell := as.numeric(gsub("_", ".", cell))]
  
  
  # order
  setorder(tab_nice, cell)
  tab_nice[, ord := seq_len(.N)]
  
  
  # add row_header_1
  tab_nice[, row_header := row_header_1[ord]]
  
  
  
  # remove cell
  tab_nice[, cell := NULL]
  tab_nice[, ord := NULL]
  
  # rename
  old_col_names = c("row_header",setdiff( names(tab_nice),"row_header"))
  new_col_names <- unlist(c("", name_period[setdiff( names(tab_nice),"row_header")]))
  setnames(tab_nice, old_col_names,  new_col_names)
  
  tab_nice <- tab_nice[, ..new_col_names]
  
  
  ########################################
  # save
  
  outputfile <- tab_nice
  nameoutput <- paste0("D6_Table_2_descr_study_pop_",type)
  assign(nameoutput, outputfile)
  # rds
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  # csv
  fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
  # xls
  write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
  # html
  html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
  writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))
  # rtf
  doc <- read_docx() %>% body_add_table(outputfile, style = "table_template") %>% body_end_section_continuous()
  print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))
  
}



