########################################################%##
#                                                          #
####  CREATE D6_Table_2  ####
#                                                          #
########################################################%##


# author: Rosa Gini


# v 1.0 12 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D6_Table_3_study_outcomes"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# import


tab_nice <- readRDS(file.path(thisdirinput, "D5_Table_3_study_outcomes.rds"))

# create the list of outcomes calculated already

outcome_vars <- grep("^outcome", names(tab_nice), value = TRUE)

# calculate cells

col_header_1 <- c("Tipo di sanguinamento")


tab_nice[,(paste0("head_",1)) := type_bleeding ]

row_header_1 <- c()

s <- 1

row_header_1 = c(row_header_1,
  "N")

tab_nice[,(paste0("cell_",s)) := as.character(N) ]
s = s + 1

for (out in outcome_vars) {
  row_header_1 = c(row_header_1,
                   name_outcome[[out]])
  tab_nice[,(paste0("cell_",s)) := paste0(as.character(get(out))," (",as.character(get(paste0("p_",out))),"%)") ]
  s = s + 1
  
}

headers <- names(tab_nice)[grep("^head_",names(tab_nice))]

tokeep <- c("Period_LabelValue",headers,names(tab_nice)[grep("^cell_",names(tab_nice))])

tab_nice <- tab_nice[, ..tokeep]



# Reshape
tab_nice <- melt(tab_nice, id.vars = c("Period_LabelValue",headers), measure.vars = patterns("^cell_"), variable.name = "cell", value.name = "value")

# 
# 
# # Cast to one column per 'period'
# tab_nice <- dcast(tab_nice, cell ~ c("Period_LabelValue", headers), value.var = "value")
# 
# # transform cell into a number
# tab_nice[, cell := gsub("cell_", "", cell)]
# tab_nice[, cell := as.numeric(gsub("_", ".", cell))]
# 
# 
# # order
# setorder(tab_nice, cell)
# tab_nice[, ord := seq_len(.N)]
# 
# 
# # add row_header_1
# tab_nice[, row_header := row_header_1[ord]]
# 
# 
# 
# # remove cell
# tab_nice[, cell := NULL]
# tab_nice[, ord := NULL]
# 
# # rename
# old_col_names = c("row_header",setdiff( names(tab_nice),"row_header"))
# new_col_names <- unlist(c("", name_period[setdiff( names(tab_nice),"row_header")]))
# setnames(tab_nice, old_col_names,  new_col_names)
# 
# tab_nice <- tab_nice[, ..new_col_names]
# 
# 
# ########################################
# # save
# 
# outputfile <- tab_nice
# nameoutput <- "D6_Table_3_study_outcomes"
# assign(nameoutput, outputfile)
# # rds
# saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
# # csv
# fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
# # xls
# write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
# # html
# html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
# writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))
# # rtf
# doc <- read_docx() %>% body_add_table(outputfile, style = "table_template") %>% body_end_section_continuous()
# print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))
#   
# 
# 
# 
# 
