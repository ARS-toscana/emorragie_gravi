########################################################%##
#                                                          #
####  CREATE D6_Table_3  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.1 18 Dec 2024

# add days to death

# v 1.0 12 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D6_Table_3_study_outcomes"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}

# import


tab_nice <- readRDS(file.path(thisdirinput, "D5_Table_3_study_outcomes.rds"))

# create the list of outcomes calculated already

outcome_vars <- grep("^outcome", names(tab_nice), value = TRUE)

# calculate cells

row_header_1 <- c("Tipo di sanguinamento")


tab_nice[,(paste0("head_",1)) := type_bleeding ]

col_headers <-  c()

s <- 1

col_headers = c(col_headers,"N")

tab_nice[,(paste0("cell_",s)) := as.character(N) ]
s = s + 1

for (out in outcome_vars) {
  col_headers = c(col_headers, name_outcome[[out]])
  tab_nice[,(paste0("cell_",s)) := paste0(as.character(get(out))," (",as.character(get(paste0("p_",out))),"%)") ]
  s = s + 1
}

col_headers = c(col_headers, "Giorni dal sanguinamento al decesso")
tab_nice[,(paste0("cell_",s)) := paste0(as.character(days_DEATH_q50)," (",as.character(days_DEATH_q25),"-",as.character(days_DEATH_q75),")") ]
s = s + 1

row_headers <- names(tab_nice)[grep("^head_",names(tab_nice))]
cells <- names(tab_nice)[grep("^cell_",names(tab_nice))]

tokeep <- c("Period_LabelValue",row_headers,cells)

tab_nice <- tab_nice[, ..tokeep]

# 
# 
# # # Reshape
# # tab_nice <- melt(tab_nice, id.vars = c("Period_LabelValue",headers), measure.vars = patterns("^cell_"), variable.name = "cell", value.name = "value")
# 
# # order
# 
# setorderv(
#   tab_nice, c("head_1","Period_LabelValue")
#   )
# 
# tab_nice[,period := name_period[Period_LabelValue]]
# 
# tokeep <- c("period", "head_1",cells)
# 
# tab_nice <- tab_nice[, ..tokeep]
# 
# setnames(tab_nice,tokeep, c("Periodo",row_header_1,col_headers))

tab_nice <- melt(tab_nice, 
                 id.vars = c("Period_LabelValue", "head_1"),  # Columns to keep as is
                 measure.vars = patterns("^cell_"),         # Columns to melt (those starting with "cell_")
                 variable.name = "cell", 
                 value.name = "value")

# Reshape the table back to wide format with Period_LabelValue as columns
tab_nice <- dcast(tab_nice, 
                  head_1 + cell ~ Period_LabelValue, 
                  value.var = "value")

# transform cell into a number
tab_nice[, cell := gsub("cell_", "", cell)]
tab_nice[, cell := as.numeric(gsub("_", ".", cell))]


# order
setorderv(tab_nice, c("head_1","cell"))
tab_nice[, ord := seq_len(.N)]

# add row_header
tab_nice[, row_header := col_headers[cell]]

# remove cell
tab_nice[, cell := NULL]
tab_nice[, ord := NULL]

# rename
old_col_names = c(c("head_1","row_header"),setdiff( names(tab_nice),c("head_1","row_header")))
new_col_names <- unlist(c("Tipo sanguinamento","Periodo", name_period[setdiff( names(tab_nice),c("head_1","row_header"))]))
setnames(tab_nice, old_col_names,  new_col_names)

tab_nice <- tab_nice[, ..new_col_names]



########################################
# save

outputfile <- tab_nice
nameoutput <- "D6_Table_3_study_outcomes"
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





