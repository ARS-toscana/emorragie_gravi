###################################################################
# ASSIGN PARAMETERS DESCRIBING THE DATA MODEL OF THE INPUT FILES
###################################################################

# assign -TheShinISS_CDM_tables-: it is a 2-level list describing the ARS tables, and will enter the function as the first parameter. the first level is the data domain (in the example: 'Diagnosis' and 'Medicines') and the second level is the list of tables that has a column pertaining to that data domain 

TheShinISS_CDM_tables <- vector(mode="list")

TheShinISS_CDM_tables[["Diagnosis"]] = c("sdo","ps")
TheShinISS_CDM_tables[["Medicines"]]=c("fed")
# TheShinISS_CDM_tables[["Procedure"]]=c("SDO","SDOTEMP")

# assign -TheShinISS_CDM_codvar- and -TheShinISS_CDM_coding_system_cols-: they are also 2-level lists, they encode from the data model the name of the column(s) of each table that contain, respectively the code and the coding system, corresponding to a data domain the table belongs to

alldomain <- unique(names(TheShinISS_CDM_tables))

TheShinISS_CDM_codvar <- vector(mode="list")

for (dom in alldomain) {
  for (ds in TheShinISS_CDM_tables[[dom]]) {
    if (dom == "Medicines") TheShinISS_CDM_codvar[[dom]][[ds]] = "atc"
  }
}

TheShinISS_CDM_codvar[["Diagnosis"]][["sdo"]] = c("dia")

TheShinISS_CDM_codvar[["Diagnosis"]][["ps"]] = c("dia")
# TheShinISS_CDM_codvar[["Procedure"]][["SDO"]] = c("CODCHI2","CODCHI3","CODCHI4", "CODCHI5","CODCHI6" ,"CODCHI")
# TheShinISS_CDM_codvar[["Procedure"]][["SDOTEMP"]] = c("CODCHI2","CODCHI3","CODCHI4", "CODCHI5","CODCHI6" ,"CODCHI")

# assign 2 more 3-level lists: -id- -date-. They encode from the data model the name of the column(s) of each data table that contain, respectively, the personal identifier and the date. Those 2 lists are to be inputted in the rename_col option of the function. 
#NB: GENERAL  contains the names columns will have in the final datasets

ID <- vector(mode="list")
DATE <- vector(mode="list")

for (dom in alldomain) {
  for (ds in TheShinISS_CDM_tables[[dom]]) {
    ID[[dom]][[ds]] = "id"
  }
}


for (dom in alldomain) {
  for (ds in TheShinISS_CDM_tables[[dom]]) {
    if (dom == "Medicines") DATE[[dom]][[ds]] = "datasped"
  }
}

DATE[["Diagnosis"]][["sdo"]] = "data_a"
DATE[["Diagnosis"]][["ps"]] = DATE[["Diagnosis"]][["sdo"]]
# DATE[["Diagnosis"]][["PS"]] = "DATA_ORA_ACCETTAZ"
#DATE[["Procedure"]][["SDO"]] =c("DATCHI","DATCHI2","DATCHI3","DATCHI4" ,"DATCHI5","DATCHI6")

TheShinISS_CDM_datevar<-vector(mode = "list")

TheShinISS_CDM_datevar[["Diagnosis"]][["sdo"]] <- c("data_a")
TheShinISS_CDM_datevar[["Diagnosis"]][["ps"]] <- "data_a"
# TheShinISS_CDM_datevar[["Procedure"]][["SDO"]] <- c("DATAMM")
# TheShinISS_CDM_datevar[["Procedure"]][["SDOTEMP"]] = TheShinISS_CDM_datevar[["Procedure"]][["SDO"]]
TheShinISS_CDM_datevar[["Medicines"]][["fed"]] <- c("datasped")

