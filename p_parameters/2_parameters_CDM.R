###################################################################
# ASSIGN PARAMETERS DESCRIBING THE DATA MODEL OF THE INPUT FILES
###################################################################

# assign -ARS_DATAMODEL_tables-: it is a 2-level list describing the ARS tables, and will enter the function as the first parameter. the first level is the data domain (in the example: 'Diagnosis' and 'Medicines') and the second level is the list of tables that has a column pertaining to that data domain 

ARS_DATAMODEL_tables <- vector(mode="list")

ARS_DATAMODEL_tables[["Diagnosis"]]=c("SDO","SDOTEMP","PS")
ARS_DATAMODEL_tables[["Medicines"]]=c("FED2016","FED2017","FED2018","FED2019","FED2020","SPF2016","SPF2017","SPF2018","SPF2019","SPF2020")
ARS_DATAMODEL_tables[["Procedure"]]=c("SDO","SDOTEMP")

# assign -ARS_DATAMODEL_codvar- and -ARS_DATAMODEL_coding_system_cols-: they are also 2-level lists, they encode from the data model the name of the column(s) of each table that contain, respectively the code and the coding system, corresponding to a data domain the table belongs to

alldomain<-unique(names(ARS_DATAMODEL_tables))

ARS_DATAMODEL_codvar <- vector(mode="list")
#ARS_DATAMODEL_coding_system_cols <-vector(mode="list")

for (dom in alldomain) {
  for (ds in ARS_DATAMODEL_tables[[dom]]) {
    if (dom=="Medicines") ARS_DATAMODEL_codvar[[dom]][[ds]]="COD_ATC5"
  }
}

ARS_DATAMODEL_codvar[["Diagnosis"]][["SDO"]] = c("DIADIM","PAT1","PAT2","PAT3","PAT4","PAT5")
ARS_DATAMODEL_codvar[["Diagnosis"]][["SDOTEMP"]] = c("DIADIM","PAT1","PAT2","PAT3","PAT4","PAT5")
ARS_DATAMODEL_codvar[["Diagnosis"]][["PS"]] = c("COD_DIAGNOSI_ARSNEW")
ARS_DATAMODEL_codvar[["Procedure"]][["SDO"]] = c("CODCHI2","CODCHI3","CODCHI4", "CODCHI5","CODCHI6" ,"CODCHI")
ARS_DATAMODEL_codvar[["Procedure"]][["SDOTEMP"]] = c("CODCHI2","CODCHI3","CODCHI4", "CODCHI5","CODCHI6" ,"CODCHI")

# assign 2 more 3-level lists: -id- -date-. They encode from the data model the name of the column(s) of each data table that contain, respectively, the personal identifier and the date. Those 2 lists are to be inputted in the rename_col option of the function. 
#NB: GENERAL  contains the names columns will have in the final datasets

ID <- vector(mode="list")
DATE <- vector(mode="list")

for (dom in alldomain) {
  for (ds in ARS_DATAMODEL_tables[[dom]]) {
    ID[[dom]][[ds]] = "IDUNI"
  }
}


for (dom in alldomain) {
  for (ds in ARS_DATAMODEL_tables[[dom]]) {
    if (dom=="Medicines") DATE[[dom]][[ds]] = "DATAERO"
  }
}

DATE[["Diagnosis"]][["SDO"]] = "DATAMM"
DATE[["Diagnosis"]][["SDOTEMP"]] = DATE[["Diagnosis"]][["SDO"]]
DATE[["Diagnosis"]][["PS"]] = "DATA_ORA_ACCETTAZ"
DATE[["Procedure"]][["SDO"]] =c("DATCHI","DATCHI2","DATCHI3","DATCHI4" ,"DATCHI5","DATCHI6")
DATE[["Procedure"]][["SDOTEMP"]] = DATE[["Diagnosis"]][["SDO"]]

ARS_DATAMODEL_datevar<-vector(mode="list")

ARS_DATAMODEL_datevar[["Diagnosis"]][["SDO"]] <- c("DATAMM")
ARS_DATAMODEL_datevar[["Diagnosis"]][["PS"]] = "DATA_ORA_ACCETTAZ"
ARS_DATAMODEL_datevar[["Diagnosis"]][["SDOTEMP"]] = ARS_DATAMODEL_datevar[["Diagnosis"]][["SDO"]]
ARS_DATAMODEL_datevar[["Procedure"]][["SDO"]] <- c("DATAMM")
ARS_DATAMODEL_datevar[["Procedure"]][["SDOTEMP"]] = ARS_DATAMODEL_datevar[["Procedure"]][["SDO"]]
ARS_DATAMODEL_datevar[["Medicines"]][["FED2016"]] <- c("DATAERO")
ARS_DATAMODEL_datevar[["Medicines"]][["FED2017"]] <- c("DATAERO")
ARS_DATAMODEL_datevar[["Medicines"]][["FED2018"]] <- c("DATAERO")
ARS_DATAMODEL_datevar[["Medicines"]][["FED2019"]] <- c("DATAERO")
ARS_DATAMODEL_datevar[["Medicines"]][["FED2020"]] <- c("DATAERO")
ARS_DATAMODEL_datevar[["Medicines"]][["SPF2016"]] <- c("DATAERO")
ARS_DATAMODEL_datevar[["Medicines"]][["SPF2017"]] <- c("DATAERO")
ARS_DATAMODEL_datevar[["Medicines"]][["SPF2020"]] <- c("DATAERO")
ARS_DATAMODEL_datevar[["Medicines"]][["SPF2018"]] <- c("DATAERO")
ARS_DATAMODEL_datevar[["Medicines"]][["SPF2019"]] <- c("DATAERO")
