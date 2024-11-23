
#APPLY THE FUNCTION CreateConceptSetDatasets TO CREATE ONE DATASET PER CONCEPT SET CONTAINING ONLY THE CODES OF INTEREST


CreateConceptSetDatasets(dataset = ARS_DATAMODEL_tables,
                         codvar = ARS_DATAMODEL_codvar,
                         datevar = ARS_DATAMODEL_datevar,
                         dateformat = "YYYYmmdd",
                         rename_col = list(ID=ID,DATE=DATE),
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_names = concept_sets_of_our_study_drugs,
                         dirinput = dirinput,
                         diroutput = dirtemp,
                         extension = c("dta"))


CreateConceptSetDatasets(dataset = ARS_DATAMODEL_tables,
                         codvar = ARS_DATAMODEL_codvar,
                         datevar = ARS_DATAMODEL_datevar,
                         dateformat = "YYYYmmdd",
                         rename_col = list(ID=ID,DATE=DATE),
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_names = concept_sets_of_our_study_diagnosis,
                         concept_set_codes_excl = concept_set_codes_our_study_excl,
                         dirinput = dirinput,
                         diroutput =dirtemp,
                         extension = c("dta"))


CreateConceptSetDatasets(dataset = ARS_DATAMODEL_tables,
                         codvar = ARS_DATAMODEL_codvar,
                         datevar = ARS_DATAMODEL_datevar,
                         dateformat = "YYYYmmdd",
                         rename_col = list(ID=ID),
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_names = concept_sets_of_our_study_procedure,
                         concept_set_codes_excl = concept_set_codes_our_study_excl,
                         dirinput = dirinput,
                         diroutput =dirtemp,
                         extension = c("dta"))

# #remove all codes to exclude in the same diagnosis
# #remove subjects with concomitant diagnosis Hemorrhagic_stroke
# Hemorrhagic_stroke_allsubjects<-Hemorrhagic_stroke
# save(Hemorrhagic_stroke_allsubjects,file=paste0(dirtemp,"Hemorrhagic_stroke_allsubjects.RData"))
# 
# Hemorrhagic_stroke_SDO<-Hemorrhagic_stroke[Table_cdm=="SDO" |Table_cdm=="SDOTEMP",.(ID_SKNO_ARSNEW,ID,DIADIM,PAT1,PAT2,PAT3,PAT4,PAT5,DATAMM)]
# Hemorrhagic_stroke_excl_SDO<-Hemorrhagic_stroke_excl[Table_cdm=="SDO",.(ID_SKNO_ARSNEW,ID,DIADIM,PAT1,PAT2,PAT3,PAT4,PAT5,DATAMM)]
# 
# prova<-merge(Hemorrhagic_stroke_SDO,Hemorrhagic_stroke_excl_SDO,by="ID_SKNO_ARSNEW",all = F)
# id_to_remove<-prova[,"ID_SKNO_ARSNEW"] #TOGLI ""
# #by ANCHE per ID
# 
# id_to_remove<-unlist(id_to_remove)
# 
# '%!in%' <- function(x,y)!('%in%'(x,y))
# Hemorrhagic_stroke<-Hemorrhagic_stroke[Hemorrhagic_stroke$ID_SKNO_ARSNEW %!in% id_to_remove,]
# 
# save(Hemorrhagic_stroke,file=paste0(dirtemp,"Hemorrhagic_stroke.RData"))
# rm(Hemorrhagic_stroke_SDO,Hemorrhagic_stroke_excl,Hemorrhagic_stroke_excl_SDO)
# 
# 
# #remove subjects with concomitant diagnosis Ischemic_stroke
# Ischemic_stroke_allsubjects<-Ischemic_stroke
# save(Ischemic_stroke_allsubjects,file=paste0(dirtemp,"Ischemic_stroke_allsubjects.RData"))
# 
# Ischemic_stroke_SDO<-Ischemic_stroke[Table_cdm=="SDO",.(ID_SKNO_ARSNEW,ID,DIADIM,PAT1,PAT2,PAT3,PAT4,PAT5,DATAMM)]
# Ischemic_stroke_excl_SDO<-Ischemic_stroke_excl[Table_cdm=="SDO",.(ID_SKNO_ARSNEW,ID,DIADIM,PAT1,PAT2,PAT3,PAT4,PAT5,DATAMM)]
# 
# prova<-merge(Ischemic_stroke_SDO,Ischemic_stroke_excl_SDO,by="ID_SKNO_ARSNEW",all = F)
# id_to_remove<-prova[,"ID_SKNO_ARSNEW"]
# id_to_remove<-unlist(id_to_remove)
# 
# Ischemic_stroke<-Ischemic_stroke[Ischemic_stroke$ID_SKNO_ARSNEW %!in% id_to_remove,]
# save(Ischemic_stroke,file=paste0(dirtemp,"Ischemic_stroke.RData"))
# rm(Ischemic_stroke_SDO,Ischemic_stroke_excl,Ischemic_stroke_excl_SDO)
