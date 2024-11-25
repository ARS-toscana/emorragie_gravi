
# author: Rosa Gini

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_createconceptsetdatasets"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

#APPLY THE FUNCTION CreateConceptSetDatasets TO CREATE ONE DATASET PER CONCEPT SET CONTAINING ONLY THE CODES OF INTEREST


CreateConceptSetDatasets(dataset = TheShinISS_CDM_tables,
                         codvar = TheShinISS_CDM_codvar,
                         datevar = TheShinISS_CDM_datevar,
                         dateformat = "YYYYmmdd",
                         rename_col = list(ID=ID,DATE=DATE),
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_names = concept_sets_of_our_study_drugs,
                         dirinput = thisdirinput,
                         diroutput = thisdiroutput,
                         extension = c("csv"))


CreateConceptSetDatasets(dataset = TheShinISS_CDM_tables,
                         codvar = TheShinISS_CDM_codvar,
                         datevar = TheShinISS_CDM_datevar,
                         dateformat = "YYYYmmdd",
                         rename_col = list(ID=ID,DATE=DATE),
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_names = concept_sets_of_our_study_diagnosis,
                         concept_set_codes_excl = concept_set_codes_our_study_excl,
                         dirinput = thisdirinput,
                         diroutput = thisdiroutput,
                         extension = c("csv"))


# CreateConceptSetDatasets(dataset = TheShinISS_CDM_tables,
#                          codvar = TheShinISS_CDM_codvar,
#                          datevar = TheShinISS_CDM_datevar,
#                          dateformat = "YYYYmmdd",
#                          rename_col = list(ID=ID),
#                          concept_set_domains = concept_set_domains,
#                          concept_set_codes =	concept_set_codes_our_study,
#                          concept_set_names = concept_sets_of_our_study_procedure,
#                          concept_set_codes_excl = concept_set_codes_our_study_excl,
#                          dirinput = thisdirinput,
#                          diroutput = thisdiroutput,
#                          extension = c("csv"))
# 
