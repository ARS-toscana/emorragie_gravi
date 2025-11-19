###################################################################
# DESCRIBE THE VARIABLES
###################################################################

# names of TD variables

codelists_variable_condition <- c("AMI","FAT","HF", "STROKE", "RENIMP", "HEPIMP","VE" , "DIAB","DEMENTIA","CANCER")
TD_variables_condition <- paste0("VAR_",codelists_variable_condition)

codelists_variable_medication <- c("ANTIAGGR", "NSAIDS", "INHIBVITK", "DOAC", "GASTRO", "STATINS", "ANTIDEPR", "HEPARIN", "ANTICANCER", "ANTIHYPERTEN", "ANTIDIAB" )
TD_variables_medication <- paste0("VAR_",codelists_variable_medication)

TD_variables <- c("VAR_bleeding_broad", TD_variables_condition,TD_variables_medication)

# labels of TD variables

name_variable <- list()
for (concept in c(codelists_variable_condition, codelists_variable_medication)) {
  name_variable[[paste0("VAR_",concept)]] <- name_codelist[[concept]]
}
name_variable[["VAR_bleeding_broad"]] <- "Sanguinamento (in senso esteso)"

# assign the codelists and time spans to those covariates that are computed via codelists

codelists_variable <- list()
for (concept in c(codelists_variable_condition, codelists_variable_medication)) {
  codelists_variable[[paste0("VAR_",concept)]] <- concept
}
timespan <- list()
for (concept in c(codelists_variable_medication)) {
  timespan[[paste0("VAR_",concept)]] <- 180
}