# EMORRAGIE GRAVI

# author: Rosa Gini

# v 0.5 25 Nov 2024

# first version for test


rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####################
# load parameters
source(paste0(thisdir,"/p_parameters/1_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/2_parameters_CDM.R"))
source(paste0(thisdir,"/p_parameters/3_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/6_parameters_study.R"))

TEST <- T


######################################
# run scripts
source(paste0(thisdir,"/p_steps/01_T2_10_create_conceptsets.R"))
source(paste0(thisdir,"/p_steps/01_T2_20_create_spells.R"))

