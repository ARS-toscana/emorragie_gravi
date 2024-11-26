# EMORRAGIE GRAVI

# author: Rosa Gini

# v 0.6 25 Nov 2024

# complete for test Figure 1

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

#

######################################
# run scripts
source(paste0(thisdir,"/p_steps/01_T2_10_create_conceptsets.R"))
source(paste0(thisdir,"/p_steps/01_T2_20_create_spells.R"))
source(paste0(thisdir,"/p_steps/01_T2_30_create_persons.R"))
source(paste0(thisdir,"/p_steps/01_T2_50_clean_DOACs.R"))
source(paste0(thisdir,"/p_steps/01_T2_60_create_episodes_of_treatement.R"))
source(paste0(thisdir,"/p_steps/01_T2_70_create_bleeding_events.R"))
source(paste0(thisdir,"/p_steps/01_T2_80_selection_criteria_from_PERSON_to_persons_with_MoI.R"))
source(paste0(thisdir,"/p_steps/02_T3_10_select_persons_with_MoI.R"))
source(paste0(thisdir,"/p_steps/03_T3_10_create_source_population.R"))
source(paste0(thisdir,"/p_steps/07_T3_10_create_persontime_bleeding.R"))
source(paste0(thisdir,"/p_steps/07_T3_20_apply_Cube_to_persontime_bleeding.R"))
source(paste0(thisdir,"/p_steps/08_T4_30_create_D5_IR.R"))
source(paste0(thisdir,"/p_steps/08_T5_35_create_Figure_1.R"))

