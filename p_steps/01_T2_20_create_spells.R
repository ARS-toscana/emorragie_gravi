# author: Rosa Gini

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_createspells"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

ana <- fread(file.path(thisdirinput,"ANAGRAFE_ASSISTITI.csv"))


output_spells_category <- CreateSpells(
  dataset = ana,
  id = "id" ,
  start_date = "data_inizioass",
  end_date = "data_fineass",
  gap_allowed = 365
)

rm(ana)

# keep only the spells overlapping the study period
output_spells_category <- output_spells_category[study_start_date <= exit_spell_category & study_end_date >= entry_spell_category ,]
# keep only the spells long enough
output_spells_category <- output_spells_category[exit_spell_category >= entry_spell_category - 365,]

setorderv(output_spells_category, c("id", "entry_spell_category"))

processing <- output_spells_category[, is_the_study_spell := as.integer(seq(.N) == .N), by = id]

processing <- processing[is_the_study_spell == 1, ]

setnames(processing,old = c("id"),new = c("person_id"))


################################
# clean

tokeep <- c("person_id", "entry_spell_category","exit_spell_category","is_the_study_spell")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_clean_spells"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
