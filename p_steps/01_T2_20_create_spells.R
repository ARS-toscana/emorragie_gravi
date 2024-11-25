#start from the table ANAFULL containing many records per subject and compute the spells (continuous periods in which the subject was resident in Tuscany with at most 21 days of distance )

ana <- fread()


output_spells_category <- CreateSpells(
  dataset = ana,
  id = "ID" ,
  start_date = "INI_RECORD",
  end_date = "FINE",
  gap_allowed = 365
)

rm(ana)

output_spells_category<-output_spells_category[study_start %between% list(entry_spell_category,exit_spell_category ),]

setnames(output_spells_category,old = c("IDUNI"),new = c("ID"))
save(output_spells_category,file=paste0(dirtemp,"output_spells_category.RData"))
