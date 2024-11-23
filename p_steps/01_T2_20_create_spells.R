#start from the table ANAFULL containing many records per subject and compute the spells (continuous periods in which the subject was resident in Tuscany with at most 21 days of distance )

ANAFULL <- as.data.table(read_dta(paste0(dirinput,"/ANAFULL.dta")))


output_spells_category <- CreateSpells(
  dataset=ANAFULL,
  id="IDUNI" ,
  start_date = "INI_RECORD",
  end_date = "FINE",
  gap_allowed=21
)

rm(ANAFULL)

output_spells_category<-output_spells_category[study_start %between% list(entry_spell_category,exit_spell_category ),]

setnames(output_spells_category,old = c("IDUNI"),new = c("ID"))
save(output_spells_category,file=paste0(dirtemp,"output_spells_category.RData"))
