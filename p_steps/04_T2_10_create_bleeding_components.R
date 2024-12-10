# author: Rosa Gini

# v 1.0 10 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_bleeding_components"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import

load(file.path(thisdirinput, "bleeding_narrow.RData"))
source_population <- readRDS(file.path(thisdirinput, "D3_source_population.rds"))

# processing 

processing <- bleeding_narrow


setnames(processing,c("ID","DATE"),c("person_id","date"))

processing <- processing[date >= baselinedate_components & date <= baselinedate_components + 365,]

processing <- processing[ meaning == "emergency_room_diagnosis" & esito %in% list_outcomesER_severe ,component := "ER_SEVERE"]

processing <- processing[is.na(component) & meaning == "emergency_room_diagnosis",component := "ER_NON_SEVERE"]

processing <- processing[is.na(component) &  meaning == "hospital_main_diagnosis" & pres == 0 ,component := "HOSP_MAIN_NOPRES"]

processing <- processing[is.na(component) &  meaning == "hospital_main_diagnosis"  ,component := "HOSP_MAIN_PRES"]

# processing <- processing[is.na(component) &  meaning == "hospital_main_diagnosis" ,component := "HOSP_MAIN_MIPRES"]

processing <- processing[is.na(component) &  meaning == "hospital_sec_diagnosis" & pres == 0 ,component := "HOSP_SEC_NOPRES"]

processing <- processing[is.na(component) &  meaning == "hospital_sec_diagnosis" ,component := "HOSP_SEC_PRES"]

# processing <- processing[is.na(component) &  meaning == "hospital_sec" ,component := "HOSP_SEC_MIPRES"]


processing <- processing[,.(mindate_comp = min(date)),by = .(person_id,component)]

# keep only the components belonging to the same episode of bleeding, i.e., within 30 days from the inception of the episode
processing[, mindate := min(mindate_comp),by = .(person_id)]
processing[, numdayscomp :=  as.integer(mindate_comp - mindate)]
processing <- processing[numdayscomp <= 30,]

# keep all components per person
processing <- processing[,.(person_id,component)]

# reshape wide
processing <- dcast(
  processing, 
  person_id ~ component, 
  fun.aggregate = length,  
  value.var = "component"
)

# merge with source population included in the cohort on the date baselinedate_components

source_population <- source_population[entry_cohort<= baselinedate_components & baselinedate_components <= exit_cohort,]

source_population <- source_population[,.(person_id)]

processing <- merge(source_population,processing, all.x = T)

# set to 0 componnets that are not observed

for (var in setdiff(names(processing),"person_id")) {
  processing <- processing[ is.na(get(var)), (var) := 0]
}

# aggregate and create D4

aggregate <- processing[, .N, by = setdiff(names(processing),"person_id") ]


################################
# clean

# tokeep <- c("person_id","date","event")
# 
# processing <- processing[, ..tokeep]
# 
# setorderv(
#   processing,
#    c("person_id","date")
# )
# 

#########################################
# save

outputfile <- processing

nameoutput <- "D3_bleeding_components"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))


outputfile <- aggregate

nameoutput <- "D4_bleeding_components_N"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
