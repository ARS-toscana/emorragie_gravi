# author: Rosa Gini

# v 1.0 3 Dec 2024

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
load(file.path(thisdirinput, "bleeding_possible.RData"))
source_population <- readRDS(file.path(thisdirinput, "D3_source_population.rds"))

# processing 

processing <- bleeding_narrow

processing[, event := "bleeding_narrow"]

processing <- rbind(processing, bleeding_possible, fill = T)

processing <- processing[is.na(event), event := "bleeding_possible"]


setnames(processing,c("ID","DATE"),c("person_id","date"))

processing <- processing[date >= baselinedate_components & date <= baselinedate_components + 365,]

processing[,component := paste0(event,"_",Table_cdm,"_",meaning)]

processing <- processing[,.(mindate_comp = min(date)),by = .(person_id,component)]

processing[, mindate := min(mindate_comp),by = .(person_id)]
processing[, numdayscomp :=  as.integer(mindate_comp - mindate)]

processing <- processing[numdayscomp <= 30,]

processing <- processing[,.(person_id,component)]

processing <- dcast(
  processing, 
  person_id ~ component, 
  fun.aggregate = length,  
  value.var = "component"
)

# processing[, (names(processing)[-1]) := lapply(.SD, function(x) as.integer(x > 0)), .SDcols = -"person_id"]


source_population <- source_population[entry_cohort<= baselinedate_components & baselinedate_components <= exit_cohort,]

source_population <- source_population[,.(person_id)]

processing <- merge(source_population,processing, all.x = T)

for (var in setdiff(names(processing),"person_id")) {
  processing <- processing[ is.na(get(var)), (var) := 0]
}

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
