# author: Rosa Gini

# v 1.3 15 Dec 2024

# restrict to persons_with_MoI, extend to all bleedings since 2015, set bleeding to narrow if boith types on the same day, remove bleedings with a beeding less than 30 days during lookback

# v 1.2 10 Dec 2024

# update definition of narrow based on component strategy

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_bleeding_events"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# import

load(file.path(thisdirinput, "bleeding_narrow.RData"))
setnames(bleeding_narrow,c("ID","DATE"),c("person_id","date"))

persons_with_MoI <- readRDS(file.path(thisdirinput, "D4_persons_with_MoI.rds"))

# processing

processing <- persons_with_MoI

# select only bleedings in persons_with_MoI

processing <- merge(processing, bleeding_narrow, by= "person_id",all = F)

# label bleedings

processing <- processing[(meaning == "hospital_main_diagnosis") | (meaning == "emergency_room_diagnosis" & esito %in% list_outcomesER_severe ), event := "bleeding_narrow"]
processing <- processing[is.na(event), event := "bleeding_possible"]

# set date of bleeding as narrow if there are two different labellings

processing <- processing[, .(event = min(event)), by = c("person_id","date")]

# remove bleedings having another bleeding in the lookback of 30 days

temp <- copy(processing)[,.(person_id,date)]

setnames(temp,"date","past_date")

temp <- merge(processing, temp, by = "person_id", allow.cartesian = T, all = F)

temp <- temp[past_date < date & past_date >= date - 30,] 

temp <- unique(temp[,.(person_id,date)])

temp[, todrop := 1]

processing <- merge(processing,temp, by = c("person_id","date"), all.x = T)

processing <- processing[is.na(todrop),]

processing[, todrop := NULL]
################################
# clean

tokeep <- c("person_id","date","event")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id","date")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_bleeding_events"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
