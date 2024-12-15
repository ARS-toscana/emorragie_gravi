# author: Rosa Gini

# v 1.0 10 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D6_Figure_components"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  thisdirfigure <- thisdiroutput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirfigure <- dirfig
}

# import

component_dataset <- readRDS(file.path(thisdirinput, "D4_bleeding_components_N.rds"))

# processing 


name_intermediate <- paste0(thisdiroutput,"/composites")
thisnameoutput <- paste0(thisdiroutput,"/data_Figure_components")
figure_name <- paste0("/Figure_components")


# "ER_NON_SEVERE"    "ER_SEVERE"        "HOSP_MAIN_MIPRES"
# "HOSP_MAIN_NOPRES" "HOSP_MAIN_PRES"   "HOSP_SEC_MIPRES" 
# "HOSP_SEC_NOPRES"  "HOSP_SEC_PRES" 

if (nrow(component_dataset > 0)) {
  output <- ApplyComponentStrategy(dataset = component_dataset,
                                   individual = F, ## F -> data counts
                                   intermediate_output = T,
                                   intermediate_output_name = name_intermediate,
                                   components = c("ER_SEVERE", # 1
                                                  "ER_NON_SEVERE", # 2
                                                  "HOSP_MAIN_PRES", # 3
                                                  "HOSP_MAIN_NOPRES", # 4
                                                  "HOSP_SEC_PRES", # 5
                                                  "HOSP_SEC_NOPRES" # 6
                                   ),
                                   labels_of_components = c("ER grave",  #1
                                                            "ER non grave",  #2
                                                            "Ric principale, PA", #3
                                                            "Ric principale, NPA",  #4
                                                            "Ric secondaria, PA", #5
                                                            "Ric sec, NPA"
                                   ),   
                                   composites_to_be_created = list(
                                     list(1, 2),          #7
                                     list(3, 4),          #8
                                     list(5, 6),    #9
                                     list(8, 9),    #10
                                     list(3, 5)    #11
                                   ),
                                   labels_of_composites_to_be_created = c(
                                     "ER (1 o 2)",         #7
                                     "Ricovero princ (3 o 4)",#8
                                     "Ricovero sec (5 o 6)",    #9
                                     "Ricovero (8 o 9)",    #10
                                     "Ricovero PA (3 o 5)"    #11
                                   ),
                                   pairs_to_be_compared=list(
                                     list(1,3), #12
                                     list(1,8), #13
                                     list(1,10), #14
                                     list(2,10), #15
                                     list(1,4), #16
                                     list(1,11) #17
                                   ),
                                   labels_of_pairs_to_be_compared = c(
                                     "ER grave vs Ric princ PA (1 vs 3)", #12
                                     "ER grave vs Ric princ (1 vs 8)", #13 
                                     "ER grave vs Ricovero (1 vs 10)", #14
                                    "ER non grave vs Ricovero (2 vs 10)", # 15
                                    "ER grave vs Ric sec NPA (1 vs 4)", # 16
                                    "ER grave vs Ric PA (1 or 11)" # 17
                                   ),
                                   figure_name = figure_name,
                                   K=1000,
                                   # strata = list("ds"),
                                   count_var = "N",
                                   figure = T,
                                   aggregate = F ,
                                   output_name = thisnameoutput,
                                   dirfigure = thisdirfigure
  )
  
  load(paste0(thisnameoutput,".RData"))
  processing <- get(paste0(thisnameoutput,".RData"))
  processing <- processing[,.(ord_alg, N_, PROP_, PROP_10, PROP_11, PROP_01)]
  processing[, N := as.character(N_)]
  processing <- processing[ N_ > 0 & N_ < 5, N := "< 5"]
  setnames(processing, "ord_alg", "Algorithm")
  for (var in c("PROP_", "PROP_10", "PROP_11", "PROP_01")) {
    processing[, (var) := round(get(var), 1)]
  }
  for (var in c("PROP_10", "PROP_11", "PROP_01")) {
    processing[1:10, (var) := NA]
  }
  
  processing[,PROP_L:=  PROP_10 + PROP_11]
  processing[,PROP_R:=  PROP_01 + PROP_11]
  
  processing <- processing[,.(Algorithm, N, PROP_, PROP_L, PROP_R, PROP_10, PROP_11, PROP_01)]
}else{
  processing <- data.table()
  processing[, Algorithm := ""]
  for (var in c("N", "PROP_", "PROP_L", "PROP_R", "PROP_10", "PROP_11", "PROP_01")) {
    processing[, (var) := NA_integer_ ]
  }
}
setnames(processing, c("PROP_", "PROP_L", "PROP_R", "PROP_10", "PROP_11", "PROP_01"), c("Total in the algorithm (left-hand OR right-hand)", "Left-hand","Right-hand", "Unique contribution of the left-hand component (left-hand AND NOT right-hand)", "Overlap of both components (left-hand AND right-hand)", "Unique contribution of the right-hand component (right-hand AND NOT left-hand)"))



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

# outputfile <- processing
# 
# nameoutput <- "D3_bleeding_components"
# nameoutputext <- paste0(nameoutput,".rds")
# assign(nameoutput, outputfile)
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
# 
# 
