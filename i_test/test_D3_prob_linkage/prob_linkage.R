if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer")
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")
library(officer)
library(flextable)

# Probabilistic linkage ----

if (TEST){
  testname <- "test_D3_dispensings_AA"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# when running on real data
D3_dispensings_AA <- readRDS(file.path(thisdirinput, "D3_dispensings_AA.rds"))
D3_study_population <- readRDS(file.path(thisdirinput, "D3_study_population.rds"))

# when running on simulated data
# D3_dispensings_AA <- readRDS(file.path(thisdirinput, "g_output", "D3_dispensings_AA.rds"))
# load simulated study_population (to be saved first, in simulate_input.R)
# D3_study_population <- readRDS(file.path(thisdirinput,"..", "test_D3_prob_linkage", "g_output", "D3_study_population.rds"))


## 1) matching naive: rows with negative values for dispensings/vials removed ----

# remove negative records
D3_dispensings_AA <- D3_dispensings_AA[Nvialsday>0 & Ndispday>0]

# initialize list including m datasets, one for each tolerance value
matched_data <- list()

# set tolerate gap in days
tolerance <- c(0:3)

for (i in seq_along(tolerance)) {
  
  # create time interval for dispensing dates
  D3_dispensings_AA_i <- copy(D3_dispensings_AA)
  D3_dispensings_AA_i <- D3_dispensings_AA_i[, `:=`(data_min = data - tolerance[i],
                                                    data_max = data + tolerance[i])]
  
  D3_dispensings_AA_i <- D3_dispensings_AA_i[, id_dispensing:= 1:.N]
  
  # create artificial start and end dates equal to the bleeding date
  # data_2 <- copy(D3_study_population)
  data_2 <- copy(D3_study_population)
  data_2[, `:=`(date_start = date_bleeding, date_end = date_bleeding)]
  
  setkey(data_2, date_start, date_end)
  setkey(D3_dispensings_AA_i, data_min, data_max)
  
  # full join non-equi
  left_bleed  <- foverlaps(data_2, D3_dispensings_AA_i,
                      by.x = c("date_start", "date_end"),
                      by.y = c("data_min", "data_max"),
                      type = "within", nomatch = NA)
  
  left_disp <- foverlaps(D3_dispensings_AA_i, data_2,
                              by.x = c("data_min", "data_max"),
                              by.y = c("date_start", "date_end"),
                              type = "any", nomatch = NA)
  
  bleed_only <- left_bleed %>% filter(is.na(data))
  disp_only <- left_disp %>% filter(is.na(date_bleeding))
  matched <- left_bleed %>% filter(!is.na(data))
  # matched_check <- left_disp %>% filter(!is.na(date_bleeding))
  
  # 3) unisci i due dataset con rbind, eliminando duplicati
  data_m <- rbindlist(list(bleed_only, disp_only, matched), fill=TRUE)
  
  data_m[, c("date_start", "date_end", "data_min", "data_max") := NULL]
  
  data_m <- data_m %>% 
    relocate(person_id, .before = "data") %>% 
    relocate(date_bleeding, .after = "person_id") %>% 
    rename(date_disp_AA = data)
  
  matched_data[[i]] <- data_m
  
  names(matched_data)[i] <- paste("tolerance", tolerance[i], sep = "_")
  
  
}


### descriptive table ----

D3_dispensings_AA[, tag := NA]
D3_dispensings_AA[!is.na(data), tag := duplicated(data)]

D3_study_population[, tag := NA]
D3_study_population[!is.na(date_bleeding), tag := duplicated(person_id, date_bleeding)]

descriptive_table <- list(
  
  "Number of rows of D3_study_population dataset (bleeding as unit)" = D3_study_population[,uniqueN(data.table(person_id, date_bleeding))],
  "Number of rows of dispensing_AA dataset (dispensing day and hospital as unit)" = nrow(D3_dispensings_AA),
  "Number of bleeding dates replicated in D3_study_population" = D3_study_population[, .N, by = tag],
  "Number of dispensing dates replicated in D3_dispensings_AA" = D3_dispensings_AA[, .N, by = tag]
  
)
  
for (i in seq_along(tolerance)) {
  
  bleed_summary <- matched_data[[i]][!is.na(person_id) & !is.na(date_bleeding), .(n_fes = if (all(is.na(date_disp_AA))) 0L else .N), by = .(person_id, date_bleeding)]

  setorder(bleed_summary, n_fes)   
  descriptive_table[[paste("Number of rows of matched dataset", "tolerance", tolerance[i], sep = "_")]] = nrow(matched_data[[i]])
  # descriptive_table[[paste("Number of IDs without a matched dispensing", "tolerance", tolerance[i], sep = "_")]] = matched_data[[i]][is.na(date_disp_AA)][!duplicated(person_id), .N]
  # descriptive_table[[paste("Number of IDs with a matched dispensing", "tolerance", tolerance[i], sep = "_")]] = matched_data[[i]][!is.na(date_disp_AA)][!duplicated(person_id), .N] 
  
  descriptive_table[[paste("Bleedings categorized per number of matches", "tolerance", tolerance[i], sep = "_")]] = bleed_summary[, .N, by = .(n_fes)]
  
  # non distinguo però il caso in cui ho una data di dispensazione ripetuta solo una volta o più
  dispensing_summary <- matched_data[[i]][!is.na(date_disp_AA), .(n_bleedings = if (all(is.na(date_bleeding))) 0L else uniqueN(data.table(person_id, date_bleeding))), by = .(date_disp_AA)]
  dispensing_summary <- matched_data[[i]][!is.na(date_disp_AA), .(n_bleedings = if (all(is.na(date_bleeding))) 0L else .N), by = .(id_dispensing)]
  
  setorder(dispensing_summary, n_bleedings)
  
  descriptive_table[[paste("Dispensing in fes categorized per number of matches", "tolerance", tolerance[i], sep = "_")]] = dispensing_summary[, .N, by = .(n_bleedings)]
  
  # matched_data[[i]][, tag := NA]
  # matched_data[[i]][!is.na(date_disp_AA), tag := duplicated(date_disp_AA)]
  # 
  # descriptive_table[[paste("Number of dispensing dates replicated in the matched dataset", "tolerance", tolerance[i], sep = "_")]] = matched_data[[i]][, .N, by = tag]
  

}

  
descriptive_table

# print table
doc <- read_docx()

for (i in names(descriptive_table)) {
     
      tmp <- descriptive_table[[i]]
      
      if (is.data.frame(tmp)) {
        
        df <- tmp
        if (ncol(df) == 1) names(df) <- "N"
        
      } else {
        
        df <- data.frame(N = tmp, check.names = FALSE)
        
      }
          
      doc <- doc %>%
            body_add_par(i, style = "heading 1") %>%
            body_add_flextable(flextable(df)) %>%
            body_add_par("", style = "Normal")
            
}

print(doc, target = file.path(thisdiroutput,"descriptive_post_prob_linkage.docx"))

## 2) matching keeping negative values for dispensings/vials ----
