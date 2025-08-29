# if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt")
# if (!requireNamespace("gtsummary", quietly = TRUE)) install.packages("gtsummary")
# library(gt)
# library(gtsummary)

# Probabilistic linkage ----

# authors: Sabrina Giometto

# v 0.1 13 Aug 2025 - First attempt of probabilistic linkage after removing rows in fes with negative Nvialsday & Ndispday

# v 0.2 27 Aug 2025 - Refining record linkage: 
# - reduce tolerance window of 1 day to only one direction (fes date after bleeding date);
# - remove rows with exact match (1 to 1) and then move to second round matching (with tolerance);
# - print new descriptive tables with more details and stratified by type of bleeding;

#############################

if (TEST){
  testname <- "test_D3_dispensings_AA"
  thisdirinput <- file.path(dirtest,testname,"g_output")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# when running on real data
D3_dispensings_AA <- readRDS(file.path(thisdirinput, "D3_dispensings_AA.rds"))
D3_study_population <- readRDS(file.path(thisdirinput, "D3_study_population.rds"))


## 1) matching naive: rows with negative values for dispensings/vials removed ----

# remove negative records
D3_dispensings_AA <- D3_dispensings_AA[Nvialsday>0 & Ndispday>0]

# reshape dataset with bleedings with unique rows per date of bleeding
counts <- D3_study_population[,.(N = .N), by = date_bleeding]

#?? in caso di date di bleeding coincidenti, quale tengo (e quindi coi caratterizzo) e quale butto? Tutte. Tengo solo quelle non ripetute
D3_study_population_u <- D3_study_population[date_bleeding %in% counts[N == 1, date_bleeding]]

#?? quindi butto anche righe di fes con date coincidenti? NO(?!)
# D3_dispensings_AA_u <- D3_dispensings_AA[!duplicated(data) ,]

# create id dispensing
D3_dispensings_AA <- D3_dispensings_AA[, id_dispensing:= 1:.N]

# exact matching
match_exact <- D3_dispensings_AA[D3_study_population_u, on = .(data = date_bleeding), nomatch = 0L]

# create 1° and 2° round matching by excluding those matched 1 to 1 with the exact matching
ids <- match_exact[, person_id]

# extract ids to be matched at 2° round
D3_study_population_u2 <- D3_study_population_u[!person_id %in% ids ,]

# perform 2° round matching with 1 day of tolerance (only in one direction)
tolerance <- 1

D3_dispensings_AA <- D3_dispensings_AA[, `:=`(data_min = data - tolerance,
                                                  data_max = data)]

# create artificial start and end dates equal to the bleeding date
D3_study_population_u2[, `:=`(date_start = date_bleeding, date_end = date_bleeding)]

setkey(D3_study_population_u2, date_start, date_end)
setkey(D3_dispensings_AA, data_min, data_max)

# full join non-equi
left_bleed  <- foverlaps(D3_study_population_u2, D3_dispensings_AA,
                         by.x = c("date_start", "date_end"),
                         by.y = c("data_min", "data_max"),
                         type = "within", nomatch = NA)

left_disp <- foverlaps(D3_dispensings_AA, D3_study_population_u2,
                       by.x = c("data_min", "data_max"),
                       by.y = c("date_start", "date_end"),
                       type = "any", nomatch = NA)

bleed_only <- left_bleed %>% filter(is.na(data))
disp_only <- left_disp %>% filter(is.na(date_bleeding))
matched <- left_bleed %>% filter(!is.na(data))
# matched_check <- left_disp %>% filter(!is.na(date_bleeding))

# unisci i due dataset con rbind, eliminando duplicati
match_2_round <- rbindlist(list(bleed_only, disp_only, matched), fill=TRUE)

match_2_round[, c("date_start", "date_end", "data_min", "data_max") := NULL]

match_2_round <- match_2_round %>% 
  relocate(person_id, .before = "data") %>% 
  relocate(date_bleeding, .after = "person_id") 

# create tag variables to identify matching quality
match_exact <- match_exact[, `:=`(is_1_match = 1,
                                  is_2_match = 0)]

match_2_round <- match_2_round[, `:=`(is_1_match = 0,
                                      is_2_match = fifelse(!is.na(data) & !is.na(date_bleeding), 1, 0))]

# bind ids rows with 1°round matching and those with 2° round matching
match_df <- rbind(match_exact[, .(person_id, event, is_1_match, is_2_match, data, id_dispensing, Nvialsday)], match_2_round[, .(person_id, event, is_1_match, is_2_match, data, id_dispensing, Nvialsday)])

# create exposure variable
match_df <- match_df[, is_exposed := fifelse(is_1_match == 1 | is_2_match == 1, 1, 0)]

# 0) descriptive table general

D3_dispensings_AA[, tag := NA]
D3_dispensings_AA[!is.na(data), tag := duplicated(data)]

D3_study_population[, tag := NA]
D3_study_population[!is.na(date_bleeding), tag := duplicated(person_id, date_bleeding)]


descriptive_table <- list(
  
  "Number of rows of D3_study_population dataset (bleeding as unit)" = nrow(D3_study_population),
  "Number of rows of dispensing_AA dataset (dispensing day and hospital as unit)" = nrow(D3_dispensings_AA),
  "Number of bleeding dates replicated in D3_study_population" = D3_study_population[, .N, by = tag],
  "Number of unique dispensing dates" = D3_dispensings_AA[, uniqueN(data)],
  "Number of dispensing dates replicated in D3_dispensings_AA" = D3_dispensings_AA[, .N, by = tag],
  "Number of rows of the matched dataset (inner join at 1 round and full join at 2 round)" = nrow(match_df)
  
)


# 1) descriptive table bleedings

today <- format(Sys.Date(), "%Y%m%d")

tab <- match_df[!is.na(event) & !duplicated(person_id)]

tbl1 <- tab |> tbl_summary(by = event, include = is_exposed) |>
       add_overall() |> 
       as_gt() |> 
       gt::gtsave(filename = file.path(thisdiroutput,paste0("tbl_all", today, ".docx")))

tab_exposed <- tab[is_exposed==1 ,]

tbl2 <- tab_exposed |> tbl_summary(by = event, include = c(is_1_match, is_2_match)) |>
               add_overall() |> 
               as_gt() |> 
               gt::gtsave(filename = file.path(thisdiroutput,paste0("tbl_exposed", today, ".docx")))

# 2) descriptive table fes

dispensing_summary <- match_df[!is.na(data), .(n_bleedings = if (all(is.na(event))) 0L else .N), by = id_dispensing]

setorder(dispensing_summary, n_bleedings)

descriptive_table[["Dispensing in fes categorized per number of matches"]] = dispensing_summary[, .N, by = .(n_bleedings)]

setorder(D3_dispensings_AA, Nvialsday)

descriptive_table[["Distribution of Nvials per dispensing in fes"]] = D3_dispensings_AA[, .N, by = .(Nvialsday)]

# descriptive_table

saveRDS(descriptive_table, file = file.path(thisdiroutput,"descriptive_post_prob_linkage.rds"))


## 2) matching keeping negative values for dispensings/vials ----
