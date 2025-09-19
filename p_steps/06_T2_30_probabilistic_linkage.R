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

# v 0.3 02 Sep 2025 - Refining record linkage:
# - focus only on narrow bleedings;
# - extend the linkage to those bleedings with duplicated dates;
# - implement a probabilistic linkage to solve remaining ambiguities after the deterministic one already implemented;


#############################

if (TEST){
  testname <- "test_D3_prob_linkage"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# load data
D3_dispensings_DOACs <- readRDS(file.path(thisdirinput, "D3_dispensings_DOACs.rds"))
D3_dispensings_AA <- readRDS(file.path(thisdirinput, "D3_dispensings_AA.rds"))
D3_study_population <- readRDS(file.path(thisdirinput, "D3_study_population.rds"))

## 0) extract dosing of DOAcs ----

# aic
dose_20_rivarox <- c("038744189", "049224328", "049651058", "049674284")
dose_15_rivarox <- c("038744138", "049224239", "049651045", "049674221")

processing <- D3_dispensings_DOACs

# keep only rivaroxaban 
# processing <- D3_dispensings_DOACs[label==1 ,]

# merge D3_dispensings_DOACs with D3_study_population by person_id
processing <- processing[D3_study_population, on = "person_id"]

# keep rows where date of DOAC <= date_bleeding
processing <- processing[date <= date_bleeding, ]

# keep the maximum date of DOAC per each person_id and date_bleeding
processing <- processing[, .SD[date == max(date)], by = .(person_id, date_bleeding)]

# categorize dosing based on aic
processing <- processing[, `:=`(type_dose_15 = fifelse(AIC %in% dose_15_rivarox, "high", "other"),
                                type_dose_15_20 = fifelse(AIC %in% c(dose_15_rivarox, dose_20_rivarox), "high", "other"))]

tokeep <- c("person_id", "date_bleeding", "type_dose_15", "type_dose_15_20")

processing <- processing[, ..tokeep]

# add info on dosing in D3_study_population
D3_study_population <- processing[D3_study_population, on = c("person_id", "date_bleeding")]

D3_study_population <- D3_study_population[!duplicated(person_id) ,]

D3_study_population[, `:=`(type_dose_15 = fifelse(type_dose_15 == "high", type_dose_15, "other"),
                           type_dose_15_20 = fifelse(type_dose_15_20 == "high", type_dose_15_20, "other"))]


## 1) matching naive: rows with negative values for dispensings/vials removed ----

descriptive_fes_pre_management <- list(
  
  "Distribution of number of negative vials in fes" = D3_dispensings_AA[Nvialsday<0, .(N = .N), by = Nvialsday]
  
)

saveRDS(descriptive_fes_pre_management, file = file.path(thisdiroutput,"descriptive_fes_pre_management.rds"))

# create id_bleeding
D3_study_population <- D3_study_population[, id_bleeding:= 1:.N]

# remove negative records
D3_dispensings_AA <- D3_dispensings_AA[Nvialsday>0 & Ndispday>0]

# keep only narrow bleedings
D3_study_population <- D3_study_population[event=="bleeding_narrow" ,]

# categorize dosing
D3_dispensings_AA[, type_dose:= fifelse(Nvialsday==9, "high", "other")]

# reshape dataset with bleedings with unique rows per date of bleeding
counts <- D3_study_population[,.(N = .N), by = date_bleeding]

# keep only bleedings with unique dates 
D3_study_population_u <- D3_study_population[date_bleeding %in% counts[N == 1, date_bleeding]]

# keep only bleedings with duplicated dates
D3_study_population_d <- D3_study_population[!date_bleeding %in% counts[N == 1, date_bleeding]]

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
match_exact <- match_exact[, `:=`(quality_matching = "1° round")]

match_2_round <- match_2_round[, `:=`(quality_matching = fifelse(!is.na(data) & !is.na(date_bleeding), "2° round", "No matching"))]

# bind ids rows with 1°round matching and those with 2° round matching
match_df <- rbind(match_exact[, .(person_id, event, quality_matching, type_dose, type_dose_15, type_dose_15_20, data, id_dispensing, id_bleeding, Nvialsday)], match_2_round[, .(person_id, event, type_dose, type_dose_15, type_dose_15_20, quality_matching, data, id_dispensing, id_bleeding, Nvialsday)])

# create exposure variable
match_df <- match_df[, is_exposed := fifelse(quality_matching %in% c("1° round", "2° round"), 1, 0)]

ids_dispensing_matched <- match_df[is_exposed==1, id_dispensing]

# isolate dispensings in fes still not associated with any bleedings
D3_dispensings_AA_s <- D3_dispensings_AA[!id_dispensing %in% ids_dispensing_matched, ]

# handle bleedings with duplicated dates: count nrows by date in both dataset and proceed with a deterministic linkage in case nrow is equal
count_dates_bleeding <- D3_study_population_d[, .(count_a = .N), by = date_bleeding]
count_dates_fes <- D3_dispensings_AA_s[, .(count_b = .N), by = data]
count_merged <- merge.data.table(count_dates_bleeding, count_dates_fes, by.x = "date_bleeding", by.y = "data", all = TRUE)
dates_dup_matched <- count_merged[count_a==count_b, date_bleeding]


if(length(dates_dup_matched) > 0) {
  
  match_dup_dates <- D3_dispensings_AA_s[D3_study_population_d, on = .(data = date_bleeding), nomatch = 0L][data %in% dates_dup_matched]
  match_dup_dates <- match_dup_dates[, quality_matching := "3° round"]
  
  # bind ids rows with 1°round matching and those with 2° round matching
  match_df <- rbind(match_df[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)], match_dup_dates[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)])
  
  
}

# isolate records in both dataset that are still vacant
D3_study_population_d2 <- D3_study_population_d[!date_bleeding %in% dates_dup_matched, ]
D3_dispensings_AA_s2 <- D3_dispensings_AA_s[!data %in% dates_dup_matched, ]

# allow a tolerance of 1 day and count again duplications per data/data+1
D3_dispensings_AA_s2 <- D3_dispensings_AA_s2[, range_dates := paste0(data_min, "_", data_max)]
D3_study_population_d2 <- D3_study_population_d2[, range_dates := paste0(date_bleeding, "_", date_bleeding+1)]
count_range_dates_bleeding <- D3_study_population_d2[, .(count_a = .N), by = range_dates]
count_range_dates_fes <- D3_dispensings_AA_s2[, .(count_b = .N), by = range_dates]
count_range_merged <- merge.data.table(count_range_dates_bleeding, count_range_dates_fes, by = "range_dates", all = TRUE)
range_dates_dup_matched <- count_range_merged[count_a==count_b, range_dates]

if(length(range_dates_dup_matched) > 0) {
  
  match_range_dup_dates <- D3_dispensings_AA_s2[D3_study_population_d2, on = .(range_dates), nomatch = 0L][range_dates %in% range_dates_dup_matched]
  match_range_dup_dates <- match_range_dup_dates[, quality_matching := "4° round"]
  
  # bind ids rows with 1°round matching and those with 2° round matching
  match_df <- rbind(match_df[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)], match_range_dup_dates[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)])
  
}

# create exposure variable
match_df <- match_df[, is_exposed := fifelse(quality_matching %in% c("1° round", "2° round", "3° round", "4° round"), 1, 0)]

# matching using dosing of DOACs and AA

# isolate dispensings in fes still not associated with any bleedings
ids_dispensing_matched <- match_df[is_exposed==1, id_dispensing]
D3_dispensings_AA_s <- D3_dispensings_AA[!id_dispensing %in% ids_dispensing_matched, ]

# isolate bleedings still not associated with any dispensings
ids_bleedings_matched <- match_df[is_exposed==1, id_bleeding]
D3_study_population_s <- D3_study_population[!id_bleeding %in% ids_bleedings_matched, ]

# filter only high dose
D3_dispensings_AA_s <- D3_dispensings_AA_s[type_dose == "high" ,]
D3_study_population_s <- D3_study_population_s[type_dose_15 == "high" ,]
D3_study_population_sbis <- D3_study_population_s[type_dose_15_20 == "high" ,]

# considering high dose of DOACs only 15 mg
# match by date and type and dose and eventually handle bleedings with duplicated dates and dose: count nrows by date and dose in both dataset and proceed with a deterministic linkage in case nrow is equal

count_dates_bleeding <- D3_study_population_s[, .(count_a = .N), by = .(date_bleeding, type_dose_15)]
count_dates_fes <- D3_dispensings_AA_s[, .(count_b = .N), by = .(data, type_dose)]
count_merged <- merge.data.table(count_dates_bleeding, count_dates_fes, by.x = "date_bleeding", by.y = "data", all = TRUE)
dates_dup_matched <- count_merged[count_a==count_b, date_bleeding]


if(length(dates_dup_matched) > 0) {
  
  match_dup_dates <- D3_dispensings_AA_s[D3_study_population_s, on = .(data = date_bleeding, type_dose = type_dose_15), nomatch = 0L][data %in% dates_dup_matched]
  match_dup_dates <- match_dup_dates[, quality_matching := "5° round"]
  
  # bind ids rows with 1°round matching and those with 2° round matching
  match_df <- rbind(match_df[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)], match_dup_dates[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)])
  
  
}

# add tolerance window of 1 day while matching by date and DOSE

# isolate records in both dataset that are still vacant
D3_study_population_s2 <- D3_study_population_s[!date_bleeding %in% dates_dup_matched, ]
D3_dispensings_AA_s2 <- D3_dispensings_AA_s[!data %in% dates_dup_matched, ]

# allow a tolerance of 1 day and count again duplications per data/data+1
D3_dispensings_AA_s2 <- D3_dispensings_AA_s2[, range_dates := paste0(data_min, "_", data_max)]
D3_study_population_s2 <- D3_study_population_s2[, range_dates := paste0(date_bleeding, "_", date_bleeding+1)]
count_range_dates_bleeding <- D3_study_population_s2[, .(count_a = .N), by = range_dates]
count_range_dates_fes <- D3_dispensings_AA_s2[, .(count_b = .N), by = range_dates]
count_range_merged <- merge.data.table(count_range_dates_bleeding, count_range_dates_fes, by = "range_dates", all = TRUE)
range_dates_dup_matched <- count_range_merged[count_a==count_b, range_dates]

if(length(range_dates_dup_matched) > 0) {
  
  match_range_dup_dates <- D3_dispensings_AA_s2[D3_study_population_s2, on = .(range_dates), nomatch = 0L][range_dates %in% range_dates_dup_matched]
  match_range_dup_dates <- match_range_dup_dates[, quality_matching := "6° round"]
  
  # bind ids rows with 1°round matching and those with 2° round matching
  match_df <- rbind(match_df[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)], match_range_dup_dates[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)])
  
}

# considering high dose of DOACs both 15 and 20 mg
# match by date and type and dose and eventually handle bleedings with duplicated dates and dose: count nrows by date and dose in both dataset and proceed with a deterministic linkage in case nrow is equal

count_dates_bleeding <- D3_study_population_sbis[, .(count_a = .N), by = .(date_bleeding, type_dose_15_20)]
count_dates_fes <- D3_dispensings_AA_s[, .(count_b = .N), by = .(data, type_dose)]
count_merged <- merge.data.table(count_dates_bleeding, count_dates_fes, by.x = "date_bleeding", by.y = "data", all = TRUE)
dates_dup_matched <- count_merged[count_a==count_b, date_bleeding]


if(length(dates_dup_matched) > 0) {
  
  match_dup_dates <- D3_dispensings_AA_s[D3_study_population_sbis, on = .(data = date_bleeding, type_dose = type_dose_15_20), nomatch = 0L][data %in% dates_dup_matched]
  match_dup_dates <- match_dup_dates[, quality_matching := "5° round (b)"]
  
  # bind ids rows with 1°round matching and those with 2° round matching
  match_df <- rbind(match_df[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)], match_dup_dates[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)])
  
  
}

# add tolerance window of 1 day while matching by date and DOSE

# isolate records in both dataset that are still vacant
D3_study_population_s2 <- D3_study_population_sbis[!date_bleeding %in% dates_dup_matched, ]
D3_dispensings_AA_s2 <- D3_dispensings_AA_s[!data %in% dates_dup_matched, ]

# allow a tolerance of 1 day and count again duplications per data/data+1
D3_dispensings_AA_s2 <- D3_dispensings_AA_s2[, range_dates := paste0(data_min, "_", data_max)]
D3_study_population_s2 <- D3_study_population_s2[, range_dates := paste0(date_bleeding, "_", date_bleeding+1)]
count_range_dates_bleeding <- D3_study_population_s2[, .(count_a = .N), by = range_dates]
count_range_dates_fes <- D3_dispensings_AA_s2[, .(count_b = .N), by = range_dates]
count_range_merged <- merge.data.table(count_range_dates_bleeding, count_range_dates_fes, by = "range_dates", all = TRUE)
range_dates_dup_matched <- count_range_merged[count_a==count_b, range_dates]

if(length(range_dates_dup_matched) > 0) {
  
  match_range_dup_dates <- D3_dispensings_AA_s2[D3_study_population_s2, on = .(range_dates), nomatch = 0L][range_dates %in% range_dates_dup_matched]
  match_range_dup_dates <- match_range_dup_dates[, quality_matching := "6° round (b)"]
  
  # bind ids rows with 1°round matching and those with 2° round matching
  match_df <- rbind(match_df[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)], match_range_dup_dates[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)])
  
}


# create exposure variable
match_df <- match_df[, is_exposed := fifelse(quality_matching %in% c("1° round", "2° round", "3° round", "4° round", "5° round", "6° round", "5° round (b)", "6° round (b)"), 1, 0)]



# 0) descriptive table general

bleeding_counts <- D3_study_population[!is.na(date_bleeding), .N, by = date_bleeding]
setorder(bleeding_counts, N)

dispensing_counts <- D3_dispensings_AA[!is.na(data), .N, by = data]
setorder(dispensing_counts, N)


descriptive_table <- list(
  
  "Number of rows of D3_study_population dataset (narrow bleeding as unit)" = nrow(D3_study_population),
  "Number of rows of dispensing_AA dataset (dispensing day and hospital as unit)" = nrow(D3_dispensings_AA),
  "Number of bleeding dates in D3_study_population by n of duplication" = bleeding_counts[, .N, by = N],
  "Number of rows of D3_study_population dataset with unique bleedings" = nrow(D3_study_population_u),
  "Number of rows of the matched dataset after 1 round of matching (exact match)" = nrow(match_exact),
  "Number of rows of the matched dataset after 2 round of matching (1 not included)" = nrow(match_2_round),
  "Number of unique dispensing dates" = D3_dispensings_AA[, uniqueN(data)],
  "Number of dispensing dates in D3_dispensings_AA by n of duplication" = dispensing_counts[, .N, by = N],
  "Number of rows of the matched dataset (inner join at 1 round and full join at 2 round)" = nrow(match_df)
  
)


# 1) descriptive table bleedings

today <- format(Sys.Date(), "%Y%m%d")

tab <- match_df[!is.na(event) & !duplicated(person_id)]

tbl1 <- tab |> tbl_summary(include = is_exposed) |>
       as_gt() |> 
       gt::gtsave(filename = file.path(thisdiroutput,paste0("tbl_all", today, ".docx")))

tab_exposed <- tab[is_exposed==1 ,]

tbl2 <- tab_exposed |> tbl_summary(include = quality_matching) |>
               as_gt() |> 
               gt::gtsave(filename = file.path(thisdiroutput,paste0("tbl_exposed", today, ".docx")))

# 2) descriptive table fes

dispensing_summary <- match_df[!is.na(data), .(n_bleedings = if (all(is.na(event))) 0L else .N), by = id_dispensing]

setorder(dispensing_summary, n_bleedings)

descriptive_table[["Dispensing in fes categorized per number of matches"]] = dispensing_summary[, .N, by = .(n_bleedings)]

setorder(D3_dispensings_AA, Nvialsday)

descriptive_table[["Distribution of Nvials per dispensing in fes"]] = D3_dispensings_AA[, .N, by = .(Nvialsday)]

descriptive_table[["Distribution of categorized dose of AA in fes"]] = D3_dispensings_AA[, .N, by = .(type_dose)]

descriptive_table[["Distribution of categorized dose of DOACs, considering high dose only 15 mg"]] = D3_study_population[, .N, by = .(type_dose_15)]

descriptive_table[["Distribution of categorized dose of DOACs, considering high dose both 15 and 20 mg"]] = D3_study_population[, .N, by = .(type_dose_15_20)]

# descriptive_table

saveRDS(descriptive_table, file = file.path(thisdiroutput,"descriptive_post_prob_linkage.rds"))


## 2) matching keeping negative values for dispensings/vials ----

