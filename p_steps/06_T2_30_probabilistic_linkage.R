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

# aic_doacs <- read.xlsx("G:\\Il mio Drive\\Scuola specializzazione unipi\\Tesi\\AIC_duratadispensazione_SG.xlsx", sheet = 3)
# aic_doacs <- data.table(aic_doacs)
# 
# aic_doacs[, dose:= str_extract(Descrizione.prodotto, "(\\d+,?\\d*)(?=MG)")]
# 
# dose_20_rivarox <- aic_doacs[dose==20, Codice.prodotto]
# dose_15_rivarox <- aic_doacs[dose==15, Codice.prodotto]
# 
# aic_doacs <- read.xlsx("G:\\Il mio Drive\\Scuola specializzazione unipi\\Tesi\\AIC_duratadispensazione_SG.xlsx", sheet = 2)
# aic_doacs <- data.table(aic_doacs)
# 
# aic_doacs[, dose:= str_extract(Descrizione.prodotto, "(\\d+,?\\d*)(?=MG)")]
# 
# dose_5_apix <- aic_doacs[dose==5, Codice.prodotto]

# aic
dose_20_rivarox <- c(
  "038744189", "049224328", "049651058", "049674284", "038744177", "038744191", "038744203", "038744215", "038744241", "038744379",
  "038744393", "038744494", "045234073", "045562081", "045562093", "045562105", "045907197", "045907209", "045907211", "045907223",
  "045907235", "045907247", "046408148", "046408151", "046408163", "046408175", "046408187", "047059148", "047059151", "047059163",
  "047059175", "047059187", "048104071", "048104083", "048104145", "048158036", "048204059", "048343228", "048343230", "048343242",
  "048343255", "048343267", "048343279", "048343281", "048423180", "048423192", "048423204", "048423216", "048423228", "048423230",
  "048788107", "048788119", "048788121", "048788133", "048827253", "048827265", "048827277", "048827289", "048827291", "048827303",
  "048827315", "049197104", "049197116", "049201407", "049201419", "049201421", "049201433", "049201445", "049201458", "049201460",
  "049201472", "049201484", "049201496", "049201508", "049201510", "049201522", "049201534", "049224304", "049224316", "049224330",
  "049224342", "049224355", "049224367", "049224379", "049224381", "049224393", "049674258", "049674260", "049674272", "049674296",
  "049674308", "049674359", "049674361", "049781040", "049793413", "049793425", "049793437", "049793449", "049793452", "049793464",
  "049793476", "049793488", "049793490", "049793502", "049793514", "049793526", "049793538", "049793540", "049793565", "049793577",
  "049793589", "050000138", "050000140", "050000153", "050000165", "050000177", "050000189", "050000191", "050000203", "050000215",
  "050000227", "050000239", "050000241", "050009048", "050020041", "050157155", "050157167", "050157179", "050157181", "050157193",
  "050706047", "050765039", "050768098", "050768100", "050768112", "050768124", "050779178", "050779180", "050779192", "051139172",
  "051139184", "051139196", "051139208", "051139210"
)

dose_15_rivarox <- c(
  "038744138", "049224239", "049651045", "049674221", "038744114", "038744126", "038744140", "038744153", "038744165", "038744239",
  "038744367", "038744381", "038744482", "045234061", "045562042", "045562055", "045562067", "045562079", "045907110", "045907122",
  "045907134", "045907146", "045907159", "045907161", "045907173", "045907185", "046408086", "046408098", "046408100", "046408112",
  "046408124", "046408136", "047059086", "047059098", "047059100", "047059112", "047059124", "047059136", "048104057", "048104069",
  "048104133", "048158024", "048204046", "048343141", "048343154", "048343166", "048343178", "048343180", "048343192", "048343204",
  "048343216", "048423127", "048423139", "048423141", "048423154", "048423166", "048423178", "048788057", "048788069", "048788071",
  "048788083", "048788095", "048827188", "048827190", "048827202", "048827214", "048827226", "048827238", "048827240", "049197080",
  "049197092", "049201243", "049201256", "049201268", "049201270", "049201282", "049201294", "049201306", "049201318", "049201320",
  "049201332", "049201344", "049201357", "049201369", "049201371", "049201383", "049224191", "049224203", "049224215", "049224227",
  "049224241", "049224254", "049224266", "049224278", "049224280", "049224292", "049674171", "049674183", "049674195", "049674207",
  "049674219", "049674233", "049674245", "049781026", "049781038", "049793262", "049793274", "049793286", "049793298", "049793300",
  "049793312", "049793324", "049793336", "049793348", "049793351", "049793363", "049793375", "049793387", "049793399", "049793401",
  "050000013", "050000025", "050000037", "050000049", "050000052", "050000064", "050000076", "050000088", "050000090", "050000102",
  "050000114", "050000126", "050009036", "050009051", "050009063", "050020039", "050157104", "050157116", "050157128", "050157130",
  "050157142", "050706035", "050765027", "050768050", "050768062", "050768074", "050768086", "050779127", "050779139", "050779141",
  "050779154", "050779166", "051139119", "051139121", "051139133", "051139145", "051139158", "051139160"
)

dose_5_apix <- c(
  "041225095", "041225145", "041225069", "041225071", "041225083", "041225107", "041225119", "041225121", "047930058", "047930060",
  "047930072", "047930084", "048461141", "048461154", "048461166", "048461178", "048461180", "048461192", "048461204", "048461216",
  "048461228", "048461230", "048461242", "048461255", "048461267", "048461279", "048932216", "048932228", "048932230", "048932242",
  "048932255", "048932267", "048932279", "048932281", "048932293", "048932305", "048932317", "048932329", "048932331", "048932343",
  "048932356", "048932368", "048932370", "048932382", "048932394", "048932406", "048932444", "048932457", "048932469", "049518184",
  "049518196", "049518208", "049518210", "049518222", "049518234", "049518246", "049518259", "049518261", "049518273", "049518285",
  "049518297", "049518309", "049518311", "049518323", "049518335", "049518347", "049518350", "049518362", "049518386", "049566072",
  "049566084", "049566096", "049566108", "049566110", "049566122", "049566134", "049566146", "049600176", "049600188", "049600190",
  "049600202", "049600214", "049600226", "049600238", "049600240", "049600253", "049600265", "049600277", "049600289", "049600291",
  "049600303", "049600315", "049600327", "049600339", "049600341", "049600366", "049650043", "049650056", "049683194", "049683206",
  "049683218", "049683220", "049683232", "049683244", "049683257", "049683269", "049683271", "049683283", "049683295", "049683307",
  "049683319", "049683321", "049683333", "049683345", "049683358", "049683360", "049683372", "049855101", "049855113", "049855125",
  "049855137", "049855149", "049855152", "049855164", "049855176", "050301199", "050301201", "050301213", "050301225", "050301237",
  "050301249", "050301252", "050301264", "050301276", "050301288", "050301290", "050301302", "050301314", "050301326", "050301338",
  "050301340", "050301353", "050301365", "050319096", "050319108", "050319110", "050319122", "050319134", "050319146", "050319159",
  "050319161", "050319173", "050319247", "050319250", "050319262", "050319274", "050319286", "050319298", "050319425", "050512045",
  "050512058", "050512060", "050513047", "050513050", "050513062", "050990023", "051012060", "051012072", "051012084", "051012096",
  "051012108", "051012110", "051012122", "051037051", "051037063", "051037075", "051037087", "051255040", "051255053"
)



# dose_20_rivarox <- c("038744189", "049224328", "049651058", "049674284")
# dose_15_rivarox <- c("038744138", "049224239", "049651045", "049674221")

processing <- D3_dispensings_DOACs

# keep only rivaroxaban 
# processing <- D3_dispensings_DOACs[label==1 ,]

# merge D3_dispensings_DOACs with D3_study_population by person_id
processing <- processing[D3_study_population, on = "person_id"]

# keep rows where date of DOAC <= date_bleeding
processing <- processing[date <= date_bleeding, ]

# create sequence variable per id
setorder(processing, person_id, date)
processing[, seq:= 1:.N, by = person_id]

# keep the maximum date of DOAC per each person_id and date_bleeding
processing <- processing[, .SD[date == max(date)], by = .(person_id, date_bleeding)]

# categorize dosing based on aic
processing <- processing[, `:=`(type_dose_15 = fifelse((AIC %in% dose_15_rivarox) | (AIC %in% dose_5_apix & seq==1), "high", "low"),
                                type_dose_15_20 = fifelse((AIC %in% c(dose_15_rivarox, dose_20_rivarox)) | (AIC %in% dose_5_apix & seq==1), "high", "low"))]

tokeep <- c("person_id", "date_bleeding", "type_dose_15", "type_dose_15_20")

processing <- processing[, ..tokeep]

# add info on dosing in D3_study_population
D3_study_population <- processing[D3_study_population, on = c("person_id", "date_bleeding")]

D3_study_population[, `:=`(type_dose_15 = fifelse(type_dose_15 == "high", type_dose_15, "low"),
                           type_dose_15_20 = fifelse(type_dose_15_20 == "high", type_dose_15_20, "low"))]


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
D3_dispensings_AA[, type_dose:= fifelse(Nvialsday==9, "high", 
                                        fifelse(Nvialsday==5, "low", "other"))]

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
# D3_dispensings_AA_s <- D3_dispensings_AA_s[type_dose == "high" ,]
# D3_study_population_s <- D3_study_population_s[type_dose_15 == "high" ,]
# D3_study_population_sbis <- D3_study_population_s[type_dose_15_20 == "high" ,]

# considering high dose of DOACs only 15 mg
# match by date and type and dose and eventually handle bleedings with duplicated dates and dose: count nrows by date and dose in both dataset and proceed with a deterministic linkage in case nrow is equal

count_dates_bleeding <- D3_study_population_s[, .(count_a = .N), by = .(date_bleeding, type_dose_15)]
count_dates_fes <- D3_dispensings_AA_s[, .(count_b = .N), by = .(data, type_dose)]
count_merged <- merge.data.table(count_dates_bleeding, count_dates_fes, by.x = c("date_bleeding","type_dose_15"), by.y = c("data", "type_dose"), all = TRUE)
dates_dup_matched <- count_merged[count_a==count_b, date_bleeding]

################################

# define small function to keep matching variables in y

join_with_keys <- function(x, i, on, nomatch = 0L, filter_expr = NULL) {
  # eseguo la join
  res <- x[i, on = on, nomatch = nomatch]
  
  # se serve, applico un filtro
  if (!is.null(filter_expr)) {
    res <- res[eval(filter_expr)]
  }
  
  # ricreo le colonne di i (a destra dell'uguale in on)
  for (nm in names(on)) {
    res[[on[[nm]]]] <- res[[nm]]
  }
  
  return(res)
}

###############################


if(length(dates_dup_matched) > 0) {
  
  match_dup_dates <- join_with_keys(
    D3_dispensings_AA_s,
    D3_study_population_s,
    on = c("data" = "date_bleeding", "type_dose" = "type_dose_15"),
    nomatch = 0L,
    filter_expr = quote(data %in% dates_dup_matched)
  )
  
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
count_range_dates_bleeding <- D3_study_population_s2[, .(count_a = .N), by = .(range_dates, type_dose_15)]
count_range_dates_fes <- D3_dispensings_AA_s2[, .(count_b = .N), by = .(range_dates, type_dose)]
count_range_merged <- merge.data.table(count_range_dates_bleeding, count_range_dates_fes, by.x = c("range_dates", "type_dose_15"), by.y = c("range_dates", "type_dose"), all = TRUE)
range_dates_dup_matched <- count_range_merged[count_a==count_b, range_dates]

if(length(range_dates_dup_matched) > 0) {
  
  match_range_dup_dates <- join_with_keys(
    D3_dispensings_AA_s2,
    D3_study_population_s2,
    on = c("range_dates" = "range_dates", "type_dose" = "type_dose_15"),
    nomatch = 0L,
    filter_expr = quote(range_dates %in% range_dates_dup_matched)
  )
  
  match_range_dup_dates <- match_range_dup_dates[, quality_matching := "6° round"]
  
  # bind ids rows with 1°round matching and those with 2° round matching
  match_df <- rbind(match_df[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)], match_range_dup_dates[, .(person_id, id_bleeding, type_dose, type_dose_15, type_dose_15_20, event, quality_matching, data, id_dispensing, Nvialsday)])
  
}


# considering high dose of DOACs both 15 and 20 mg
# match by date and type and dose and eventually handle bleedings with duplicated dates and dose: count nrows by date and dose in both dataset and proceed with a deterministic linkage in case nrow is equal

count_dates_bleeding <- D3_study_population_s[, .(count_a = .N), by = .(date_bleeding, type_dose_15_20)]
count_dates_fes <- D3_dispensings_AA_s[, .(count_b = .N), by = .(data, type_dose)]
count_merged <- merge.data.table(count_dates_bleeding, count_dates_fes, by.x = c("date_bleeding","type_dose_15_20"), by.y = c("data", "type_dose"), all = TRUE)
dates_dup_matched <- count_merged[count_a==count_b, date_bleeding]


if(length(dates_dup_matched) > 0) {
  
  match_dup_dates <- join_with_keys(
    D3_dispensings_AA_s,
    D3_study_population_s,
    on = c("data" = "date_bleeding", "type_dose" = "type_dose_15_20"),
    nomatch = 0L,
    filter_expr = quote(data %in% dates_dup_matched)
  )
  
  match_dup_dates <- match_dup_dates[, quality_matching := "5° round (b)"]
  
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
count_range_dates_bleeding <- D3_study_population_s2[, .(count_a = .N), by = .(range_dates, type_dose_15_20)]
count_range_dates_fes <- D3_dispensings_AA_s2[, .(count_b = .N), by = .(range_dates, type_dose)]
count_range_merged <- merge.data.table(count_range_dates_bleeding, count_range_dates_fes, by.x = c("range_dates", "type_dose_15_20"), by.y = c("range_dates", "type_dose"), all = TRUE)
range_dates_dup_matched <- count_range_merged[count_a==count_b, range_dates]

if(length(range_dates_dup_matched) > 0) {
  
  match_range_dup_dates <- join_with_keys(
    D3_dispensings_AA_s2,
    D3_study_population_s2,
    on = c("range_dates" = "range_dates", "type_dose" = "type_dose_15_20"),
    nomatch = 0L,
    filter_expr = quote(range_dates %in% range_dates_dup_matched)
  )
  
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