########################################################%##
#                                                          #
####  COMPUTE D5_results_from_analysis
####
#                                                          #
########################################################%##


# authors: Rosa Gini, Ersilia Lucenteforte, Sabrina Giometto

# v 0.4 16 Dec 2024 - Sensitivity analysis added

# v 0.3 11 Dec 2024

# exposure as factor

# v 0.2 06 Dic 2024

# v 0.1 28 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D5_results_from_analysis"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}


# save log file
sink(file=file.path(thisdiroutput, "log_file.txt"))

# import input datasets
input <- readRDS(file.path(thisdirinput, "D4_analytical_dataset.rds"))

# analysis
input <- input %>% 
           mutate(time = as.numeric(ceiling((date_bleeding - as.Date("2018-01-01"))/30)),
                  month = month(date_bleeding),
                  agebands_analysis = case_when((age >= 0 & age <= 59) ~ "0-59",
                                                (age > 59 & age <= 64) ~ "60-64",
                                                (age > 64 & age <= 69) ~ "65-69",
                                                (age > 69 & age <= 74) ~ "70-74",
                                                (age > 74 & age <= 79) ~ "75-79",
                                                (age > 80 & age <= 84) ~ "80-84",
                                                (age > 85 & age <= 89) ~ "85-89",
                                                (age > 89) ~ "90+"))

input <- input %>% 
           mutate(time = case_when(time==0 ~ 1,
                                   TRUE ~ as.numeric(time)))

fix_time <- data.frame(
             time = as.numeric(1:84)) %>% 
             mutate(year = cut(time, breaks = seq(1,96, by = 12), labels = c(2018:2024), right = FALSE),
                    period = case_when(time <= 26 ~ "1a",
                                       (time > 26 & time <= 41) ~ "1b",
                                       (time > 41 & time <= 44) ~ "1c",
                                       (time > 44 & time <= 67) ~ "2",
                                       time > 67 ~ "3")) %>% 
             group_by(year) %>% 
             mutate(month = 1:12) %>% 
             relocate(year, .before = time) %>% 
             relocate(month, .after = year)

# estraggo denominatore (pazienti con emorragia)
den_narrow <- input %>%
                filter(type_bleeding=="narrow") %>% 
                group_by(time) %>% 
                summarise(n_emor_narrow = n())

den_poss <- input %>%
                  filter(type_bleeding=="possible") %>% 
                  group_by(time) %>% 
                  summarise(n_emor_poss = n())

den_broad <- input %>%
                   group_by(time) %>% 
                   summarise(n_emor_broad = n())

# creo un dataset dove l'unità è il tempo (mese)

# outcome_vars <- grep("^outcome", names(input), value = TRUE)
outcome_vars <- c("outcome_THROM", "outcome_DEATH")

results <- map(outcome_vars, function(var) {
  
  input %>%
    group_by(time) %>% 
    summarise(event_narrow = sum(.data[[var]]*(type_bleeding == "narrow"), na.rm = T),
              event_poss = sum(.data[[var]]*(type_bleeding == "possible"), na.rm = T),
              event_broad = sum(.data[[var]], na.rm = T)) %>% 
    mutate(period = case_when(time <= 26 ~ "1a",
                              (time > 26 & time <= 41) ~ "1b",
                              (time > 41 & time <= 44) ~ "1c",
                              (time > 44 & time <= 67) ~ "2",
                              time > 67 ~ "3"),
             year = cut(time, breaks = seq(1,96, by = 12), labels = c(2018:2024), right = FALSE)) %>%
    relocate(year, .before = time)
  
})

names(results) <- outcome_vars


results_updated <- list()

for (i in names(results)) {
  
  df <- results[[i]] 
  
  results_updated[[i]] <- df %>% 
                         bind_rows(fix_time %>% select(-month) %>% anti_join(results[[i]], by = c("year", "time", "period"))) %>% 
                         group_by(year) %>% 
                         mutate(month = 1:12) %>% 
                         relocate(month, .after = year) %>%
                         # ungroup() %>%
                         left_join(den_poss, by = "time") %>%
                         left_join(den_narrow, by = "time") %>%
                         left_join(den_broad, by = "time") %>%
                         mutate(prop_poss = event_poss/n_emor_poss,
                                prop_narrow = event_narrow/n_emor_narrow,
                                prop_broad = event_broad/n_emor_broad)

}

combined_data <- bind_rows(lapply(names(results_updated), function(name) {
  results_updated[[name]] %>% 
    mutate(outcome = name)
}), .id = "id")

png(file.path(thisdiroutput,"outcomes_broad.png"), units="in", height=10, width=15, res=300)

# scatter plot separati per outcome (nel tempo)
ggplot(combined_data, aes(x = time, y = prop_broad)) +
  geom_line(aes(color = outcome), size = 1) +
  geom_point(size = 1) +
  labs(
    title = "Andamento della proporzione degli outcome di interesse (def. broad)",
    x = "Tempo, mesi",
    y = "Proporzione"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_wrap(~outcome, scales = "free_y")

dev.off()

png(file.path(thisdiroutput,"outcomes_narrow.png"), units="in", height=10, width=15, res=300)

# scatter plot separati per outcome (nel tempo)
ggplot(combined_data, aes(x = time, y = prop_narrow)) +
  geom_line(aes(color = outcome), size = 1) +
  geom_point(size = 1) +
  labs(
    title = "Andamento della proporzione degli outcome di interesse (def.narrow)",
    x = "Tempo, mesi",
    y = "Proporzione"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_wrap(~outcome, scales = "free_y")

dev.off()

# analysis with time as unit ----

fit_models <- function(data) {
  
  data <- data %>%
            mutate(period = factor(period))
  
  list(
    # periodo
    fit_periodo_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ period, family = binomial, data = data)),
    fit_periodo_broad <- summary(glm(cbind(event_broad, n_emor_broad) ~ period, family = binomial, data = data)),
    # stagionalità (considero un df per stagione, 4 all'anno, dove df = numero di nodi interni + 1)
    # se usassi una natural spline ns, terrei conto allo stesso tempo di trend e stagionalità mentr con pbs riesco ad isolare la stagionalità
    fit_per_stag_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + period, family = binomial, data = data)),
    fit_per_stag_broad <- summary(glm(cbind(event_broad, n_emor_broad) ~ pbs(month, df = 4) + period, family = binomial, data = data)),
    # trend e stagionalità (con pbs)
    fit_per_stag_trend_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + time + period, family = binomial, data = data)),
    fit_per_stag_trend_broad <- summary(glm(cbind(event_broad, n_emor_broad) ~ pbs(month, df = 4) + time + period, family = binomial, data = data))
  )
  
}

model_results <- map(results_updated, fit_models)

save(model_results, file = file.path(thisdiroutput,"model_results.rda"))

# create composite event

input <- input %>% 
           mutate(outcome_comp = case_when((outcome_DEATH==1 | outcome_THROM==1) ~ 1,
                                                TRUE ~ 0))

# analysis with individual as unit ----

# model_results_indiv <- list(
#   
#   fit_indiv_narrow_mixed <- summary(glmer(outcome_DEATH ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
#   fit_indiv_narrow_fixed <- summary(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
#   robust_se_cluster <- coeftest(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")])),
#   fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_DEATH ~ period + agebands_analysis + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
#   fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_DEATH ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
#   robust_se_cluster <- coeftest(glm(outcome_DEATH ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_DEATH ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")])),
#   
#   fit_indiv_narrow_mixed <- summary(glmer(outcome_THROM ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
#   fit_indiv_narrow_fixed <- summary(glm(outcome_THROM ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
#   robust_se_cluster <- coeftest(glm(outcome_THROM ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_THROM ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")])),
#   fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_THROM ~ period + agebands_analysis + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
#   fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_THROM ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
#   robust_se_cluster <- coeftest(glm(outcome_THROM ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_THROM ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")])),
#   
#   fit_indiv_narrow_mixed <- summary(glmer(outcome_composite ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
#   fit_indiv_narrow_fixed <- summary(glm(outcome_composite ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
#   robust_se_cluster <- coeftest(glm(outcome_composite ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")])),
#   fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_composite ~ period + agebands_analysis + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
#   fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_composite ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
#   robust_se_cluster <- coeftest(glm(outcome_composite ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_DEATH ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")])),
# )
#   

# names(model_results_indiv) <- c("Effetti misti con età in continuo - Caso narrow",
#                                 "Effetti fissi con età in continuo - Caso narrow",
#                                 "Effetti fissi con età in continuo e SE robusti - Caso narrow",
#                                 "Effetti misti con età categorica - Caso narrow",
#                                 "Effetti fissi con età categorica - Caso narrow",
#                                 "Effetti fissi con età categorica e SE robusti - Caso narrow")

# save(model_results_indiv, file = file.path(thisdiroutput,"model_results_indiv.rda"))

# relevel age 
input <- input %>% 
  mutate(agebands_analysis = relevel(factor(agebands_analysis), "80-84")) 

# rename period
input <- input %>% 
           mutate(period = factor(case_when(period == "1a" ~ "Senza antidoto pre COVID",
                                     period == "1b" ~ "Senza antidoto con restrizioni",
                                     period == "1c" ~ "Senza antidoto senza restrizioni",
                                     period == "2" ~ "Con antidoto senza linee guida",
                                     period == "3" ~ "Con antidoto con linee guida")))

input <- input %>% 
           mutate(period = factor(period, levels = c("Senza antidoto pre COVID",
                                                     "Senza antidoto con restrizioni",
                                                     "Senza antidoto senza restrizioni",
                                                     "Con antidoto senza linee guida",
                                                     "Con antidoto con linee guida")))

# relevel period
input <- input %>% 
  mutate(period = relevel(factor(period), "Senza antidoto pre COVID"))

# rename agebands
input <- input %>% 
           rename(Eta_cat = agebands_analysis)

input <- input %>% 
           mutate(Eta_cat = relevel(factor(Eta_cat), "80-84"))

# Sensitivity analysis (considering only the first emorragic event per individual) ----

input_sens <- input %>% 
                filter(number_previous_bleedings==0)

# estraggo denominatore (pazienti con emorragia)
den_narrow_sens <- input_sens %>%
                       filter(type_bleeding=="narrow") %>% 
                       group_by(time) %>% 
                       summarise(n_emor_narrow = n())

den_poss_sens <- input_sens %>%
                     filter(type_bleeding=="possible") %>% 
                     group_by(time) %>% 
                     summarise(n_emor_poss = n())

den_broad_sens <- input_sens %>%
                     group_by(time) %>% 
                     summarise(n_emor_broad = n())

# creo un dataset dove l'unità è il tempo (mese)

# outcome_vars <- grep("^outcome", names(input), value = TRUE)
outcome_vars <- c("outcome_THROM", "outcome_DEATH")

results_sens <- map(outcome_vars, function(var) {
  
  input_sens %>%
    group_by(time) %>% 
    summarise(event_narrow = sum(.data[[var]]*(type_bleeding == "narrow"), na.rm = T),
              event_poss = sum(.data[[var]]*(type_bleeding == "possible"), na.rm = T),
              event_broad = sum(.data[[var]], na.rm = T)) %>% 
    mutate(period = case_when(time <= 26 ~ "1a",
                              (time > 26 & time <= 41) ~ "1b",
                              (time > 41 & time <= 44) ~ "1c",
                              (time > 44 & time <= 67) ~ "2",
                              time > 67 ~ "3"),
           year = cut(time, breaks = seq(1,96, by = 12), labels = c(2018:2024), right = FALSE)) %>%
    relocate(year, .before = time)
  
})

names(results_sens) <- outcome_vars


results_sens_updated <- list()

for (i in names(results_sens)) {
  
  df <- results_sens[[i]] 
  
  results_sens_updated[[i]] <- df %>% 
    bind_rows(fix_time %>% select(-month) %>% anti_join(results_sens[[i]], by = c("year", "time", "period"))) %>% 
    group_by(year) %>% 
    mutate(month = 1:12) %>% 
    relocate(month, .after = year) %>%
    # ungroup() %>%
    left_join(den_poss_sens, by = "time") %>%
    left_join(den_narrow_sens, by = "time") %>%
    left_join(den_broad_sens, by = "time") %>%
    mutate(prop_poss = event_poss/n_emor_poss,
           prop_narrow = event_narrow/n_emor_narrow,
           prop_broad = event_broad/n_emor_broad)
  
}

combined_data <- bind_rows(lapply(names(results_sens_updated), function(name) {
  results_sens_updated[[name]] %>% 
    mutate(outcome = name)
}), .id = "id")

# Plots ----

png(file.path(thisdiroutput,"outcomes_broad_sens.png"), units="in", height=10, width=15, res=300)

# scatter plot separati per outcome (nel tempo)
ggplot(combined_data, aes(x = time, y = prop_poss)) +
  geom_line(aes(color = outcome), size = 1) +
  geom_point(size = 1) +
  labs(
    title = "Andamento della proporzione degli outcome di interesse (def.broad) - Solo primo outcome emorragico",
    x = "Tempo, mesi",
    y = "Proporzione"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_wrap(~outcome, scales = "free_y")

dev.off()

png(file.path(thisdiroutput,"outcomes_narrow_sens.png"), units="in", height=10, width=15, res=300)

# scatter plot separati per outcome (nel tempo)
ggplot(combined_data, aes(x = time, y = prop_narrow)) +
  geom_line(aes(color = outcome), size = 1) +
  geom_point(size = 1) +
  labs(
    title = "Andamento della proporzione degli outcome di interesse (def.narrow) - Solo primo outcome emorragico",
    x = "Tempo, mesi",
    y = "Proporzione"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_wrap(~outcome, scales = "free_y")

dev.off()

# analysis with time as unit ----

model_results_sens <- map(results_sens_updated, fit_models)
save(model_results_sens, file = file.path(thisdiroutput,"model_results_sens.rda"))

# analysis with individual as unit, for both main and sensitivity analysis ----

outcomes <- c("outcome_DEATH", "outcome_THROM", "outcome_comp")

dataset_names <- c("input", "input_sens")

# models_name <- c("Decesso_log_misure_rip_eta_cont", "Decesso_log_misure_rip_eta_cat",
#                  "Tromb_log_misure_rip_eta_cont", "Tromb_log_misure_rip_eta_cat",
#                  "Comp_log_misure_rip_eta_cont", "Comp_log_misure_rip_eta_cat")

# models_fit <- list(
#   
#   "Decesso_mix_misure_rip_eta_cont" = function(data) glmer(outcome_DEATH ~ period + age + gender + (1|person_id), 
#                                                              subset = type_bleeding == "narrow", 
#                                                              family = binomial, data = data),
#   "Decesso_mix_misure_rip_eta_cat" = function(data) glmer(outcome_DEATH ~ period + agebands_analysis + gender + (1|person_id), 
#                                                             subset = type_bleeding == "narrow", 
#                                                             family = binomial, data = data),
#   "Decesso_log_misure_rip_eta_cont" = function(data) glm(outcome_DEATH ~ period + age + gender, 
#                                                          subset = type_bleeding == "narrow", 
#                                                          family = binomial, data = data),
#   "Decesso_log_misure_rip_eta_cat" = function(data) glm(outcome_DEATH ~ period + agebands_analysis + gender, 
#                                                         subset = type_bleeding == "narrow", 
#                                                         family = binomial, data = data),
#   
#   "Tromb_log_misure_rip_eta_cont" = function(data) glm(outcome_THROM ~ period + age + gender, 
#                                                          subset = type_bleeding == "narrow", 
#                                                          family = binomial, data = data),
#   "Tromb_log_misure_rip_eta_cat" = function(data) glm(outcome_THROM ~ period + agebands_analysis + gender, 
#                                                         subset = type_bleeding == "narrow", 
#                                                         family = binomial, data = data),
#   
#   "Comp_log_misure_rip_eta_cont" = function(data) glm(outcome_composite ~ period + age + gender, 
#                                                          subset = type_bleeding == "narrow", 
#                                                          family = binomial, data = data),
#   "Comp_log_misure_rip_eta_cat" = function(data) glm(outcome_composite ~ period + agebands_analysis + gender, 
#                                                         subset = type_bleeding == "narrow", 
#                                                         family = binomial, data = data)
#   
# )

models_definitions <- list(
  # "mix_eta_cont" = function(data, outcome) 
  #   glmer(as.formula(paste(outcome, "~ period + age + gender + (1|person_id)")), 
  #         subset = type_bleeding == "narrow", 
  #         family = binomial, data = data),
  
  "mix_eta_cat" = function(data, outcome) 
    glmer(as.formula(paste(outcome, "~ period + Eta_cat + gender + (1|person_id)")), 
          subset = type_bleeding == "narrow", 
          family = binomial, data = data),
  
  # "log_eta_cont" = function(data, outcome) 
  #   glm(as.formula(paste(outcome, "~ period + age + gender")), 
  #       subset = type_bleeding == "narrow", 
  #       family = binomial, data = data),
  
  "log_eta_cat" = function(data, outcome) 
    glm(as.formula(paste(outcome, "~ period + Eta_cat + gender")), 
        subset = type_bleeding == "narrow", 
        family = binomial, data = data),
  
  "log_stag" = function(data, outcome) 
    glm(as.formula(paste(outcome, "~ period + Eta_cat + gender + pbs(month, df = 4)")), 
        subset = type_bleeding == "narrow", 
        family = binomial, data = data),
  
  "mix_stag" = function(data, outcome) 
    glmer(as.formula(paste(outcome, "~ period + Eta_cat + gender + pbs(month, df = 4) + (1|person_id)")), 
          subset = type_bleeding == "narrow", 
          family = binomial, data = data)

  
)

# names(models_fit) <- models_name

# Ciclo per ciascun outcome
for (i in outcomes) {
  # Definisci il percorso del file Excel
  out <- gsub("[^A-Za-z0-9]", "_", i)  # Rendi il nome del file sicuro
  file_path <- file.path(thisdiroutput, paste0("narrow_", out ,".xlsx"))
  
  # Carica o crea il workbook
  if (file.exists(file_path)) {
    wb <- loadWorkbook(file_path)
  } else {
    wb <- createWorkbook()
  }
  
  
  for (l in dataset_names) {
    
    # Nome del dataset da usare nel foglio
    dataset <- get(l)
    dataset_name <- ifelse(identical(dataset, input), "input", "input_sens")
    
    tabella_eventi <- dataset %>%
      group_by(period) %>%
      summarise(
        n_eventi = sum(get(i), na.rm = TRUE),   # Usa il nome dinamico per l'outcome
        n_emorragie = n()                        # Numero di soggetti (denominatore)
      ) %>%
      ungroup()
    
    # Aggiungi la riga "Totale"
    final_table <- tabella_eventi %>%
      bind_rows(data.frame(
        period = "Totale",                     # Nome per la riga di totale
        n_eventi = sum(tabella_eventi$n_eventi),
        n_emorragie = sum(tabella_eventi$n_emorragie)
      ))
  
  # Aggiungi un foglio per ogni modello
  for (j in names(models_definitions)) {
    sheet_name <- paste0(sub("^[^_]*_", "", out), "_", j, "_", l)
    # Controlla se il foglio esiste già
    if (!(sheet_name %in% names(wb))) {
      addWorksheet(wb, sheet_name)
    }
    
    # write final table
    writeData(wb, sheet = sheet_name, x = final_table, startRow = 1)
    
  #   # Scrivi i dati nel foglio
  #   if (k %in% names(models_fit)) {
  #     writeData(wb, sheet = k, x = models_fit[[k]])
  #   }
  # }
    
    # Ottieni il summary del modello
    # model_summary <- broom::tidy(models_fit[[j]])
    
    # Prova a eseguire il modello
    result <- tryCatch({
    #   # Esegui il modello
    #   model <- models_fit[[j]](input) 
    #   # Se il modello funziona, scrivi il summary, verificando il tipo di modello
    #   if (inherits(model, "glmerMod")) {
    #     summary_table <- broom.mixed::tidy(model)
    #   } else {
    #     summary_table <- broom::tidy(model)
    #   }
    #   writeData(wb, sheet = j, x = summary_table)
    #   NULL  # Nessun errore
    # }, error = function(e) {
    #   # Se c'è un errore, scrivi il messaggio nel foglio
    #   writeData(wb, sheet = j, x = tibble(Error = e$message))
    #   e$message  # Ritorna il messaggio d'errore
    # })
      
      model <- models_definitions[[j]](input, i)  # Passa l'outcome dinamicamente
      
      # tab <- tab_model(model, transform = "exp", show.intercept = FALSE)
      
      tab <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>% 
               slice(-1) %>%
               mutate(CI_95 = paste(round(conf.low,2), round(conf.high,2), sep = "-"),
                      estimate = round(estimate, 2),
                      term = case_when(
                        term == "periodSenza antidoto con restrizioni" ~ "Senza antidoto con restrizioni",
                        term == "periodSenza antidoto senza restrizioni" ~ "Senza antidoto senza restrizioni",
                        term == "periodCon antidoto senza linee guida" ~ "Con antidoto senza linee guida",
                        term == "periodCon antidoto con linee guida" ~ "Con antidoto con linee guida",
                        term == "Eta_cat0-59" ~ "Età 0-59",
                        term == "Eta_cat60-64" ~ "Età 60-64",
                        term == "Eta_cat65-69"~ "Età 65-69",
                        term == "Eta_cat70-74"~ "Età 70-74",
                        term == "Eta_cat75-79"~ "Età 75-79",
                        term == "Eta_cat85-89"~ "Età 85-89",
                        term == "Eta_cat90+"~ "Età 90+",
                        term == "genderM"~ "Sesso, M",
                        TRUE ~ term # Mantiene il valore originale per altri livelli
                      )) %>% 
               select(c("term","estimate","CI_95")) %>% 
               rename(OR = estimate)
               
      
      # # Verifica il tipo di modello per il tidy
      # if (inherits(model, "glmerMod")) {
      #   summary_table <- broom.mixed::tidy(model)
      # } else {
      #   summary_table <- broom::tidy(model)
      # }

      # Calcola BIC
      bic_value <- BIC(model)
      # calcolo r2
      r_quadro <- round(unlist(r2(model)),2)
      
      # Scrivi l'output nel foglio
      # writeData(wb, sheet = sheet_name, x = tab)
      writeData(wb, sheet = sheet_name, x = tab, startRow = nrow(final_table) + 3)
      
      writeData(wb, sheet = sheet_name, x = tibble(BIC = bic_value), startCol = ncol(tab) + 1, startRow = 1)
      
      writeData(wb, sheet = sheet_name, x = tibble(R_quadro = r_quadro), startCol = ncol(tab) + 2, startRow = 1)
      
      NULL
    }, error = function(e) {
      # Scrivi il messaggio d'errore se il modello fallisce
      writeData(wb, sheet = sheet_name, x = tibble(Error = e$message))
      e$message
    })
    
    # Log per console (facoltativo)
    if (!is.null(result)) {
      message("Errore nel modello '", j, "' per outcome '", i, "': ", result)
    }
  }
}
  
  # Salva il workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

# broad (narrow + possible) ----

models_definitions <- list(
  # "mix_eta_cont" = function(data, outcome) 
  #   glmer(as.formula(paste(outcome, "~ period + age + gender + (1|person_id)")), 
  #         family = binomial, data = data),
  
  "mix_eta_cat" = function(data, outcome) 
    glmer(as.formula(paste(outcome, "~ period + Eta_cat + gender + (1|person_id)")), 
          family = binomial, data = data),
  
  # "log_eta_cont" = function(data, outcome) 
  #   glm(as.formula(paste(outcome, "~ period + age + gender")), 
  #       family = binomial, data = data),
  
  "log_eta_cat" = function(data, outcome) 
    glm(as.formula(paste(outcome, "~ period + Eta_cat + gender")), 
        family = binomial, data = data),
  
  "log_stag" = function(data, outcome) 
    glm(as.formula(paste(outcome, "~ period + Eta_cat + gender + pbs(month, df = 4)")), 
        family = binomial, data = data),
  
  "mix_stag" = function(data, outcome) 
    glmer(as.formula(paste(outcome, "~ period + Eta_cat + gender + pbs(month, df = 4) + (1|person_id)")), 
          family = binomial, data = data)
  
  
)

# names(models_fit) <- models_name

# Ciclo per ciascun outcome
for (i in outcomes) {
  # Definisci il percorso del file Excel
  out <- gsub("[^A-Za-z0-9]", "_", i)  # Rendi il nome del file sicuro
  file_path <- file.path(thisdiroutput, paste0("broad_",out ,".xlsx"))
  
  # Carica o crea il workbook
  if (file.exists(file_path)) {
    wb <- loadWorkbook(file_path)
  } else {
    wb <- createWorkbook()
  }
  
  
  for (l in dataset_names) {
    
    # Nome del dataset da usare nel foglio
    dataset <- get(l)
    dataset_name <- ifelse(identical(dataset, input), "input", "input_sens")
    
    tabella_eventi <- dataset %>%
      group_by(period) %>%
      summarise(
        n_eventi = sum(get(i), na.rm = TRUE),   # Usa il nome dinamico per l'outcome
        n_emorragie = n()                        # Numero di soggetti (denominatore)
      ) %>%
      ungroup()
    
    # Aggiungi la riga "Totale"
    final_table <- tabella_eventi %>%
      bind_rows(data.frame(
        period = "Totale",                     # Nome per la riga di totale
        n_eventi = sum(tabella_eventi$n_eventi),
        n_emorragie = sum(tabella_eventi$n_emorragie)
      ))
    
    # Aggiungi un foglio per ogni modello
    for (j in names(models_definitions)) {
      sheet_name <- paste0(sub("^[^_]*_", "", out), "_", j, "_", l)
      # Controlla se il foglio esiste già
      if (!(sheet_name %in% names(wb))) {
        addWorksheet(wb, sheet_name)
      }
      
      # write final table
      writeData(wb, sheet = sheet_name, x = final_table, startRow = 1)
      
      #   # Scrivi i dati nel foglio
      #   if (k %in% names(models_fit)) {
      #     writeData(wb, sheet = k, x = models_fit[[k]])
      #   }
      # }
      
      # Ottieni il summary del modello
      # model_summary <- broom::tidy(models_fit[[j]])
      
      # Prova a eseguire il modello
      result <- tryCatch({
        #   # Esegui il modello
        #   model <- models_fit[[j]](input) 
        #   # Se il modello funziona, scrivi il summary, verificando il tipo di modello
        #   if (inherits(model, "glmerMod")) {
        #     summary_table <- broom.mixed::tidy(model)
        #   } else {
        #     summary_table <- broom::tidy(model)
        #   }
        #   writeData(wb, sheet = j, x = summary_table)
        #   NULL  # Nessun errore
        # }, error = function(e) {
        #   # Se c'è un errore, scrivi il messaggio nel foglio
        #   writeData(wb, sheet = j, x = tibble(Error = e$message))
        #   e$message  # Ritorna il messaggio d'errore
        # })
        
        model <- models_definitions[[j]](input, i)  # Passa l'outcome dinamicamente
        
        # tab <- tab_model(model, transform = "exp", show.intercept = FALSE)
        
        tab <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>% 
          slice(-1) %>%
          mutate(CI_95 = paste(round(conf.low,2), round(conf.high,2), sep = "-"),
                 estimate = round(estimate, 2),
                 term = case_when(
                   term == "periodSenza antidoto con restrizioni" ~ "Senza antidoto con restrizioni",
                   term == "periodSenza antidoto senza restrizioni" ~ "Senza antidoto senza restrizioni",
                   term == "periodCon antidoto senza linee guida" ~ "Con antidoto senza linee guida",
                   term == "periodCon antidoto con linee guida" ~ "Con antidoto con linee guida",
                   term == "Eta_cat0-59" ~ "Età 0-59",
                   term == "Eta_cat60-64" ~ "Età 60-64",
                   term == "Eta_cat65-69"~ "Età 65-69",
                   term == "Eta_cat70-74"~ "Età 70-74",
                   term == "Eta_cat75-79"~ "Età 75-79",
                   term == "Eta_cat85-89"~ "Età 85-89",
                   term == "Eta_cat90+"~ "Età 90+",
                   term == "genderM"~ "Sesso, M",
                   TRUE ~ term # Mantiene il valore originale per altri livelli
                 )) %>% 
          select(c("term","estimate","CI_95")) %>% 
          rename(OR = estimate)
        
        
        # # Verifica il tipo di modello per il tidy
        # if (inherits(model, "glmerMod")) {
        #   summary_table <- broom.mixed::tidy(model)
        # } else {
        #   summary_table <- broom::tidy(model)
        # }
        
        # Calcola BIC
        bic_value <- BIC(model)
        # calcolo r2
        r_quadro <- round(unlist(r2(model)),2)
        
        # Scrivi l'output nel foglio
        # writeData(wb, sheet = sheet_name, x = tab)
        writeData(wb, sheet = sheet_name, x = tab, startRow = nrow(final_table) + 3)
        
        writeData(wb, sheet = sheet_name, x = tibble(BIC = bic_value), startCol = ncol(tab) + 1, startRow = 1)
        
        writeData(wb, sheet = sheet_name, x = tibble(R_quadro = r_quadro), startCol = ncol(tab) + 2, startRow = 1)
        
        NULL
      }, error = function(e) {
        # Scrivi il messaggio d'errore se il modello fallisce
        writeData(wb, sheet = sheet_name, x = tibble(Error = e$message))
        e$message
      })
      
      # Log per console (facoltativo)
      if (!is.null(result)) {
        message("Errore nel modello '", j, "' per outcome '", i, "': ", result)
      }
    }
  }
  
  # Salva il workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

# close print log file
sink()

# model_results_indiv_sens <- list(
#   
#   fit_indiv_narrow_mixed <- summary(glmer(outcome_DEATH ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   fit_indiv_narrow_fixed <- summary(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_DEATH ~ period + agebands_analysis + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_DEATH ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   
#   fit_indiv_narrow_mixed <- summary(glmer(outcome_THROM ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   fit_indiv_narrow_fixed <- summary(glm(outcome_THROM ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_THROM ~ period + agebands_analysis + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_THROM ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   
#   fit_indiv_narrow_mixed <- summary(glmer(outcome_composite ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   fit_indiv_narrow_fixed <- summary(glm(outcome_composite ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   robust_se_cluster <- coeftest(glm(outcome_composite ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens), vcov = vcovCL(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens), cluster = input_sens$person_id[which(input_sens$type_bleeding=="narrow")])),
#   fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_composite ~ period + agebands_analysis + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_composite ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
#   robust_se_cluster <- coeftest(glm(outcome_composite ~ period + agebands_analysis + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens), vcov = vcovCL(glm(outcome_DEATH ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens), cluster = input_sens$person_id[which(input_sens$type_bleeding=="narrow")])),
#   
#   
# )

# names(model_results_indiv) <- c("Effetti misti con età in continuo - Caso narrow",
#                                 "Effetti fissi con età in continuo - Caso narrow",
#                                 "Effetti fissi con età in continuo e SE robusti - Caso narrow",
#                                 "Effetti misti con età categorica - Caso narrow",
#                                 "Effetti fissi con età categorica - Caso narrow",
#                                 "Effetti fissi con età categorica e SE robusti - Caso narrow")

# save(model_results_indiv_sens, file = file.path(thisdiroutput,"model_results_indiv_sens.rda"))

# # Descriptive table with number of death, thromboembolic and emorragic events by period ----
# 
# num_death <- input %>% 
#   group_by(period) %>% 
#   summarise(num_death = sum(outcome_DEATH))
# 
# num_throm <- input %>% 
#   group_by(period) %>% 
#   summarise(num_throm = sum(outcome_THROM))
#   
# num_emorr <- input %>% 
#   group_by(period) %>% 
#   summarise(num_emorr = n())
# 
# num_emorr_solo_prima <- input_sens %>% 
#   group_by(period) %>% 
#   summarise(num_emorr_prima = n())
# 
# tables <- list(num_death, num_throm, num_emorr, num_emorr_solo_prima)
# 
# descr_tab <- purrr::reduce(tables, inner_join, by = "period")
# 
# save(descr_tab, file = file.path(thisdiroutput,"descr_tab.rda"))

# processing <- ...
  
# ################################
# # clean
# 
# tokeep <- c(...)
# 
# results <- results[, ..tokeep]
# 
# setorderv(
#   results, c(...)
# )
# 
# 
# #########################################
# # save
# 
# outputfile <- results
# 
# nameoutput <- "D5_results_from_analysis"
# nameoutputext <- paste0(nameoutput,".rds")
# assign(nameoutput, outputfile)
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
