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


# import input datasets
input <- readRDS(file.path(thisdirinput, "D4_analytical_dataset.rds"))

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

input <- input %>% 
           mutate(period_3level = ifelse(grepl("^1", period), "1", period))

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

fix_time <- fix_time %>% 
              mutate(period_3level = ifelse(grepl("^1", period), "1", period))

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
    relocate(year, .before = time) %>% 
    mutate(period_3level = ifelse(grepl("^1", period), "1", period))
  
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

png(file.path(thisdiroutput,"outcomes_possible.png"), units="in", height=10, width=15, res=300)

# scatter plot separati per outcome (nel tempo)
ggplot(combined_data, aes(x = time, y = prop_poss)) +
  geom_line(aes(color = outcome), size = 1) +
  geom_point(size = 1) +
  labs(
    title = "Andamento della proporzione degli outcome di interesse (def. possible)",
    x = "Tempo, mesi",
    y = "Proporzione"
  ) +
  scale_color_manual(
    values = c("outcome_DEATH" = "#F8766D", "outcome_THROM" = "#00BFC4"),
    labels = c("outcome_DEATH" = "Decesso", "outcome_THROM" = "Evento tromboembolico")
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_wrap(~outcome, scales = "free_y",
             labeller = as_labeller(c(
               "outcome_DEATH" = "Decesso",
               "outcome_THROM" = "Evento tromboembolico"
             )))

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
  scale_color_manual(
    values = c("outcome_DEATH" = "#F8766D", "outcome_THROM" = "#00BFC4"),
    labels = c("outcome_DEATH" = "Decesso", "outcome_THROM" = "Evento tromboembolico")
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_wrap(~outcome, scales = "free_y",
             labeller = as_labeller(c(
               "outcome_DEATH" = "Decesso",
               "outcome_THROM" = "Evento tromboembolico"
             )))

dev.off()

#######################################################################################

# A) analysis with time as unit ----

## A1) periodo with 3 levels ----

fit_models <- function(data) {
  
  data <- data %>%
    mutate(period_3level = factor(period_3level))
  
  list(
    # periodo
    fit_periodo_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ period_3level, family = binomial, data = data)),
    fit_periodo_broad <- summary(glm(cbind(event_broad, n_emor_broad) ~ period_3level, family = binomial, data = data)),
    # stagionalità (considero un df per stagione, 4 all'anno, dove df = numero di nodi interni + 1)
    # se usassi una natural spline ns, terrei conto allo stesso tempo di trend e stagionalità mentr con pbs riesco ad isolare la stagionalità
    fit_per_stag_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + period_3level, family = binomial, data = data)),
    fit_per_stag_broad <- summary(glm(cbind(event_broad, n_emor_broad) ~ pbs(month, df = 4) + period_3level, family = binomial, data = data)),
    # trend e stagionalità (con pbs)
    fit_per_stag_trend_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + time + period_3level, family = binomial, data = data)),
    fit_per_stag_trend_broad <- summary(glm(cbind(event_broad, n_emor_broad) ~ pbs(month, df = 4) + time + period_3level, family = binomial, data = data))
  )
  
}

model_results <- map(results_updated, fit_models)

# save
save(model_results, file = file.path(thisdiroutput,"model_results_1_aggregate_3levels.rda"))


## B) periodo with 5 levels ----

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

# save
save(model_results, file = file.path(thisdiroutput,"model_results_2_aggregate_5levels.rda"))

# create composite event

input <- input %>% 
           mutate(outcome_comp = case_when((outcome_DEATH==1 | outcome_THROM==1) ~ 1,
                                                TRUE ~ 0))

#########################################################################################################

# B) analysis with individual as unit ----

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

# rename period
input <- input %>% 
  mutate(period_3level = factor(case_when(period_3level == "1" ~ "Senza antidoto",
                                          period_3level == "2" ~ "Con antidoto senza linee guida",
                                          period_3level == "3" ~ "Con antidoto con linee guida")))

input <- input %>% 
  mutate(period_3level = factor(period_3level, levels = c("Senza antidoto",
                                                          "Con antidoto senza linee guida",
                                                          "Con antidoto con linee guida")))

# relevel period
input <- input %>% 
  mutate(period_3level = relevel(factor(period_3level), "Senza antidoto"))

# rename agebands
input <- input %>% 
           rename(Eta_cat = agebands_analysis)

input <- input %>% 
           mutate(Eta_cat = relevel(factor(Eta_cat), "80-84"))


outcomes <- c("outcome_DEATH", "outcome_THROM", "outcome_comp")

##  B1) period with 3 levels ----

models_definitions <- function(data, outcome) {
  
  data <- data %>%
    mutate(period_3level = factor(period_3level))
  
  f <- as.formula(paste(outcome, "~ period_3level + prob_exp + Eta_cat + gender"))
  f_stag <- as.formula(paste(outcome, "~ period_3level + prob_exp + Eta_cat + gender + pbs(month, df = 4)"))
  f_mix <- as.formula(paste(outcome, "~ period_3level + prob_exp + Eta_cat + gender + (1|person_id)"))
  f_stag_mix <- as.formula(paste(outcome, "~ period_3level + prob_exp + Eta_cat + gender + pbs(month, df = 4) + (1|person_id)"))
  
  # filtro i dati una sola volta
  data_narrow <- data %>% filter(type_bleeding == "narrow")
  
  list(
    fit_periodo_narrow_mix = summary(glmer(f_mix, data = data_narrow, family = binomial)),
    fit_periodo_narrow_log = summary(glm(f, data = data_narrow, family = binomial)),
    
    fit_periodo_stag_narrow_mix = summary(glmer(f_stag_mix, data = data_narrow, family = binomial)),
    fit_periodo_stag_narrow_log = summary(glm(f_stag, data = data_narrow, family = binomial))
  )
}


model_results <- list()

for (i in outcomes) {
  
  model_results[[i]] <- models_definitions(input, i)
  
  
}

# save
save(model_results, file = file.path(thisdiroutput,"model_results_3_individual_3levels.rda"))


## B2) period with 5 levels----

models_definitions <- function(data, outcome) {
  
  data <- data %>%
    mutate(period = factor(period))
  
  f <- as.formula(paste(outcome, "~ period + prob_exp + Eta_cat + gender"))
  f_stag <- as.formula(paste(outcome, "~ period + prob_exp + Eta_cat + gender + pbs(month, df = 4)"))
  f_mix <- as.formula(paste(outcome, "~ period + prob_exp + Eta_cat + gender + (1|person_id)"))
  f_stag_mix <- as.formula(paste(outcome, "~ period + prob_exp + Eta_cat + gender + pbs(month, df = 4) + (1|person_id)"))
  
  # filtro i dati una sola volta
  data_narrow <- data %>% filter(type_bleeding == "narrow")
  
  list(
    fit_periodo_narrow_mix = summary(glmer(f_mix, data = data_narrow, family = binomial)),
    fit_periodo_narrow_log = summary(glm(f, data = data_narrow, family = binomial)),
    
    fit_periodo_stag_narrow_mix = summary(glmer(f_stag_mix, data = data_narrow, family = binomial)),
    fit_periodo_stag_narrow_log = summary(glm(f_stag, data = data_narrow, family = binomial))
  )
}

model_results <- list()

for (i in outcomes) {
  
  model_results[[i]] <- models_definitions(input, i)
  
  
}

# save
save(model_results, file = file.path(thisdiroutput,"model_results_4_individual_5levels.rda"))

