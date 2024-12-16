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

# analysis
input <- input %>% 
           mutate(time = as.numeric(ceiling((date_bleeding - as.Date("2018-01-01"))/30)),
                  month = month(date_bleeding))

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

# creo un dataset dove l'unità è il tempo (mese)

# outcome_vars <- grep("^outcome", names(input), value = TRUE)
outcome_vars <- c("outcome_THROM", "outcome_DEATH")

results <- map(outcome_vars, function(var) {
  
  input %>%
    group_by(time) %>% 
    summarise(event_narrow = sum(.data[[var]]*(type_bleeding == "narrow"), na.rm = T),
              event_poss = sum(.data[[var]]*(type_bleeding == "possible"), na.rm = T)) %>% 
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
                         mutate(prop_poss = event_poss/n_emor_poss,
                                prop_narrow = event_narrow/n_emor_narrow)

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
    fit_periodo_poss <- summary(glm(cbind(event_poss, n_emor_poss) ~ period, family = binomial, data = data)),
    # stagionalità (considero un df per stagione, 4 all'anno, dove df = numero di nodi interni + 1)
    # se usassi una natural spline ns, terrei conto allo stesso tempo di trend e stagionalità mentr con pbs riesco ad isolare la stagionalità
    fit_per_stag_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + period, family = binomial, data = data)),
    fit_per_stag_poss <- summary(glm(cbind(event_poss, n_emor_poss) ~ pbs(month, df = 4) + period, family = binomial, data = data)),
    # trend e stagionalità (con pbs)
    fit_per_stag_trend_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + time + period, family = binomial, data = data)),
    fit_per_stag_trend_poss <- summary(glm(cbind(event_poss, n_emor_poss) ~ pbs(month, df = 4) + time + period, family = binomial, data = data))
  )
  
}

model_results <- map(results_updated, fit_models)

save(model_results, file = file.path(thisdiroutput,"model_results.rda"))

# analysis with individual as unit ----

model_results_indiv <- list(
  
  fit_indiv_narrow_mixed <- summary(glmer(outcome_DEATH ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
  fit_indiv_narrow_fixed <- summary(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
  robust_se_cluster <- coeftest(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")])),
  fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_DEATH ~ period + ageband + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
  fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_DEATH ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
  robust_se_cluster <- coeftest(glm(outcome_DEATH ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_DEATH ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")])),
  
  fit_indiv_narrow_mixed <- summary(glmer(outcome_THROM ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
  fit_indiv_narrow_fixed <- summary(glm(outcome_THROM ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
  robust_se_cluster <- coeftest(glm(outcome_THROM ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_THROM ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")])),
  fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_THROM ~ period + ageband + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input)),
  fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_THROM ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input)),
  robust_se_cluster <- coeftest(glm(outcome_THROM ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input), vcov = vcovCL(glm(outcome_THROM ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input), cluster = input$person_id[which(input$type_bleeding=="narrow")]))
  
  )
  

# names(model_results_indiv) <- c("Effetti misti con età in continuo - Caso narrow",
#                                 "Effetti fissi con età in continuo - Caso narrow",
#                                 "Effetti fissi con età in continuo e SE robusti - Caso narrow",
#                                 "Effetti misti con età categorica - Caso narrow",
#                                 "Effetti fissi con età categorica - Caso narrow",
#                                 "Effetti fissi con età categorica e SE robusti - Caso narrow")

save(model_results_indiv, file = file.path(thisdiroutput,"model_results_indiv.rda"))

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

# creo un dataset dove l'unità è il tempo (mese)

# outcome_vars <- grep("^outcome", names(input), value = TRUE)
outcome_vars <- c("outcome_THROM", "outcome_DEATH")

results_sens <- map(outcome_vars, function(var) {
  
  input_sens %>%
    group_by(time) %>% 
    summarise(event_narrow = sum(.data[[var]]*(type_bleeding == "narrow"), na.rm = T),
              event_poss = sum(.data[[var]]*(type_bleeding == "possible"), na.rm = T)) %>% 
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
    mutate(prop_poss = event_poss/n_emor_poss,
           prop_narrow = event_narrow/n_emor_narrow)
  
}

combined_data <- bind_rows(lapply(names(results_sens_updated), function(name) {
  results_sens_updated[[name]] %>% 
    mutate(outcome = name)
}), .id = "id")

# Plots ----

png(file.path(thisdiroutput,"outcomes_possible_sens.png"), units="in", height=10, width=15, res=300)

# scatter plot separati per outcome (nel tempo)
ggplot(combined_data, aes(x = time, y = prop_poss)) +
  geom_line(aes(color = outcome), size = 1) +
  geom_point(size = 1) +
  labs(
    title = "Andamento della proporzione degli outcome di interesse (def. possible) - Solo primo outcome emorragico",
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

# analysis with individual as unit ----

model_results_indiv_sens <- list(
  
  fit_indiv_narrow_mixed <- summary(glmer(outcome_DEATH ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
  fit_indiv_narrow_fixed <- summary(glm(outcome_DEATH ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
  fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_DEATH ~ period + ageband + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
  fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_DEATH ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
  
  fit_indiv_narrow_mixed <- summary(glmer(outcome_THROM ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
  fit_indiv_narrow_fixed <- summary(glm(outcome_THROM ~ period + age + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
  fit_indiv_narrow_mixed_agecat <- summary(glmer(outcome_THROM ~ period + ageband + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
  fit_indiv_narrow_fixed_agecat <- summary(glm(outcome_THROM ~ period + ageband + gender, subset = type_bleeding == "narrow", family = binomial, data = input_sens)),
  
)

# names(model_results_indiv) <- c("Effetti misti con età in continuo - Caso narrow",
#                                 "Effetti fissi con età in continuo - Caso narrow",
#                                 "Effetti fissi con età in continuo e SE robusti - Caso narrow",
#                                 "Effetti misti con età categorica - Caso narrow",
#                                 "Effetti fissi con età categorica - Caso narrow",
#                                 "Effetti fissi con età categorica e SE robusti - Caso narrow")

save(model_results_indiv_sens, file = file.path(thisdiroutput,"model_results_indiv_sens.rda"))

# Descriptive table with number of death, thromboembolic and emorragic events by period ----

num_death <- input %>% 
  group_by(period) %>% 
  summarise(num_death = sum(outcome_DEATH))

num_throm <- input %>% 
  group_by(period) %>% 
  summarise(num_throm = sum(outcome_THROM))
  
num_emorr <- input %>% 
  group_by(period) %>% 
  summarise(num_emorr = n())

num_emorr_solo_prima <- input_sens %>% 
  group_by(period) %>% 
  summarise(num_emorr_prima = n())

tables <- list(num_death, num_throm, num_emorr, num_emorr_solo_prima)

descr_tab <- purrr::reduce(tables, inner_join, by = "period")

save(descr_tab, file = file.path(thisdiroutput,"descr_tab.rda"))

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
