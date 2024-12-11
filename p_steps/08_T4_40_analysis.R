########################################################%##
#                                                          #
####  COMPUTE D5_results_from_analysis
####
#                                                          #
########################################################%##


# authors: Rosa Gini, Ersilia Lucenteforte, Sabrina Giometto

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
           mutate(time = as.numeric(ceiling((date_bleeding+1 - as.Date("2018-01-01"))/30)),
                  month = month(date_bleeding))

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

den_broad <- input %>%
               filter(type_bleeding=="possible") %>% 
               group_by(time) %>% 
               summarise(n_emor_broad = n())

# creo un dataset dove l'unità è il tempo (mese)
outcome_vars <- grep("^outcome", names(input), value = TRUE)
             
results <- map(outcome_vars, function(var) {
  
  input %>%
    group_by(time) %>% 
    summarise(event_narrow = sum(.data[[var]]*(type_bleeding == "narrow"), na.rm = T),
              event_broad = sum(.data[[var]]*(type_bleeding == "possible"), na.rm = T)) %>% 
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
                         left_join(den_broad, by = "time") %>%
                         left_join(den_narrow, by = "time") %>%
                         mutate(prop_broad = event_broad/n_emor_broad,
                                prop_narrow = event_narrow/n_emor_narrow)

}

combined_data <- bind_rows(lapply(names(results_updated), function(name) {
  results_updated[[name]] %>% 
    mutate(outcome = name)
}), .id = "id")

png(file.path(thisdiroutput,"outcomes_possible.png"), units="in", height=10, width=15, res=300)

# scatter plot separati per outcome (nel tempo)
ggplot(combined_data, aes(x = time, y = prop_broad)) +
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
    fit_periodo_broad <- summary(glm(cbind(event_broad, n_emor_broad) ~ period, family = binomial, data = data)),
    # stagionalità (considero un df per stagione, 4 all'anno, dove df = numero di nodi interni + 1)
    fit_per_stag_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + period, family = binomial, data = data)),
    fit_per_stag_broad <- summary(glm(cbind(event_broad, n_emor_broad) ~ pbs(month, df = 4) + period, family = binomial, data = data)),
    # trend e stagionalità (con pbs)
    fit_per_stag_trend_narrow <- summary(glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + time + period, family = binomial, data = data)),
    fit_per_stag_trend_broad <- summary(glm(cbind(event_broad, n_emor_broad) ~ pbs(month, df = 4) + time + period, family = binomial, data = data))
  )
  
}

model_results <- map(results_updated, fit_models)

save(model_results, file = file.path(thisdiroutput,"model_results.rda"))

# analysis with individual as unit ----


fit_indiv_narrow <- summary(glmer(outcome_DEATH ~ period + age + gender + (1|person_id), subset = type_bleeding == "narrow", family = binomial, data = input))

save(fit_indiv_narrow, file = file.path(thisdiroutput,"fit_indiv_narrow.rda"))

# fit_models_indiv <- function(data) {
#   
#      data <- data %>%
#         mutate(period = factor(period))
# 
#     fit3 <- glmer(n_emor_narrow ~ period + age + gender + (1|person_id), family = binomial, data = data)
# 
# }
# 
# model_results_indiv <- map(results_updated, fit_models_indiv)



# death <- input %>%
#            group_by(time) %>% 
#            summarise(event = sum(outcome_DEATH)) %>% 
#            mutate(period = case_when(time <= 26 | (time > 42 & time <= 44) ~ 1,
#                                      time > 26 & time <= 42 ~ 2,
#                                      time > 44 & time <= 67 ~ 3,
#                                      time > 67 ~ 4),
#                   year = cut(time, breaks = seq(1,96, by = 12), labels = c(2018:2024), right = FALSE)) %>%
#            relocate(year, .before = time) 
# 
# death <- death %>% 
#            bind_rows(fix_time %>% select(-month) %>% anti_join(death, by = c("year", "time", "period"))) %>% 
#            group_by(year) %>% 
#            mutate(month = 1:12) %>% 
#            relocate(month, .after = year)
# 
# 
# death <- death %>% 
#            left_join(den, by = "time") %>% 
#            mutate(prop = event/n_emor)

    

# # grafico
# plot(death$time, death$event)
# plot(results_updated[["outcome_AMI"]]$time, results_updated[["outcome_AMI"]]$prop_broad)
# hist(results_updated[["outcome_AMI"]]$prop_broad)

# # assumo dati indipendenti per l'outcome death, non potendosi ripetere
# death <- death %>% 
#            mutate(period = factor(period))
# fit1 <- glm(event ~ time + period, family = poisson, data = death)
# summary(fit1)
# 
# # gestisco la stagionalità inserendo una spline (tengo conto allo stesso tempo di trend e stagionalità con la ns)
# death <- death %>% 
#            mutate(period = factor(period))
# fit2 <- glm(event ~ ns(time, df = 21) + period, family = poisson, data = death)
# summary(fit2)

# # confronto AIC
# aic_values <- AIC(fit1, fit2)

# # periodo
# fit_periodo_narrow <- glm(cbind(event_narrow, n_emor_narrow) ~ period, family = binomial, data = death)
# fit_periodo_broad <- glm(cbind(event_broad, n_emor_broad) ~ period, family = binomial, data = death)
# # stagionalità (considero un df per stagione, 4 all'anno, dove df = numero di nodi interni + 1)
# fit_per_stag_narrow <- glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + period, family = binomial, data = death)
# fit_per_stag_broad <- glm(cbind(event_broad, n_emor_broad) ~ pbs(month, df = 4) + period, family = binomial, data = death)
# # trend e stagionalità (con pbs)
# fit_per_stag_trend_narrow <- glm(cbind(event_narrow, n_emor_narrow) ~ pbs(month, df = 4) + time + period, family = binomial, data = death)
# fit_per_stag_trend_broad <- glm(cbind(event_broad, n_emor_broad) ~ pbs(month, df = 4) + time + period, family = binomial, data = death)

# hist(death$rate)

# summary(fit6)

# confronto AIC
# aic_values <- AIC(fit3, fit4, fit5, fit2)

# # per gli altri outcome non assumo dati indipendenti perchè i soggetti possono rientrare
# fit3 <- glmer(event ~ time + period + (1|person_id), family = binomial, data = x)

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
