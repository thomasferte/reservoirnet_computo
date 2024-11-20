##### load packages and functions
library(dplyr)
library(ggplot2)
library(scales)

source(here::here("functions/fct_smoothing_derivative.R"))

##### load data
ls_files_results <- list.files("data/results_forecast_testset_11922514/", full.names = TRUE)
ls_files_results_res <- grep(ls_files_results, pattern = "hp_sets", value = TRUE, invert = TRUE)
hp_file <- grep(ls_files_results, pattern = "hp_sets", value = TRUE)
df_obfuscated_epidemio <- readRDS("data/df_obfuscated_epidemio.rds")

##### labels and features
hp_features_enet <- c("lambda",
                      "alpha")

hp_features_reservoir <- c("input_scaling",
                           "spectral_radius",
                           "leaking_rate",
                           "ridge")
hp_features <- c("hosp",
                 "P_TOUS_AGES",
                 "P_60_90_PLUS_ANS",
                 "FRACP_TOUS_AGES",
                 "FRACP_60_90_PLUS_ANS",
                 "URG_covid_19_COUNT",
                 "IPTCC.mean",
                 "Vaccin_1dose",
                 "hosp_rolDeriv7",
                 "P_TOUS_AGES_rolDeriv7",
                 "P_60_90_PLUS_ANS_rolDeriv7",
                 "FRACP_TOUS_AGES_rolDeriv7",
                 "FRACP_60_90_PLUS_ANS_rolDeriv7",
                 "URG_covid_19_COUNT_rolDeriv7",
                 "IPTCC.mean_rolDeriv7",
                 "Vaccin_1dose_rolDeriv7")

feature_labels <- conservation_status <- c(
  "outcome" = "Hospitalisations t+14",
  "outcomeDeriv" = "Outcome",
  "input_scaling" = "Input scaling",
  "leaking_rate" = "Leaking rate",
  "ridge" = "Ridge",
  "spectral_radius" = "Spectral radius",
  "hosp" = "Hospitalizations",
  "P_TOUS_AGES" = "RT-PCR+",
  "P_60_90_PLUS_ANS" = "RT-PCR+ 60 yo+",
  "FRACP_TOUS_AGES" = "% RT-PCR+",
  "FRACP_60_90_PLUS_ANS" = "% RT-PCR+ 60 yo+",
  "URG_covid_19_COUNT" = "Emergency",
  "IPTCC.mean" = "IPTCC",
  "Vaccin_1dose" = "Vaccine",
  "hosp_rolDeriv7" = "Hospitalizations (d)",
  "P_TOUS_AGES_rolDeriv7" = "RT-PCR+ (d)",
  "P_60_90_PLUS_ANS_rolDeriv7" = "RT-PCR+ 60 yo+ (d)",
  "FRACP_TOUS_AGES_rolDeriv7" = "% RT-PCR+ (d)",
  "FRACP_60_90_PLUS_ANS_rolDeriv7" = "% RT-PCR+ 60 yo+ (d)",
  "URG_covid_19_COUNT_rolDeriv7" = "Emergency (d)",
  "IPTCC.mean_rolDeriv7" = "IPTCC (d)",
  "Vaccin_1dose_rolDeriv7" = "Vaccine (d)"
)

model_labels <- c(
  "common_input_scaling" = "Common IS \n\n R %>>% O",
  "common_input_scaling_linked_source" = "Common IS \n\n I + R %>>% O",
  "multiple_input_scaling" = "Multiple IS \n\n R %>>% O",
  "multiple_input_scaling_linked_source" = "Multiple IS \n\n I + R %>>% O",
  "enet" = "Elastic-net",
  "enet_hp_on_train_set" = "Elastic-net",
  "enet_daily_hp_update" = "Elastic-net (hp daily updated)"
)

##### present data
data_i <- fct_smoothing_derivative(data = df_obfuscated_epidemio,
                                   maxOutcomeDate = df_obfuscated_epidemio$START_DATE %>% max,
                                   forecast_days = 14)

plot_present_data <- data_i %>%
  select(-START_DATE, -outcomeHosp, -ends_with("rolDeriv7")) %>%
  tidyr::pivot_longer(cols = -"outcomeDate") %>%
  mutate(name = factor(name, levels = names(feature_labels), labels = feature_labels)) %>%
  ggplot(mapping = aes(x = outcomeDate, y = value, color = name)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2021-03-01"), linetype = 2) +
  scale_color_manual(values = c(rep("#FB8500",2), rep("#023047", 16))) +
  scale_x_date(date_breaks = "5 months", date_labels =  "%m-%y") +
  facet_wrap(name ~ ., scales = "free",
             ncol = 2) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Date",
       y = "Value")

##### hyperparameters plots
df_hp <- readRDS(hp_file) %>%
  bind_rows(.id = "model") %>%
  group_by(model) %>%
  dplyr::mutate(rank_perf = dense_rank(mean_absolute_error),
                n_try = n(),
                rank_perf_color = case_when(rank_perf == 1 ~ "Best",
                                            rank_perf <= 10 ~ "Top 10",
                                            rank_perf <=  40 ~ "Top 40",
                                            rank_perf > 40 ~ "Others"),
                rank_perf_color = factor(rank_perf_color, levels = c("Best", "Top 10", "Top 40", "Others")),
                mean_absolute_error = if_else(mean_absolute_error >= 30, 30, mean_absolute_error)) %>%
  ungroup() %>%
  dplyr::select(all_of(c("mean_absolute_error",
                         "rank_perf_color",
                         "model",
                         hp_features_reservoir,
                         hp_features,
                         hp_features_enet))) %>%
  tidyr::pivot_longer(cols = all_of(c(hp_features_reservoir, hp_features, hp_features_enet)), 
                      values_to = "HP_value", names_to = "HP") %>%
  na.omit() %>%
  filter(mean_absolute_error < 30)

plot_enet_hp <- df_hp %>%
  filter(HP %in% hp_features_enet) %>%
  ggplot(mapping = aes(x = HP_value, 
                       y = mean_absolute_error,
                       color = rank_perf_color)) +
  geom_point() +
  facet_grid(model ~ HP, scales = "free_x",
             labeller = labeller(model = model_labels)) +
  scale_x_log10(labels = scales::trans_format("log10", scales::label_math(10^.x)),
                breaks = 10^seq(-10, 10, 1),
                minor_breaks = 10^(seq(-10, 5))) +
  scale_color_manual(values = c("blue", "#E76F51", "#E9C46A", "#2A9D8F")) +
  lims(y = c(15, 30)) +
  theme_minimal() + 
  theme(strip.text.y = element_text(angle = 0),
        strip.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom") +
  labs(x = "Hyperparameter value", 
       y = "Mean absolute error",
       color = "") 

plot_reservoir_hp <- df_hp %>%
  filter(HP %in% hp_features_reservoir) %>%
  ggplot(mapping = aes(x = HP_value, 
                       y = mean_absolute_error,
                       color = rank_perf_color,
                       alpha = rank_perf_color)) +
  geom_point() +
  facet_grid(model ~ HP, scales = "free_x",
             labeller = labeller(model = model_labels,
                                 HP = feature_labels)) +
  scale_x_log10(labels = scales::trans_format("log10", scales::label_math(10^.x)),
                breaks = c(1e-10, 1e-5, 1e0, 1e5, 1e10),
                minor_breaks = 10^(seq(-10, 5))) +
  scale_color_manual(values = c("blue", "#E76F51", "#E9C46A", "#2A9D8F")) +
  scale_alpha_manual(values = c(1, 1, 1, 0.1)) +
  lims(y = c(15, 30)) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0),
        strip.text.y = element_text(angle = 0),
        strip.text.x = element_text(angle = 90)) +
  labs(x = "Hyperparameter value", 
       y = "Mean absolute error",
       color = "",
       alpha = "") 

plot_feature_input_scaling <- df_hp %>%
  filter(HP %in% hp_features,
         model == "multiple_input_scaling_linked_source") %>%
  ggplot(mapping = aes(x = HP_value, 
                       y = mean_absolute_error,
                       color = rank_perf_color,
                       alpha = rank_perf_color)) +
  geom_point() +
  facet_wrap(HP ~ ., scales = "free_x",
             labeller = labeller(model = model_labels,
                                 HP = feature_labels),
             ncol = 3) +
  scale_x_log10(labels = scales::trans_format("log10", scales::label_math(10^.x)),
                breaks = c(1e-10, 1e-5, 1e0, 1e5, 1e10),
                minor_breaks = 10^(seq(-10, 5))) +
  scale_y_continuous(breaks = c(15, 20, 30)) +
  scale_color_manual(values = c("blue", "#E76F51", "#E9C46A", "#2A9D8F")) +
  scale_alpha_manual(values = c(1, 1, 1, 0.1)) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0)) +
  labs(x = "Hyperparameter value", 
       y = "Mean absolute error",
       color = "",
       alpha = "")

##### importance plots
df_importance <- lapply(ls_files_results_res, function(x) readRDS(x)$dfImp) %>%
  dplyr::bind_rows()

df_importance_no_reservoir <- df_importance %>%
  dplyr::mutate(model = factor(model,
                               levels = names(model_labels),
                               labels = model_labels),
                feature = factor(feature,
                                 levels = names(feature_labels),
                                 labels = feature_labels)) %>%
  filter(!is.na(feature)) %>%
  group_by(feature, model, hp_set) %>%
  summarise(importance = mean(importance), .groups = "drop")

ordered_feature_imp <- df_importance_no_reservoir %>%
  filter(model == "Elastic-net") %>%
  arrange(importance) %>%
  pull(feature) %>%
  as.character()

S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)

# mean feature coefficient by hp set and by model.
plot_importance <- df_importance_no_reservoir %>%
  mutate(feature = factor(feature, levels = ordered_feature_imp)) %>%
  ggplot(mapping = aes(y = feature, color = model, x = importance)) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  scale_alpha_manual(values = c(0.2, 0.2, 1)) +
  scale_x_continuous(trans = "S_sqrt", breaks = c(-100, -50, -10, -2, 0, 2, 10, 50, 100)) +
  scale_color_manual(values = c("#023047", "#8ECAE6", "#FB8500")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color=guide_legend(nrow=2)) +
  labs(x = "Coefficient", y = "", color = "", fill = "")

df_importance_reservoir <- df_importance %>%
  dplyr::mutate(model = factor(model,
                               levels = names(model_labels),
                               labels = model_labels),
                feature = factor(feature,
                                 levels = c(names(feature_labels), paste0("reservoir", 1:500)),
                                 labels = c(feature_labels, paste0("reservoir", 1:500)))) %>%
  filter(model != "Elastic-net") %>%
  select(outcomeDate, model, hp_set, feature, importance) %>%
  group_by(outcomeDate, model, hp_set) %>%
  mutate(rank = dense_rank(desc(abs(importance)))) %>%
  ungroup() %>%
  filter(feature %in% feature_labels) %>%
  mutate(feature = factor(feature, levels = ordered_feature_imp))

# feature importance by hp set and by esn model.
nb_features = df_importance$feature %>% unique() %>% length()
plot_reservoir_importance <- df_importance_reservoir %>%
  ggplot(mapping = aes(y = feature, color = model, x = rank)) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  scale_alpha_manual(values = c(0.2, 0.2, 1)) +
  scale_color_manual(values = c("#023047", "#8ECAE6")) +
  scale_fill_manual(values = c("#023047", "#8ECAE6")) +
  scale_x_reverse(breaks = c(1, 100, 200, 300, 400, nb_features)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Rank", y = "", color = "", fill = "")

##### forecast plots
df_forecast <- lapply(ls_files_results_res, function(x) readRDS(x)$dfPred) %>%
  dplyr::bind_rows()

df_forecast_aggregated <- df_forecast %>%
  group_by(START_DATE, outcomeDate, model) %>%
  summarise(outcome = unique(outcome),
            forecast = median(forecast),
            hosp = unique(hosp),
            .groups = "drop") %>%
  dplyr::mutate(model = factor(model,
                               levels = names(model_labels),
                               labels = model_labels))

table_perf_esn <- df_forecast_aggregated %>%
  dplyr::mutate(forecast = if_else(forecast < 10, 10, forecast),
                hosp = if_else(hosp < 10, 10, hosp),
                outcome = if_else(outcome < 10, 10, outcome)) %>%
  dplyr::mutate(absolute_error = abs(forecast - outcome),
                relative_error = absolute_error/outcome,
                baseline_absolute_error = abs(hosp - outcome),
                absolute_error_delta_baseline = absolute_error - baseline_absolute_error,
                absolute_error_relative_baseline = absolute_error/baseline_absolute_error) %>%
  group_by(model) %>%
  summarise(mae = mean(absolute_error),
            mre = median(relative_error, na.rm = TRUE),
            mae_baseline = mean(absolute_error_delta_baseline),
            mre_baseline = median(absolute_error_relative_baseline, na.rm = TRUE),
            .groups = "drop")

plot_forecast <- df_forecast_aggregated %>%
  tidyr::pivot_longer(cols = c(outcome, forecast, hosp)) %>%
  mutate(name = factor(name,
                       levels = c("outcome",
                                  "hosp",
                                  "forecast"),
                       labels = c("Outcome",
                                  "Baseline",
                                  "Model forecast"))) %>%
  ggplot(mapping = aes(x = outcomeDate, y = value, color = name)) +
  geom_line() +
  scale_color_manual(values = c("#023047", "#8ECAE6", "#FB8500")) +
  scale_x_date(date_breaks = "3 months", date_labels =  "%m-%y") +
  facet_wrap(model ~ ., ncol = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date (month-year)",
       y = "Hospitalizations",
       color = "")

##### aggregation forecast plots
nb_aggregation <- c(1, 5, 10, 15, 20, 30, 40)
nb_boot <- 250

df_aggregation_forecast <- parallel::mclapply(seq_len(nb_boot),
                                              mc.cores = parallel::detectCores()-2, 
                                              function(boot_i){
                                                lapply(nb_aggregation,
                                                       function(nb_aggregation_i){
                                                         df_forecast %>%
                                                           filter(!grepl(x = model, pattern = "enet")) %>%
                                                           select(model, rank) %>%
                                                           distinct() %>%
                                                           group_by(model) %>%
                                                           sample_n(nb_aggregation_i, replace = TRUE) %>%
                                                           left_join(df_forecast,
                                                                     by = c("model", "rank"),
                                                                     relationship = "many-to-many") %>%
                                                           group_by(START_DATE, outcomeDate, model) %>%
                                                           summarise(outcome = unique(outcome),
                                                                     forecast = median(forecast),
                                                                     hosp = unique(hosp),
                                                                     .groups = "drop") %>%
                                                           dplyr::mutate(model = factor(model,
                                                                                        levels = names(model_labels),
                                                                                        labels = model_labels)) %>%
                                                           dplyr::mutate(absolute_error = abs(forecast - outcome),
                                                                         relative_error = absolute_error/outcome,
                                                                         baseline_absolute_error = abs(hosp - outcome),
                                                                         absolute_error_delta_baseline = absolute_error - baseline_absolute_error,
                                                                         absolute_error_relative_baseline = absolute_error/baseline_absolute_error) %>%
                                                           group_by(model) %>%
                                                           summarise(mae = mean(absolute_error),
                                                                     mre = median(relative_error, na.rm = TRUE),
                                                                     mae_baseline = mean(absolute_error_delta_baseline),
                                                                     mre_baseline = median(absolute_error_relative_baseline, na.rm = TRUE),
                                                                     .groups = "drop") %>%
                                                           mutate(nb_aggregation = nb_aggregation_i)
                                                       }) %>%
                                                  bind_rows() %>%
                                                  mutate(boot_i = boot_i)
                                              }) %>%
  bind_rows()

plot_aggregation <- df_aggregation_forecast %>%
  mutate(mae = if_else(mae > 50, 50, mae)) %>%
  group_by(model, nb_aggregation) %>%
  summarise(mae_mean = mean(mae),
            mae_inf = quantile(mae, 0.025),
            mae_sup = quantile(mae, 0.975)) %>%
  ggplot(mapping = aes(x = nb_aggregation,
                       y = mae_mean,
                       ymin = mae_inf,
                       ymax = mae_sup,
                       group = model)) +
  geom_ribbon(fill = "grey", color = NA) +
  geom_point() +
  geom_line() +
  facet_wrap(model ~ .) +
  lims(y = c(0,50)) +
  theme_minimal() +
  labs(x = "Nb of repetition of the model with the best hp set",
       y = "Mean absolute error")

plot_before_aggregation_forecast <- df_forecast %>%
  filter(!grepl(x = model, "^enet")) %>%
  mutate(forecast = if_else(forecast > 200, 200, forecast)) %>%
  ggplot(mapping = aes(x = outcomeDate, y = forecast, color = "Model forecast")) +
  geom_point(alpha = 0.1) +
  geom_line(mapping = aes(y = outcome, color = " Outcome")) +
  geom_line(mapping = aes(y = hosp, color = "Baseline")) +
  scale_color_manual(values = c("#023047", "#8ECAE6", "#FB8500")) +
  scale_x_date(date_breaks = "3 months", date_labels =  "%m-%y") +
  facet_wrap(model ~ ., ncol = 2, labeller = labeller(model = model_labels)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date (month-year)",
       y = "Hospitalizations",
       color = "")

saveRDS(object = list(plot_present_data = plot_present_data,
                      plot_enet_hp = plot_enet_hp,
                      plot_reservoir_hp = plot_reservoir_hp,
                      plot_feature_input_scaling = plot_feature_input_scaling,
                      table_perf_esn = table_perf_esn,
                      plot_before_aggregation_forecast = plot_before_aggregation_forecast,
                      plot_aggregation = plot_aggregation,
                      plot_forecast = plot_forecast,
                      plot_reservoir_importance = plot_reservoir_importance,
                      plot_importance = plot_importance),
        file = "data/precomputed_results.rds")