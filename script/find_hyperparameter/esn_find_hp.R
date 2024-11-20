timestart = Sys.time()
print(timestart)
##### LOAD PACKAGES
library(dplyr)
library(reservoirnet)
##### SLAR VARIABLES
slar_taskid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
slar_jobid <- as.numeric(Sys.getenv("SLURM_ARRAY_JOB_ID"))
slar_job_name <- Sys.getenv("SLURM_JOB_NAME")
slar_task_max <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_MAX"))
##### LOAD FUNCTIONS
invisible(lapply(list.files(here::here("functions/"), full.names = TRUE), source))
##### LOAD DATA (only data before 2021-03-01 for learning hp)
lsdataDates <- fct_load_data_find_hp()
##### META PARAMETERS
nb_iter <- regexpr("\\d+", slar_job_name) %>%
  regmatches(slar_job_name, .) %>%
  as.numeric()
link_source <- regexpr(pattern = "FALSE|TRUE", slar_job_name) %>%
  regmatches(slar_job_name, .) %>%
  as.logical()
if(is.na(nb_iter)) stop("nb_iter cannot be extracted from slar_job_name")
n_samples = 2000/slar_task_max
########################## MULTIPLE INPUT SCALING ##########################
##### SAMPLE HYPERPARAMETERS
## generate hp sampling function for reservoir hp
df_fct_hp <- list(ridge = function(n) reservoirnet::rloguniform(n = n, min = 1e-10, max = 1e5),
                  spectral_radius = function(n) reservoirnet::rloguniform(n = n, min = 1e-5, max = 1e5),
                  leaking_rate = function(n) reservoirnet::rloguniform(n = n, min = 1e-3, max = 1),
                  seed = function(n) round(runif(n = n, min = 0, max = 1e3))) %>%
  reservoirnet::random_search_hyperparam(n = n_samples,
                                         ls_fct = .) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-search_id)
## generate hp sampling function for multiple input scaling
# get all features
vec_features <- fct_smoothing_derivative(lsdataDates$data_covid,
                                         maxOutcomeDate = max(lsdataDates$data_covid$START_DATE)) %>%
  dplyr::select(-matches(match = "outcome|date")) %>%
  colnames()
# one sampling function per feature
ls_fct_is <- NULL
for(feature in vec_features){
  ls_fct_is[[feature]] <- function(n) reservoirnet::rloguniform(n = n, min = 1e-5, max = 1e5)
}
# sample one value hp for each feature
df_fct_is <- ls_fct_is %>%
  reservoirnet::random_search_hyperparam(n = n_samples,
                                         ls_fct = .) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-search_id)
## merge both
dfHyperparam <- cbind(df_fct_hp, df_fct_is)
##### COMPUTE MEAN ABSOLUTE ERROR
dfres <- lapply(seq_len(n_samples),
                function(id_sample){
                  print(paste0("---------------- id_sample : ", id_sample))
                  time_start <- Sys.time()
                  mean_absolute_error <- fct_objective(lsdataDates$data_covid,
                                                       lsdataDates$vecDates,
                                                       warmup = 30,
                                                       forecast_days = 14,
                                                       units = 500,
                                                       lr = dfHyperparam$leaking_rate[id_sample],
                                                       sr = dfHyperparam$spectral_radius[id_sample],
                                                       ridge = dfHyperparam$ridge[id_sample],
                                                       input_scaling = df_fct_is[id_sample,],
                                                       link_source = link_source,
                                                       nb_iter = nb_iter,
                                                       seed = dfHyperparam$seed[id_sample])
                  time_end <- Sys.time()
                  res <- dfHyperparam[id_sample,] %>%
                    dplyr::mutate(mean_absolute_error = mean_absolute_error,
                                  time = difftime(time_end, time_start, units = "secs") %>% as.numeric(),
                                  link_source = link_source,
                                  nb_iter = nb_iter)
                  return(res)
                }) %>%
  bind_rows()

##### SAVE RESULTS
fct_save_results(subDir = paste0("data/multiple_input_scaling_", slar_jobid),
                 slar_taskid = slar_taskid,
                 object = dfres)

########################## COMMON INPUT SCALING ##########################
##### SAMPLE HYPERPARAMETERS
dfHyperparam <- reservoirnet::random_search_hyperparam(n = n_samples,
                                                       ls_fct = list(ridge = function(n) reservoirnet::rloguniform(n = n, min = 1e-10, max = 1e5),
                                                                     input_scaling = function(n) reservoirnet::rloguniform(n = n, min = 1e-5, max = 1e5),
                                                                     spectral_radius = function(n) reservoirnet::rloguniform(n = n, min = 1e-5, max = 1e5),
                                                                     leaking_rate = function(n) reservoirnet::rloguniform(n = n, min = 1e-3, max = 1),
                                                                     seed = function(n) round(runif(n = n, min = 0, max = 1e3)))) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-search_id)
##### COMPUTE MEAN ABSOLUTE ERROR
dfres <- lapply(seq_len(n_samples),
                function(id_sample){
                  print(paste0("----------------- id_sample : ", id_sample))
                  time_start <- Sys.time()
                  mean_absolute_error <- fct_objective(data_covid = lsdataDates$data_covid,
                                                       vecDates = lsdataDates$vecDates,
                                                       warmup = 30,
                                                       forecast_days = 14,
                                                       units = 500,
                                                       lr = dfHyperparam$leaking_rate[id_sample],
                                                       sr = dfHyperparam$spectral_radius[id_sample],
                                                       ridge = dfHyperparam$ridge[id_sample],
                                                       input_scaling = dfHyperparam$input_scaling[id_sample],
                                                       nb_iter = nb_iter,
                                                       seed = dfHyperparam$seed[id_sample])
                  time_end <- Sys.time()
                  res <- dfHyperparam[id_sample,] %>%
                    dplyr::mutate(mean_absolute_error = mean_absolute_error,
                                  time = difftime(time_end, time_start, units = "secs") %>% as.numeric(),
                                  link_source = link_source,
                                  nb_iter = nb_iter)
                  return(res)
                }) %>%
  bind_rows()

##### SAVE RESULTS
fct_save_results(subDir = paste0("data/common_input_scaling_", slar_jobid),
                 slar_taskid = slar_taskid,
                 object = dfres)

timeend = Sys.time()
print(timeend)
print(timestart, timeend, units = "hour")

