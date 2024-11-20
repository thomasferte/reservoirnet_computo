timestart = Sys.time()
print(timestart)
##### LOAD PACKAGES
library(dplyr)
library(reservoirnet)
##### LOAD FUNCTIONS
invisible(lapply(list.files(here::here("functions/"), full.names = TRUE), source))
##### LOAD DATA (only data before 2021-03-01 for learning hp)
lsdataDates <- fct_load_data_find_hp()
##### META PARAMETERS
n_samples = 2000
########################## COMMON INPUT SCALING ##########################
##### SAMPLE HYPERPARAMETERS
dfHyperparam <- reservoirnet::random_search_hyperparam(n = n_samples,
                                                       ls_fct = list(lambda = function(n) reservoirnet::rloguniform(n = n, min = 1e-10, max = 1e5),
                                                                     alpha = function(n) runif(n = n, min = 0, max = 1))) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-search_id)
##### COMPUTE MEAN ABSOLUTE ERROR
dfres <- parallel::mclapply(seq_len(n_samples),
                            mc.cores = parallel::detectCores()-2,
                            function(id_sample){
                              print(paste0("----------------- id_sample : ", id_sample))
                              time_start <- Sys.time()
                              mean_absolute_error <- fct_objective(data_covid = lsdataDates$data_covid,
                                                                   model = "enet",
                                                                   vecDates = lsdataDates$vecDates,
                                                                   forecast_days = 14,
                                                                   lambda = dfHyperparam$lambda[id_sample],
                                                                   alpha = dfHyperparam$alpha[id_sample])
                              time_end <- Sys.time()
                              res <- dfHyperparam[id_sample,] %>%
                                dplyr::mutate(mean_absolute_error = mean_absolute_error,
                                              time = difftime(time_end, time_start, units = "secs") %>% as.numeric(),
                                              link_source = link_source,
                                              nb_iter = 1)
                              return(res)
                            }) %>%
  bind_rows()

##### SAVE RESULTS
fct_save_results(subDir = paste0("data/enet_", 1),
                 slar_taskid = 1,
                 object = dfres)

timeend = Sys.time()
print(timeend)

