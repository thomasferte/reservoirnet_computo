#' fct_load_data_find_hp
#'
#' @description Function to load data and get the list of dates and the loaded data
#'
#' @param max_date_eval The max date to be evaluated
#' @param data_file The data path
#'
#' @return A list with the vector of dates to be evaluated and the data
fct_load_data_find_hp <- function(max_date_eval = "2021-03-01",
                                  data_file = "data/df_obfuscated_epidemio.rds"){
  data_covid <- readRDS(file = data_file) %>%
    dplyr::filter(START_DATE <= max_date_eval)
  ##### SELECT TRAINING DATES
  # skip first 90 days before learning start
  # (only one date over 2 to speed up hp selection)
  vecDates <- data_covid$START_DATE[90:length(data_covid$START_DATE)]
  vecDates <- vecDates[as.numeric(vecDates) %% 2 == 0]
  
  return(list(data_covid = data_covid,
              vecDates = vecDates))
}