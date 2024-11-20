#' fct_objective
#'
#' @description The objective function optimised to find hyperparameters
#'
#' @param data_covid The data
#' @param vecDates The vector of dates, each date in this vector will be used to train and forecast.
#' @param warmup The reservoir warmup
#' @param forecast_days The forecast horizon
#' @param units The number of units in the reservoir
#' @param lr The leaking rate
#' @param sr The spectral radius
#' @param ridge The ridge penalty
#' @param input_scaling The input scaling
#' @param seed The seed setting the reservoir connection
#' @param link_source Should the input be linked to the target
#' @param model esn or enet
#' @param nb_iter Number of replication of the model
#' @param lambda The lambda elastic-net (use cv.glmnet if not supplied)
#' @param alpha The alpha parameter of elastic-net
#'
#' @return The mean absolute error
fct_objective <- function(data_covid,
                          vecDates,
                          warmup = 30,
                          forecast_days = 14,
                          link_source = FALSE,
                          units = 500,
                          model = "esn",
                          lr = 0.5,
                          sr = 1,
                          ridge = 1e2,
                          input_scaling = 1,
                          seed = 1,
                          nb_iter = 1,
                          lambda = NULL,
                          alpha = 0.5){
  # iterate over each date in vecDates to train and forecast with a reservoir
  fct_iterative_forecast_from_hp(data_covid = data_covid,
                                 vecDates = vecDates,
                                 link_source = link_source,
                                 model = model,
                                 warmup = warmup,
                                 forecast_days = forecast_days,
                                 units = units,
                                 lr = lr,
                                 sr = sr,
                                 ridge = ridge,
                                 input_scaling = input_scaling,
                                 seed = seed,
                                 nb_iter = nb_iter,
                                 lambda = lambda,
                                 alpha = alpha) %>%
    # take the median forecast if several forecast (nb_iter > 1)
    group_by(START_DATE, outcomeDate, outcome, hosp) %>%
    summarise(forecast = median(forecast), .groups = "drop") %>%
    # get the MAE
    dplyr::mutate(absolute_error = abs(forecast - outcome)) %>%
    dplyr::pull(absolute_error) %>%
    mean() %>%
    return()
}

