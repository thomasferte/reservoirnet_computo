#' fct_aggregate_forecast
#'
#' @description Function to add forecast_deriv to the initial dataframe and retrieve only usefull columns.
#'
#' @param data_i The dataset
#' @param forecast_deriv The forecast derivative
#'
#' @return A dataframe with date, outcome date, outcome, forecast and hospitalisations
fct_aggregate_forecast <- function(data_i,
                                   forecast_deriv){
  data_i %>%
    dplyr::mutate(forecast_deriv = forecast_deriv,
                  forecast = forecast_deriv + outcomeHosp,
                  forecast = if_else(forecast < 0, 0, forecast)) %>%
    dplyr::slice_max(outcomeDate) %>%
    dplyr::select(START_DATE, outcomeDate, outcome, forecast, hosp = outcomeHosp) %>%
    return()
}

