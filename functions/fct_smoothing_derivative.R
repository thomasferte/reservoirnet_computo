#' fct_smoothing_derivative
#' 
#' Compute the ouctome, smooth other features and compute first derivatives
#'
#' @param data The dataframe with hosp and START_DATE columns
#' @param forecast_days The forecast horizon
#' @param span_days The nearby points used for smoothing
#' @param derivative_days The number of days the derivative should be computed
#'
#' @return A dataframe with outcome, outcomeDate and first derivatives columns
#' with smoothed explanatory features.
fct_smoothing_derivative <- function(data,
                                     maxOutcomeDate,
                                     forecast_days = 14,
                                     span_days = 21,
                                     derivative_days = 7){
  data_filtered <- data %>%
    arrange(START_DATE) %>%
    # compute outcome and outcome date (forecast at 14 days)
    dplyr::mutate(outcome = lead(hosp, forecast_days),
                  outcomeDate = lead(START_DATE, forecast_days)) %>%
    # remove future data from smoothing
    dplyr::filter(outcomeDate <= maxOutcomeDate)
  
  # set smoothing parameter
  span = span_days/nrow(data_filtered)
  
  data_filtered %>%
    # smooth data
    mutate(across(where(is.double) & !any_of(c("START_DATE", "outcome", "outcomeDate")),
                  function(x){
                    dblSTART_DATE <- as.numeric(START_DATE)
                    loess(x ~ dblSTART_DATE,
                          data = .,
                          surface = "direct",
                          span = span,
                          degree = 1)$fitted
                  } )) %>%
    # compute first derivative
    mutate(across(where(is.double) & !any_of(c("START_DATE", "outcome", "outcomeDate")),
                  list(rolDeriv7 = function(x){
                    (x-lag(x, n = (derivative_days-1)))/derivative_days
                  }))) %>%
    # remove missing values due to derivative computation
    na.omit() %>%
    # get the outcome derivative
    mutate(outcomeDeriv = outcome - hosp,
           outcomeHosp = hosp) %>%
    mutate(across(!START_DATE & !outcome & !outcomeDate & !outcomeDeriv & !outcomeHosp,
                  function(x){
                    maxX <- max(abs(x))
                    if(maxX == 0){
                      return(x)
                    } else {
                      return(x/maxX)
                    }
                  })) %>%
    return()
}

