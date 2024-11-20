#' fct_train_test_sets
#'
#' @description Split data_i into train and test sets
#'
#' @param data_i the data to be splitted
#'
#' @return A list with X, Y and Xtest
fct_train_test_sets <- function(data_i){
  train_set <- data_i %>%
    dplyr::filter(outcomeDate <= max(START_DATE)) %>%
    dplyr::select(-dplyr::contains("date")) %>%
    janitor::remove_constant()
  
  X = train_set %>%
    dplyr::select(-dplyr::contains("outcome")) %>%
    as.matrix()
  
  Y = train_set %>%
    dplyr::select(outcomeDeriv) %>%
    as.matrix()
  
  Xtest <- data_i %>%
    dplyr::select(colnames(train_set)) %>%
    dplyr::select(-dplyr::contains("outcome")) %>%
    as.matrix()
  
  res <- list(X = X, Y = Y, Xtest = Xtest)
  
  return(res)
}
