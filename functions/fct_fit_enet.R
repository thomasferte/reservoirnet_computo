#' fct_fit_enet
#' 
#' @description Function to fit elastic-net regression
#'
#' @param X The training features
#' @param Xtest The test features
#' @param Y The training outcome
#' @param lambda The lambda elastic-net (use cv.glmnet if not supplied)
#' @param alpha The alpha parameter of elastic-net
#' @param importance Should the feature importance be returned (defaut is FALSE)
#'
#' @return A vector with elastic-net predictions or a list with feature importance and predictions.
fct_fit_enet <- function(X, Xtest, Y, lambda = NULL, alpha = 0.5, importance = FALSE){
  if(is.null(lambda)){
    enet <- glmnet::cv.glmnet(x = X,
                              y = Y,
                              alpha = alpha,
                              family = "gaussian")
    res <- predict(enet, newx = Xtest, s = "lambda.1se") %>%
      as.numeric()
  } else {
    enet <- glmnet::glmnet(x = X,
                           y = Y,
                           lambda = lambda,
                           alpha = alpha,
                           family = "gaussian")
    res <- predict(enet, newx = Xtest) %>%
      as.numeric()
  }
  dfimp <- coef(enet) %>%
    as.matrix() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()
  colnames(dfimp) <- c("feature", "importance")
  
  if(importance){
    return(list(pred = res,
                dfimp = dfimp))
  } else {
    return(res)
  }
}
