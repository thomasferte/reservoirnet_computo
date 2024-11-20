#' fct_fit_esn
#'
#' @description Function to fit reservoir depending on hyperparameters and forecast on Xtest
#'
#' @param X The X matrix
#' @param Y The outcome
#' @param units The number of nodes (default = 500)
#' @param warmup The warmup length
#' @param lr The leaking rate
#' @param sr The spectral radius
#' @param ridge The ridge penalty
#' @param input_scaling The input scaling
#' @param Xtest The test X matrix
#' @param seed The seed determining reservoir connections
#' @param link_source Should the input layer be directly linked to output layer (default FALSE)
#' @param importance Should the feature importance be returned (defaut is FALSE)
#'
#' @import reservoirnet
#'
#' @return the reservoir forecast
#' @export
fct_fit_esn <- function(X,
                        Xtest,
                        Y,
                        units = 500,
                        link_source = FALSE,
                        warmup,
                        lr,
                        sr,
                        ridge,
                        input_scaling,
                        seed,
                        importance = FALSE){
  ##### setup reservoir
  ## check if input scaling is a dataframe, if so, select the correct order of input scaling 
  if(is.data.frame(input_scaling)){
    input_scaling <- input_scaling %>%
      dplyr::select(colnames(X)) %>%
      as.numeric()
  }
  reservoir <-
    reservoirnet::createNode(
      nodeType = "Reservoir",
      units = as.integer(units),
      lr = lr,
      sr = sr,
      input_scaling = input_scaling,
      seed = seed
    )
  readout <- reservoirnet::createNode(nodeType = "Ridge",
                                      ridge = ridge)
  if(link_source){
    source <- createNode("Input")
    model <- list(source %>>% reservoir, source) %>>% readout
  } else {
    model <- reservoirnet::link(reservoir, readout)
  }
  ##### fit reservoir
  fit <- reservoirnet::reservoirR_fit(node = model, X = X, Y = Y, warmup = warmup)
  ##### forecast
  res <- reservoirnet::predict_seq(node = fit$fit,
                                   X = Xtest,
                                   reset = TRUE)
  res <- as.numeric(res)
  ##### return results
  if(importance){
    dfimp = model$get_param(readout$name)$Wout %>%
      as.matrix() %>%
      as.data.frame() %>%
      mutate(feature = paste0("reservoir", 1:as.numeric(units)), .before = 1)
    
    if(link_source){
      size_concatenator = model$nodes[2]
      if(size_concatenator$input_dim[0] == ncol(X) & size_concatenator$input_dim[1] == as.numeric(units)){
        dfimp <- dfimp %>%
          mutate(feature = c(colnames(X), paste0("reservoir", 1:as.numeric(units))), .before = 1)
      } else if(size_concatenator$input_dim[0] == as.numeric(units) & size_concatenator$input_dim[1] == ncol(X)){
        dfimp <- dfimp %>%
          mutate(feature = c(paste0("reservoir", 1:as.numeric(units)), colnames(X)), .before = 1)
      }
    }
    colnames(dfimp) <- c("feature", "importance")
    return(list(pred = res,
                dfimp = dfimp))
  } else {
    return(res)
  }
}
