#' Multivariate Time Series function
#'
#' Dimension reduction for multivariate time series using sliced inverse regression.
#' @param X Predictors
#' @param y Target
#' @keywords TS
#' @export
#'
TSIR <- function(X, y, n_slices = 10, n_lags = 12){
  output <- matrix(nrow = n_lags, ncol = ncol(X))
  for (i in 1:n_lags){
    # Leading the y up, removeing the relevent NA
    output[i,] <- AlgoTSIR(X, y, n_slices = n_slices, n_lags = n_lags)
    y <- leadTSIR(y)[-length(y)]
    X <- X[-nrow(X),]
  }
  colnames(output) <- colnames(X)
  result <- output / sum(output)
  return(result)
}
