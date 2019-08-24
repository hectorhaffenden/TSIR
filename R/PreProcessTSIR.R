PreProcessTSIR <- function(X = X, y = y){
  # This function performs some data checks.
  error_number = 0
  # Is X a data frame or matrix?
  if (!is.data.frame(X) & !is.matrix(X)){
    error_number = error_number + 1
    warning("X is not a dataframe or a matrix, please fix.")
  }
  # Is y a vector?
  if (!is.vector(y_train)){
    error_number = error_number + 1
    warning("y is not a vector, please fix.")
  }
  # Is the length of y the same as the number of rows as X?
  if (length(y) != nrow(X)){
    error_number = error_number + 1
    warning("The length of y does not match the number of rows in X, please fix.")
  }
  # Are there any NAs in X?
  if (!all(!is.na(X))){
    error_number = error_number + 1
    warning("Your X data contains NAs, please fix.")
  }
  # Are there any NAs in y
  if (!all(!is.na(y))){
    error_number = error_number + 1
    warning("Your y data contains NAs, please fix.")
  }
  # If any errors occur, we stop, else return 0 and keep going.
  if (error_number > 0){
    stop("Refer to the error messages, please fix.")
  } else {
  }
}
