AlgoTSIR <- function(X, y, n_slices = 10, dimension_threshold = 0.8, n_dimensions = 5, n_lags = 12){
  # For Laggers, lets return all the values.
  # Then we can implement a 0.8/number of dimensions hyperparameter.
  # Brings package together.
  # Checks data, stops if there is an error.
  PreProcessTSIR(X = X, y = y)

  # Standardises the X values.
  X_stand <- StandardTSIR(X) # Add in errors if NA's here.

  # Gets the discretized y values with respect to the number of slices.
  y_disc <- DiscretizeTSIR(y, n_slices)

  # Computing group means and returns the V matrix.
  out_matrix <- MeansTSIR(X_stand, y_disc, 10)

  # Compute an eigenvalue decomposition and keep the index.
  idx <- rank(-abs(colMeans(out_matrix)))
  eigen_values <- eigen(out_matrix)$values[idx]

  # These eigens should be in the same order as the columns they correspond
  return(eigen_values)
}


