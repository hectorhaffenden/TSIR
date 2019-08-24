MeansTSIR <- function(X_stand, y_disc, n_slices){


  mean_matrix <- matrix(nrow = n_slices, ncol = ncol(X_stand))
  weights <- vector(length = n_slices)

  n_instances <- length(y_disc)

  # Using base R
  for (i in 1:n_slices){
    index <- y_disc == i
    mean_matrix[i,] <- colMeans(X_stand[index,]) # Each row is a difference slice
    weights[i] <- sum(index) / n_instances
  }


  # Now for the V matrix.
  V <- weights[1] * (mean_matrix[1,] %*% t(mean_matrix[1,]))
  for (i in 2:nrow(mean_matrix)){
    V <- V + (weights[i] * (mean_matrix[i,] %*% t(mean_matrix[i,])))
  }

  return(V)
}
