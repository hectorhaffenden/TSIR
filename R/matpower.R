matpower <- function(X, alpha){
  # Taken directly from the sSDR package. Changes made to deal with small nums.
  X = (X + t(X))/2
  tmp = eigen(X)
  ei_values <- tmp$values # Incase small computation error.
  ei_vectors <- tmp$vectors

  #return(ei_vectors %*% diag(ei_values^alpha) %*% t(ei_vectors))

  return(ei_vectors %*% diag((zapsmall(ei_values)+.Machine$double.eps)^alpha) %*% t(ei_vectors))

}
