StandardTSIR <- function(X){
  # returns X standardised.
  X_var = var(X)
  X_c = CenterTSIR(X)
  X_stand = X_c %*% matpower(X_var,-0.5) # From sSDR package
  return(X_stand)
}
