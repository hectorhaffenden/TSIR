DiscretizeTSIR <- function(y, n_slices){
  # Returns the Discretized y values.

  y_ordered <- y[order(y)]
  n <- length(y)
  nwith <- round(n / n_slices)
  # Identifying an equal range given the ordered y data.
  index_to_find <- 1:(n_slices-1) * nwith + 1
  # Finding the dividing lines for the y values.
  divide_lines <- y_ordered[index_to_find]


  y_disc <- rep(0, n)

  # Finding the instances above the max number.
  # Asigning the largest slice.
  y_disc[y >= divide_lines[n_slices-1]] <- n_slices

  # Change to less than or equal to.
  y_disc[y <= divide_lines[1]] <- 1


  for(i in 2:(n_slices-1)){
    # Use temp index incase we have alot of very low, or high y values.
    # We have to perform an additional check to see if the position in...
    # ... y_disc already has a value.
    # I am considering changing this...
    temp_index <- (y >= divide_lines[i-1]) & (y < divide_lines[i])

    # The extra y_disc == 0 check ensures we don't overwrite any data.
    y_disc[temp_index & y_disc==0] <- i
  }

  return(y_disc)
}




