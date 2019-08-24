leadTSIR <- function(x, n = 1L, default = NA, order_by = NULL, ...){
  if (!is.null(order_by)) {
    return(with_order(order_by, lead, x, n = n, default = default))
  }
  if (length(n) != 1 || !is.numeric(n) || n < 0) {
    bad_args("n", "must be a nonnegative integer scalar, ",
             "not {friendly_type_of(n)} of length {length(n)}")
  }
  if (n == 0)
    return(x)
  xlen <- length(x)
  n <- pmin(n, xlen)
  out <- c(x[-seq_len(n)], rep(default, n))
  attributes(out) <- attributes(x)
  out
}
