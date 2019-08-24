CenterTSIR <- function(x){
  xm = apply(x,2,mean)
  xc = t(t(x) - xm)
  return(xc)
}
