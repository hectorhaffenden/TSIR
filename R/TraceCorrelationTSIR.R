TraceCorrelationTSIR = function(v1,v2, d = 1){
  if (dim(as.matrix(v1))[2] == 1)
  {p1 = v1%*%t(v1)/c(t(v1)%*%v1)
  p2 = v2%*%t(v2)/c(t(v2)%*%v2)
  di = sum(diag(p1%*%p2))/d
  return(di)} else
    p1 <- v1%*%matpower(t(v1)%*%v1,-1)%*%t(v1)
  p2 <- v2%*%matpower(t(v2)%*%v2,-1)%*%t(v2)
  di = sum(diag(p1%*%p2))/d
  return(di)
}
