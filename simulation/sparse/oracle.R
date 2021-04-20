oracle = function (t, t1, s0, y, X){
  
  y1 = y[1:t1]
  y2 = y[(t1+1):t]
  
  X1 = X[1:t1,]
  X2 = X[(t1+1):t,]
  
  N = dim(X1)[2]
  
  X10 = X1[,1:s0]
  b = solve (t(X10) %*% X10) %*% t(X10) %*% y1
  b = append (b, rep(0, N-s0))
  
  y2.hat = drop (X2 %*% b)
  d = y2 - y2.hat
  
  return( list(b = b , d = d) )
}