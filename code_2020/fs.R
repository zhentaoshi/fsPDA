fs = function(t, t1, y, Y, cst){
  
  pre.y = y[1:t1]
  post.y = y[(t1+1):t]
  PRE.Y = Y[1:t1,]
  
  N = dim(PRE.Y)[2]
  
  # the information criteria
  IC = cst * log(log(N))*(log(t1)/t1)
  
  # the first loop/ intialisation
  R2 = matrix(0, 1, N)
  for (j in 1:N){
    X = PRE.Y[,j]
    b = solve(t(X) %*% X) %*% t(X) %*% pre.y
    SSE = (t(X %*% b)) %*% (X %*% b)
    
    R2[,j] = SSE
  }
  select = which.max(R2)
  
  X = PRE.Y[,select]
  b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
  y.hat = as.vector(X %*% b)
  e.hat = pre.y - y.hat
  sigma.sq = var(e.hat)
  
  k = 1
  Q.new = log(sigma.sq) + IC * k
  Q.old = Q.new + 1
  
  # iteration after the first selction
  
  while (Q.new < Q.old & k < t1) {
    Q.old = Q.new
    
    R2 = matrix(0, 1, N)
    for (j in 1:N){
      
      if (j %in% select)
      {
        SSE = 0
      }
      else
      {
        X = cbind(PRE.Y [,select], PRE.Y[,j])
        b = solve(t(X) %*% X) %*% t(X) %*% pre.y
        SSE = (t(X %*% b)) %*% (X %*% b)
      }
      
      R2[,j] = SSE
    }
    
    index = which.max(R2)
    select = append(select, index)
    k = length(select)
    
    X = PRE.Y[,select]
    b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
    y.hat = as.vector(X %*% b)
    e.hat = pre.y - y.hat
    sigma.sq = var(e.hat)
    Q.new = log(sigma.sq) + k * IC
  }  
  
  select = select[-(length(select))] # delete the last selection
  Y.SL = Y[,select]
  
  Y.SL = as.matrix(Y.SL) # in case only 1 variable is chosen

  PRE.X = Y.SL[1:t1,]
  POST.X = Y.SL[(t1+1):t,]
  
  b.hat = solve(t(PRE.X) %*% PRE.X) %*% t(PRE.X) %*% pre.y
  post.y.hat = as.vector(POST.X %*% b.hat)
  d = post.y - post.y.hat
  
  b = rep(0, N)
  b[select] = b.hat
  
  return( list(b = b, d = d) )
}  
   