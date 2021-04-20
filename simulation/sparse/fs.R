fs = function(t, t1, y, Y){
  
  pre.y = y[1:t1]
  post.y = y[(t1+1):t]
  
  PRE.Y = Y[1:t1,]
  
  N = dim(PRE.Y)[2]
  
  R2 = matrix(0, 1, N)
  for (j in 1:N){
    X = PRE.Y[,j]
    b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
    SSE = (t(X %*% b)) %*% (X %*% b)
    
    R2[,j] = SSE
  }
  select = which.max(R2)
  
  X = PRE.Y[,select]
  b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
  y.hat = as.vector (X %*% b)
  e.hat = pre.y - y.hat
  sigma.sq = var(e.hat)
  B = log(log(N))*log(t1)/t1
  Q.new = log(sigma.sq) + B
  Q.old = Q.new + 1
  
  # iteration after the first selction
  
  while (Q.new < Q.old ) {
    
    Q.old = Q.new
    
    # left = setdiff(1:N, select)
    # PRE.Y.LEFT = PRE.Y[,left]
    
    # R2 = matrix(0, 1, length(left))
    # 
    # for (j in 1:length(left)){
    #   
    #   X = cbind(PRE.Y [,select], PRE.Y.LEFT[,j])
    #   b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
    #   SSE = (t(X %*% b)) %*% (X %*% b)
    #   
    #   R2[,j] = SSE
    # }
    
    R2 = matrix(0, 1, N)
    for (j in 1:N){
      X = cbind(PRE.Y [,select], PRE.Y[,j])
      b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
      SSE = (t(X %*% b)) %*% (X %*% b)
      
      R2[,j] = SSE
    }
    
    index = which.max(R2)
    select = append(select, index)
    k = length(select)
    
    X = PRE.Y[,select]
    b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
    y.hat = drop (X %*% b)
    e.hat = pre.y - y.hat
    sigma.sq = var(e.hat)
    B = log(log(N))*k*log(t1)/t1 
    Q.new = log(sigma.sq) + B
  }  
  
  select = select[1:(length(select)-1)]
  Y.SL = Y[,select]
  
  Y.SL = as.matrix(Y.SL)

  PRE.X = Y.SL[1:t1,]
  POST.X = Y.SL[(t1+1):t,]
  
  b.hat = ginv (t(PRE.X) %*% PRE.X) %*% t(PRE.X) %*% pre.y
  
  post.y.hat = drop(POST.X %*% b.hat)
  
  d = post.y - post.y.hat 
  
  b = rep(0, N)
  b[select] = b.hat
  
  return( list(b = b, d = d) )
}  
   