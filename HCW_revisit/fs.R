fs_a = function(N){
  ybar = mean(pre.y)
  SST = sum((pre.y - ybar) * (pre.y - ybar))
  R2 = matrix(0, 1, N)
  for (j in 1:N){
    X = cbind(1, PRE.Y[,j])
    b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
    e = pre.y - drop(X%*%b)
    R2[,j] = 1 - sum(e*e)/SST
  }
  select = which.max(R2)
  
  X = cbind(1, PRE.Y[,select])
  b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
  e = pre.y - drop(X%*%b)
  SSR = sum(e*e)
  rsq = 1 - SSR/SST
  B = log(log(N))*log(t1)/t1
  Q.new = log(SSR/t1) + B
  Q.old = Q.new + 1
  
  # iteration after the first selction
  
  while (Q.new < Q.old ) {
    left = setdiff(1:N, select)
    PRE.Y.LEFT = PRE.Y[,left]
    
    Q.old = Q.new
    
    R2 = matrix(0, 1, length(left))
    
    for (j in 1:length(left)){
      
      X = cbind(1, PRE.Y [,select], PRE.Y.LEFT[,j])
      b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
      e = pre.y - drop(X%*%b)
      R2[,j] = 1 - sum(e*e)/SST

    }
    
    index = left[which.max(R2)]
    select = append(select, index)
    k = length(select)
    
    X = cbind(1, PRE.Y[,select])
    b = solve(t(X) %*% X) %*% t(X) %*% pre.y
    y.hat = as.vector (X %*% b)
    e = pre.y - y.hat
    SSR = sum(e*e)
    rsq = append(rsq, 1 - SSR/SST)
    B = log(log(N))*k*log(t1)/t1 
    Q.new = log(SSR/t1) + B
  }  
  
  select = select[-length(select)]
  rsq = rsq[-length(select)]
  
  X = cbind(1,PRE.Y[,select]) 
  b = solve(t(X) %*% X) %*% t(X) %*% pre.y
  
  return (list(b = b, rsq = rsq) )
}  

fs_p = function(N, pmax){
  ybar = mean(pre.y)
  SST = sum((pre.y - ybar) * (pre.y - ybar))
  
  R2 = matrix(0, 1, N)
  for (j in 1:N){
    X = cbind(1, PRE.Y[,j])
    b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
    e = pre.y - drop(X%*%b)
    R2[,j] = 1 - sum(e*e)/SST
  }
  select = which.max(R2)
  
  X = cbind(1, PRE.Y[,select])
  b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
  e = pre.y - drop(X%*%b)
  SSR = sum(e*e)
  rsq = 1 - SSR/SST
  k = 1
  
  # iteration after the first selction
  
  while (k<9) {
    left = setdiff(1:N, select)
    PRE.Y.LEFT = PRE.Y[,left]
    
    R2 = matrix(0, 1, length(left))
    
    for (j in 1:length(left)){
      
      X = cbind(1, PRE.Y [,select], PRE.Y.LEFT[,j])
      b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
      e = pre.y - drop(X%*%b)
      R2[,j] = 1 - sum(e*e)/SST
      
    }
    
    index = left[which.max(R2)]
    select = append(select, index)
    k = length(select)
    
    X = cbind(1, PRE.Y[,select])
    b = solve(t(X) %*% X) %*% t(X) %*% pre.y
    y.hat = as.vector (X %*% b)
    e = pre.y - y.hat
    SSR = sum(e*e)
    rsq = append(rsq, 1 - SSR/SST)
  }  
  
  X = cbind(1,PRE.Y[,select]) 
  b = solve(t(X) %*% X) %*% t(X) %*% pre.y
  
  return (list(b = b, rsq = rsq) )
}  
