library(foreach)
rd = function(N, pmax){
  ybar = mean(pre.y)
  SST = sum((pre.y - ybar) * (pre.y - ybar))
  
  fp = function(p){
    allc = combn(1:N, p)
    
    fl = function(index){
      X = cbind(1, PRE.Y[,index])
      b = solve(t(X) %*% X) %*% t(X) %*% pre.y
      y.hat = as.vector (X %*% b)
      e = pre.y - y.hat
      SSR = sum(e*e)
      r2 = 1 - SSR/SST
      
      return(r2)
    }
    
    R2 = apply(allc, 2, fl)
    rsq_mean = mean(R2)
    best = which.max(R2)
    rsq_max = R2[best]
    sl = allc[,best]
    print(country[sl])
    X = cbind(1, PRE.Y[,sl])
    b = solve(t(X) %*% X) %*% t(X) %*% pre.y
    
    return(list(R2 = R2, rsq_mean = rsq_mean, rsq_max = rsq_max, b = b))
  }
  
  Q = foreach(i = 1:pmax) %dopar% {
    print(i)
    fp(p = i)
  }
  
  wic = function(r){
    rst = Q[[r]]
    SSR = SST*(1 - rst$rsq_max)
    k = length(rst$b) - 1
    B = log(log(N))*k*log(t1)/t1 
    l = log(SSR/t1) + B
    return(l)
  }
  
  l_wic = sapply(1:pmax, wic)
  p_wic = which.min(l_wic)
  return(list(Q = Q, p_wic = p_wic))
}