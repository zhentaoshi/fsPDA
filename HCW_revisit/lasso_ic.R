lasso_ic = function (N){
  ybar = mean(pre.y)
  SST = sum((pre.y - ybar) * (pre.y - ybar))
  
  # offer a lambda sequence
  lam = seq(0.04, 0.004, length = 200)
  
  las = glmnet(x = PRE.Y, y = pre.y, standardize = T, intercept = T, #standarize does not influence much
                    family="gaussian", lambda = lam)
  
  B = as.matrix(las$beta)
  k = las$df
  A = matrix(las$a0, nrow = t1, ncol = length(las$lambda), byrow = T)
  y.hat = apply(B, 2, function(b)(PRE.Y %*% b))
  yhat = A + y.hat
  
  y.hat = predict.glmnet(las, newx = PRE.Y, type = "response")
  e = matrix(pre.y, nrow = nrow(y.hat), ncol = ncol(y.hat), byrow = F) - y.hat
  
  #calculate sigma.square and R2

  SSR = colSums(e*e)
  sigma.sq = SSR/t1
  RSQ = 1 - SSR/SST
  
  # if (ic == "WIC"){
    Q = log(sigma.sq) + 2*(log(log(N)))*(log(t1)/t1)*k
    plot(Q, type = "l")
  # } else if (ic == "BIC"){
  #   Q = sigma.sq + cst*log(t1)/t1*k
  # } else {
  #   Q = sigma.sq + 2*k/t1 #AIC
  # }
  
  index = which.min(Q)
  lambda.hat = las$lambda[index]
  b = B[,index]
  b = b[b != 0]
  a = las$a0[index]
  
  nb = sum(b != 0)
  
  # choose the point before "jumping"
  fpst = function(j){
    all = which(k == j)
    # r2 = RSQ[all]
    # return(all[which.max(r2)])
    
    return(all[which.max(all)])
    
  }
  rsq = RSQ[sapply(2:(nb+1), fpst)]
  
  #post lasso
  b_jump = B[,sapply(1:nb, fpst)]
  nonzero = apply(b_jump, 2, function(x)which(x!=0))
  prsq = function(id){
    X = cbind(1, PRE.Y[,id])
    b = ginv(t(X) %*% X) %*% t(X) %*% pre.y
    e = pre.y - drop(X%*%b)
    SSR = sum(e*e)
    r2 = 1 - SSR/SST
    return(r2)
  }
  rsq_post = sapply(nonzero, prsq)
  matplot(cbind(rsq, rsq_post), type = "l")
  
  return( list(b = append(a,b) , rsq = rsq, rsq_post = rsq_post) )
}
