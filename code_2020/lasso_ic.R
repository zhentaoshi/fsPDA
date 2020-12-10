lasso_ic = function (t, t1, y, Y, ic, cst){
  
  pre.y = y[1:t1]
  post.y = y[(t1+1):t]
  
  PRE.Y = Y[1:t1,]
  POST.Y = Y[(t1+1):t,]
  
  N = dim(PRE.Y)[2]
  
  lam = seq(1e-02, 1, length = 100)
  
  las = glmnet(x = PRE.Y, y = pre.y, standardize = T, intercept = F,
                    family="gaussian",lambda = lam)
  
  B = as.matrix(las$beta)
  y.hat = predict(las, newx = PRE.Y, type = "link")
  sigma.sq = apply(y.hat, MARGIN = 2, function(x)(var(pre.y-x)))
  
  k = las$df
  if (ic == "WIC"){
    Q = log(sigma.sq) + cst*(log(log(N)))*(log(t1)/t1)*k

  } else if (ic == "BIC"){
    Q = sigma.sq + cst*log(t1)/t1*k
    
  } else {
    Q = sigma.sq + 2*k/t1 #AIC
    
  }
  
  b = B[,which.min(Q)]
  post.y.hat = as.vector(POST.Y %*% b)
  d = post.y - post.y.hat
  
  return( list(b = b , d = d) )
}