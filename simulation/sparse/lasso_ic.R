lasso_ic = function (t, t1, y, X, ic, cst){
  
  y1 = y[1:t1]
  y2 = y[(t1+1):t]
  
  X1 = X[1:t1,]
  X2 = X[(t1+1):t,]
  
  N = dim(X1)[2]
  
  lam = seq(0.01, 0.5, length = 100)
  
  las = glmnet(x = X1, y = y1, standardize = T, intercept = F,
                    family="gaussian",lambda = lam)
  
  B = as.matrix(las$beta)
  yhat = predict(las, newx = X1, type = "link")
  sigma.sq = apply(yhat, MARGIN = 2, function(x)(var(y1-x)))
  k = las$df
  
  if (ic == "WIC"){
    Q = log(sigma.sq) + cst*(log(log(N)))*(log(t1)/t1)*k
  } else if (ic == "BIC"){
    Q = sigma.sq + cst*log(t1)/t1*k
  } else {
    Q = sigma.sq + 2*k/t1 #AIC
  }
  
  b = B[,which.min(Q)]
  
  mse = mean( (y1 - X1 %*% b)^2 )
  y2.hat = drop (X2 %*% b)
  d = y2 - y2.hat
  
  # b = as.numeric(b != 0)
  
  return( list(b = b , d = d, mse = mse) )
}