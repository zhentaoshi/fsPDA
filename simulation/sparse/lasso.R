lasso = function (t, t1, y, X){
  
  y1 = y[1:t1]
  y2 = y[(t1+1):t]
  
  X1 = X[1:t1,]
  X2 = X[(t1+1):t,]
  
  N = dim(X1)[2]
  
  # fid = rep(1:5, each = t1/5) # 5 fold cv
  # 
  # lam = seq(0, 1000, length = 1000) *sqrt(log(N)/t1)
  
  cvlas = cv.glmnet(x = X1, y = y1, standardize = T, intercept = F, family="gaussian",
                    lambda = seq(1e-04, 0.2, length = 1000))
                    
  # plot(cvlas)
  lambda.hat = cvlas$lambda.min
  
  las = glmnet(x = X1, y = y1, standardize = T, intercept = F)
  b = predict (las, type = "coef", s = lambda.hat)
  b = b[-1]
  
  y2.hat = drop (X2 %*% b)
  d = y2 - y2.hat
  
  # b = as.numeric(b != 0)
  
  return( list(b = b , d = d) )
}