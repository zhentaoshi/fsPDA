
set.seed(5)

library(sandwich)
library(MASS)
# library(xlsx)
library(glmnet)

source("fs.R")
source("lasso.R")
source("lasso_ic.R")
source("prd.R")
source("oracle.R")


G0 = matrix(rnorm(4*200, 1, 1), 4, 200)
# dgp by factor model
dgp_nnd = function (t, n){
  
  G = G0[,1:n]
  
  f = matrix(NA, t, 4)
  f[,1] = arima.sim( t, model = list(ar = 0.9), n.start = 1000)
  f[,2] = arima.sim( t, model = list(ar = 0.5, ma = 0.5), n.start = 1000)
  f[,3] = arima.sim( t, model = list(ma = c(0.8, 0.4)), n.start = 1000)
  # f[,4] = arima.sim( t, model = list(ar = 0.5), n.start = 1000)
  f[,4] = rnorm(t)
  
  err = rnorm(t*n, 0, 0.5)
  X = f %*% G + err
  
  return(X)
}

# inid dgp
dgp_inid = function (t, n){
  
  X = matrix(NA, t, n)
  l = floor(n/4)
  for (i in 1:l){
    X[,i] = arima.sim( t, model = (list(ar = 0.9)), n.start = 1000 )
  }
  for (i in (l + 1):(2*l)){
    X[,i] = arima.sim( t, model = (list(ma = c(0.8, 0.4) ) ), n.start = 1000)
  }
  for (i in (2*l + 1):(3*l)) {
    X[,i] = arima.sim( t, model = ( list(ar = 0.5, ma = 0.5)), n.start = 1000 )
  }
  for (i in (3*l + 1):n) {
    X[,i] = arima.sim( t, model = ( list(ar = 0.5)), n.start = 1000 )
  }
  
  sq = seq(1,3*l+1,l) 
  tr = cbind(sq, sq+1, sq+2, sq+3)
  tr = as.vector(tr)
  X = cbind(X[,tr], X[,setdiff(1:n, tr)])
  
  return(X)
}

# iid dgp
dgp_iid = function (t, n){
  X = matrix( rnorm(t*n), t, n)
  return(X)
}

mainfunc = function(t, t1, s0, n, dgp){
  
  t2 = t - t1
  b0 = rep(0, n)
  b0[1:s0] = 1
  
  if (dgp == "iid"){
    Y = dgp_iid(t = t, n = n)
    e = rnorm(t, 0, 0.5)
    y = drop (Y %*% b0 + e)
    
  } else if (dgp == "inid"){
    Y = dgp_inid(t = t, n = n)
    e = rnorm(t, 0, 0.5)
    y = drop (Y %*% b0 + e)
    
  } else if (dgp == "nnd"){
    Y = dgp_nnd(t = t, n = n)
    e = rnorm(t, 0, 0.5)
    y = drop (Y %*% b0 + e)
  }
  
  bst = fs(t = t, t1 = t1, y = y, Y = Y)
  b_bst = bst$b
  d_bst = bst$d
  
  # las = lasso(t = t, t1 = t1, y = y, X = Y)
  las = lasso_ic(t = t, t1 = t1, y = y, X = Y, ic = "WIC", cst = 2)
  b_las = las$b
  d_las = las$d
  
  orc = oracle(t = t, t1 = t1, s0 = s0, y = y, X = Y)
  b_orc = orc$b
  d_orc = orc$d
  
  Y2 = Y[(t1+1):t,]
  y_hat = Y2 %*% cbind(b_bst, b_las, b_orc)
  ym = matrix(y[(t1+1):t], t2, 3, byrow = F)
  yerr = ym - y_hat
  
  bias = colMeans(yerr)
  rss = yerr^2
  mse = sqrt (drop(colMeans(rss)))
  prderr = append(bias, mse)
  
  #zero mean shift
  shift1 = rep(0, t2)
  shift2 = rnorm(t2)
  shift3 = as.vector (arima.sim(t2, model = list(ar = 0.5), n.start = 1000))
  #iid shift
  shift4 = rnorm(t2, mean = 0.5, sd = 1)
  shift5 = rnorm(t2, mean = 1, sd = 1)
  #ar shift
  shift6 = 0.5 + as.vector (arima.sim(t2, model = list(ar = 0.5), n.start = 1000))
  shift7 = 1 + as.vector (arima.sim(t2, model = list(ar = 0.5), n.start = 1000))
  
  shift = list(shift1,shift2,shift3,shift4,shift5,shift6,shift7)
  
  rtr_bst = prd(d = d_bst, lvre = "nw", shift = shift)
  rtr_las = prd(d = d_las, lvre = "nw", shift = shift)
  rtr_orc = prd(d = d_orc, lvre = "nw", shift = shift)
  
  return( c(rtr_bst, rtr_las, rtr_orc, b_bst, b_las, b_orc, prderr) )
  
}

times = 1000 # iteration times

run_simulation = function(dgp){
  
  t_list = c(100, 200, 400)
  n_list = c(50, 100, 200)
  s_list = c(4, 8, 16)
  
  num_res = length(t_list) * length(n_list) * length(s_list)
  
  WIW = matrix (NA, num_res, 44)
  WIW = data.frame(WIW)
  names(WIW)[1:3] = c("tt", "nn", "ss")
  
  # 4 ~ 24 columns are test result, bst, las, orc
  names(WIW)[4:10] = paste("t_bst", 1:7, sep = "_")
  names(WIW)[11:17] = paste("t_las", 1:7, sep = "_")
  names(WIW)[18:24] = paste("t_orc", 1:7, sep = "_")
  
  # 25 ~ 30 are bias and mse of beta estimation
  names(WIW)[25:30] = c("b_bias_bst","b_bias_las", "b_bias_orc", "b_rmse_bst", "b_rmse_las","b_rmse_orc")
  
  # 31 ~ 38 are selection consistency
  names(WIW)[31:34] = paste("slc_bst", 1:4, sep = "_")
  names(WIW)[35:38] = paste("slc_las", 1:4, sep = "_")
  
  # 39 ~ 44 are mse for prediction error
  names(WIW)[39:41] = paste ("bias", c("bst","las","orc"), sep = "_")
  names(WIW)[42:44] = paste ("rmse", c("bst","las","orc"), sep = "_")
  
  WIW$tt = rep(c(100, 200, 400), each = 9)
  WIW$nn = rep( rep (c(50, 100, 200), each = 3), 3)
  WIW$ss = rep(c(4, 8, 16), 9)
  
  WIW$tt = rep(t_list, each = length(n_list) * length(s_list))
  WIW$nn = rep( rep(n_list, each = length(n_list)), length(s_list))
  WIW$ss = rep(s_list, length(n_list) * length(s_list))
  
  for (tau in t_list){
    for (s0 in s_list){
      for (n in n_list){
        
        nm = paste(dgp,tau,n,s0,".Rda", sep = "_")
        
        A = replicate(times, mainfunc(t = tau, t1 = tau/2, s0 = s0, n = n, dgp = dgp))
        save(A, file = nm)
        
        szpw = A[seq(3,63,by = 3),]
        szpw = (abs(szpw)> qnorm(0.975))
        szpw = rowMeans(szpw)
        WIW[(WIW$tt == tau & WIW$nn == n & WIW$ss == s0),4:24] = szpw
        
        beta_bst = A[(63+1):(63+n),]
        beta_las = A[(63+n+1):(63+2*n),]
        beta_orc = A[(63+(2*n)+1):(63+3*n),]
        beta0 = matrix(0, n, times, byrow = F)
        beta0[1:s0,] = 1
        
        bias_bst = mean(beta_bst - beta0)
        bias_las = mean(beta_las - beta0)
        bias_orc = mean(beta_orc - beta0)
        brmse_bst = sqrt(mean( (beta_bst - beta0)^2 ))
        brmse_las = sqrt(mean( (beta_las - beta0)^2 ))
        brmse_orc = sqrt(mean( (beta_orc - beta0)^2 ))
        WIW[(WIW$tt == tau & WIW$nn == n & WIW$ss == s0),25:30] = 
          c(bias_bst, bias_las, bias_orc, brmse_bst,brmse_las, brmse_orc)
        
        slb = (beta_bst != 0)
        slb = (slb == beta0)
        sl1 = mean (colMeans(slb) == 1)
        sl2 = mean (colMeans(slb[1:s0,]) == 1)
        sl3 = mean (slb[1:s0,])
        sl4 = mean (slb[(s0+1):n,])
        WIW[(WIW$tt == tau & WIW$nn == n & WIW$ss == s0),31:34] = c(sl1, sl2, sl3, sl4)
        
        slb = (beta_las != 0)
        slb = (slb == beta0)
        sl1 = mean (colMeans(slb) == 1)
        sl2 = mean (colMeans(slb[1:s0,]) == 1)
        sl3 = mean (slb[1:s0,])
        sl4 = mean (slb[(s0+1):n,])
        WIW[(WIW$tt == tau & WIW$nn == n & WIW$ss == s0),35:38] = c(sl1, sl2, sl3, sl4)
        
        prderr = A[(dim(A)[1] - 5):dim(A)[1],]
        prderr = rowMeans(prderr)
        WIW[(WIW$tt == tau & WIW$nn == n & WIW$ss == s0),39:44] = prderr
        
      }
    }
  }
  
  save (WIW, file = paste(dgp, "Rda", sep = "."))
}

