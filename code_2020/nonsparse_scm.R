rm(list=ls())
setwd("~/boosting_hcw/code_2020")

library(CVXR)
library(sandwich)
library(MASS)
library(xlsx)

source("scm.R")
source("prd.R")

set.seed(5)

n = 101 # choose from n-1 variables

G = matrix(NA, 4, n)
G[,1:5] = runif(4*5, 1, 2)
G[,6:n] = runif(4*(n-5), -0.5, 0.5)

dgp_iid = function (t, n){
  f = matrix(NA, t, 4)
  for (r in 1:4){
    f[,r] = rnorm(t, mean = 0, sd = r)
  }
  e = rnorm(t*n, 0, 0.5)
  Y = f %*% G + e
  
  return(Y)
}

dgp_nnd = function (t, n){
  f = matrix(NA, t, 4)
  f[,1] = arima.sim( t, model = list(ar = 0.9), n.start = 1000)
  f[,2] = arima.sim( t, model = list(ar = 0.5, ma = 0.5), n.start = 1000)
  f[,3] = arima.sim( t, model = list(ma = c(0.8, 0.4)), n.start = 1000)
  f[,4] = rnorm(t)
  e = rnorm(t*n, 0, 0.5)
  Y = f %*% G + e
  
  return(Y)
}
  
times = 1000 # iteration times
  
mainfunc = function(t, t1, dgp){
    
  t2 = t - t1
  
  if (dgp == "iid"){
    data = dgp_iid(t, n)
  }else{
    data = dgp_nnd(t, n)
  }
  
  y = data[,1]
  Y = data[,2:n]
  
# SCM
  res = scm(t = t, t1 = t1, y = y, Y = Y)
  b = as.vector(res$b)
  d = res$d
  bias = mean(d)
  rmse = sqrt(mean(d^2))
    
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
  prd_res = prd(d = d, lvre = "nw", shift = shift)
    
  return( c(prd_res, bias, rmse, b) )
}

for (dgp in c("iid", "nnd")){
  A1 = replicate(times, mainfunc(t = 80, t1 = 40, dgp = dgp))
  A2 = replicate(times, mainfunc(t = 160, t1 = 80, dgp = dgp))
  A3 = replicate(times, mainfunc(t = 200, t1 = 100, dgp = dgp))
  A4 = replicate(times, mainfunc(t = 400, t1 = 200, dgp = dgp))
  
  save (times, A1, A2, A3, A4, file = paste ("nonsparse_scm_", dgp, ".Rda", sep = ""))
}


for (dgp in c("iid", "nnd")){

  load (paste ("nonsparse_scm_", dgp, ".Rda", sep = ""))
  
  sh = 7 # number of delta dgp
  hf = 3*sh
  
  fn = paste("scm_", dgp, ".xlsx", sep = "")
  output_scm = matrix(NA, 4, 7)
  for (j in 1:7){
    output_scm[1,j] = mean(abs(A1[3*j,]) > qnorm(0.975))
    output_scm[2,j] = mean(abs(A2[3*j,]) > qnorm(0.975))
    output_scm[3,j] = mean(abs(A3[3*j,]) > qnorm(0.975))
    output_scm[4,j] = mean(abs(A4[3*j,]) > qnorm(0.975))
  }

  colnames(output_scm) = paste( "DGP-delta", 1:7, sep = "")
  rownames(output_scm) = c("t1 = t2 = 40","t1 = t2 = 80",
                           "t1 = t2 = 100", "t1 = t2 = 200")
  
  MS = matrix(NA, 4, 5)
  rownames(MS) = c("t1 = t2 = 40","t1 = t2 = 80",
                   "t1 = t2 = 100", "t1 = t2 = 200")
  colnames(MS) = c("p_scm_1e-5", "p_scm_1e-6", "p_scm_1e-7", "bias_scm", "rmse_scm")
  
  #sparsity -- k
  get_sparsity = function(A, threshold)
  {
    all_b = A[(hf + 3):nrow(A),]
    k = rep(0, times)
    for (i in 1:times)
    {
      b = all_b[,i]
      k[i] = sum(as.numeric(abs(b) >= threshold))
    }
    return (k)
  }
  
  MS[1,1] = median(get_sparsity(A1, 1e-5))
  MS[2,1] = median(get_sparsity(A2, 1e-5))
  MS[3,1] = median(get_sparsity(A3, 1e-5))
  MS[4,1] = median(get_sparsity(A4, 1e-5))
  
  MS[1,2] = median(get_sparsity(A1, 1e-6))
  MS[2,2] = median(get_sparsity(A2, 1e-6))
  MS[3,2] = median(get_sparsity(A3, 1e-6))
  MS[4,2] = median(get_sparsity(A4, 1e-6))
  
  MS[1,3] = median(get_sparsity(A1, 1e-7))
  MS[2,3] = median(get_sparsity(A2, 1e-7))
  MS[3,3] = median(get_sparsity(A3, 1e-7))
  MS[4,3] = median(get_sparsity(A4, 1e-7))
  
  
  # Bias and RMSE
  MS[1,4:5] = rowMeans(A1[(hf + 1):(hf + 2),])
  MS[2,4:5] = rowMeans(A2[(hf + 1):(hf + 2),])
  MS[3,4:5] = rowMeans(A3[(hf + 1):(hf + 2),])
  MS[4,4:5] = rowMeans(A4[(hf + 1):(hf + 2),])
  
  write.xlsx(output_scm, file = fn, row.names = T, col.names = T,
             sheetName = "szpw", append = T)
             
  write.xlsx(MS, file = fn,row.names = T, col.names = T,
             sheetName = "prd&k", append = T)
}

