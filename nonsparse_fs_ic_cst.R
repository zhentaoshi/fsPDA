rm(list=ls())
setwd("~/boosting_hcw/code_2020")

library(sandwich)
library(MASS)
library(xlsx)

source("fs.R")
source("prd.R")

set.seed(5) # change me

n = 101 # choose from n-1 variables

G = matrix(NA, 4, n)
G[,1:5] = runif(4*5, 1, 2) # signal too strong?
G[,6:n] = runif(4*(n-5), -0.5, 0.5)

dgp_iid = function (t, n){
  f = matrix(NA, t, 4)
  for (r in 1:4){
    f[,r] = rnorm(t, mean = 0, sd = r)
  }
  e = rnorm(t*n, 0, 0.5) # error too small?
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

mainfunc = function(t, t1, dgp, cst){
  
  t2 = t - t1
  
  if (dgp == "iid"){
    data = dgp_iid(t, n)
  }else{
    data = dgp_nnd(t, n)
  }
  
  y = data[,1]
  Y = data[,2:n]

  res = fs(t = t, t1 = t1, y = y, Y = Y, cst = cst)
  k = sum(as.numeric(res$b != 0))
  d = res$d
  bias = mean(d)
  rmse = sqrt(mean(d^2))

  #zero mean shift
  shift1 = rep(0, t2)
  shift2 = rnorm(t2)
  shift3 = as.vector(arima.sim(t2, model = list(ar = 0.5), n.start = 1000))
  #iid shift
  shift4 = rnorm(t2, mean = 0.5, sd = 1)
  shift5 = rnorm(t2, mean = 1, sd = 1)
  #ar shift
  shift6 = 0.5 + as.vector(arima.sim(t2, model = list(ar = 0.5), n.start = 1000))
  shift7 = 1 + as.vector(arima.sim(t2, model = list(ar = 0.5), n.start = 1000))
  
  shift = list(shift1,shift2,shift3,shift4,shift5,shift6,shift7)
  
  prd_res = prd(d = d, lvre = "nw", shift = shift)

  return( c(prd_res, bias, rmse, k) ) #len = 24
}

#cst_list = c(0.25, 0.5, 1, 2, 4, 8, 16)
cst_list = c(1)

for (dgp in c("iid", "nnd"))
{
  for (cst in cst_list)
  {
    A1 = replicate(times, mainfunc(t = 80, t1 = 40, dgp = dgp, cst = cst))
    A2 = replicate(times, mainfunc(t = 160, t1 = 80, dgp = dgp, cst = cst))
    A3 = replicate(times, mainfunc(t = 200, t1 = 100, dgp = dgp, cst = cst))
    A4 = replicate(times, mainfunc(t = 400, t1 = 200, dgp = dgp, cst = cst))
    
    save (A1, A2, A3, A4, file = paste ("fs_ic_", dgp, "_", cst, ".Rda", sep = ""))
  }
}

# comment the rest out if super-computer does not install xlsx pkg
for (dgp in c("iid", "nnd"))
{
  for (cst in cst_list)
  {
    load(paste ("fs_ic_", dgp, "_", cst, ".Rda", sep = ""))
    sh = 7 # number of delta dgp
    hf = 3*sh 
    
    fn = paste("fs_ic_", dgp, ".xlsx", sep = "")
    output = matrix(NA, 4, 7)
    for (j in 1:7){
      output[1,j] = mean(abs(A1[3*j,]) > qnorm(0.975))
      output[2,j] = mean(abs(A2[3*j,]) > qnorm(0.975))
      output[3,j] = mean(abs(A3[3*j,]) > qnorm(0.975))
      output[4,j] = mean(abs(A4[3*j,]) > qnorm(0.975))
    }
    
    colnames(output) = paste( "DGP-delta", 1:7, sep = "")
    rownames(output) = c("t1 = t2 = 40","t1 = t2 = 80","t1 = t2 = 100", "t1 = t2 = 200")
    
    MS = matrix(NA, 4, 3)
    rownames(MS) = c("t1 = t2 = 40","t1 = t2 = 80",
                     "t1 = t2 = 100", "t1 = t2 = 200")
    colnames(MS) = c("p_fs", "bias_fs", "rmse_fs")
    
    # sparsity
    MS[1,1] = median(A1[nrow(A1),])
    MS[2,1] = median(A2[nrow(A2),])
    MS[3,1] = median(A3[nrow(A3),])
    MS[4,1] = median(A4[nrow(A4),])
    
    # Bias and RMSE
    MS[1,2:3] = rowMeans(A1[(hf + 1):(hf + 2),])
    MS[2,2:3] = rowMeans(A2[(hf + 1):(hf + 2),])
    MS[3,2:3] = rowMeans(A3[(hf + 1):(hf + 2),])
    MS[4,2:3] = rowMeans(A4[(hf + 1):(hf + 2),])
    
    write.xlsx(output, file = fn,row.names = T, col.names = T,
               sheetName = paste ("szpw_", cst, sep = ""), append = T)
    
    write.xlsx(MS, file = fn,row.names = T, col.names = T,
               sheetName = paste ("prd&k_", cst, sep = ""), append = T)
    
    print(cst)
  }
}


