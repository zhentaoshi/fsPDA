library(reshape2)
library(xlsx)

setwd("~/simulation/sparse")

t_list = c(100, 200, 400)
n_list = c(50, 100, 200)
s_list = c(4, 8, 16)
s = rep(paste("s = ", s_list, ""), 6)

# TABLE B1 ~ B3, the selection performance
for (dgp in c("iid","inid","nnd")){
  load(paste(dgp,".Rda", sep = ""))
  
  sl = rep (c("fs_p_a","fs_p_b", "fs_p_c",
              "las_p_a", "las_p_b", "as_p_c"), each = 3)
  sl = cbind(sl, s)
  sl = rbind(c("",""), sl)
  sl = data.frame(sl)
  
  for (tau in t_list){
    A = WIW[WIW$tt == tau,]
    
    this_tau = dcast(A, ss ~ nn, value.var = "slc_bst_2")
    for (cl in c("slc_bst_3", "slc_bst_4","slc_las_2", "slc_las_3", "slc_las_4"))
    {
      this_tau = rbind(this_tau, dcast(A, ss ~ nn, value.var = cl))
    }
    this_tau = this_tau*100
    this_tau = round(this_tau, digits = 1)
    this_tau = this_tau[,2:ncol(this_tau)]
    this_tau = rbind(rep(paste("T = ", tau/2, ""), ncol(this_tau)), this_tau)
    
    sl = cbind(sl, this_tau)
  }
  
  write.xlsx(sl, file = "Table_B1-B3_sl.xlsx",sheetName = dgp, row.names = T, col.names = T, append = T)
}


# TABLE B4 ~ B6,  the bias and rmse of beta_hat  
for (dgp in c("iid","inid","nnd")){
  load(paste(dgp,".Rda", sep = ""))
  
  beta = rep (c("b_bias_bst","b_bias_las" ,"b_bias_orc",
                "b_rmse_bst", "b_rmse_las", "b_rmse_orc"), each = 3)
  beta = cbind(beta, s)
  beta = rbind(c("",""), beta)
  beta = data.frame(beta)
  
  for (tau in t_list){

    A = WIW[WIW$tt == tau,]

    # bias takes 3 digits
     beta_bias = dcast(A, ss ~ nn, value.var =  "b_bias_bst")
     for (cl in c("b_bias_las" ,"b_bias_orc")){
       beta_bias = rbind(beta_bias, dcast(A, ss ~ nn, value.var = cl))
     }
     beta_bias = beta_bias * 1000
     beta_bias = round(beta_bias, digits = 3 )
     
     # rmse takes 1 digit
     beta_rmse = dcast(A, ss ~ nn, value.var =  "b_rmse_bst")
     for (cl in c("b_rmse_las" ,"b_rmse_orc")){
       beta_rmse = rbind(beta_rmse, dcast(A, ss ~ nn, value.var = cl))
     }
     beta_rmse = beta_rmse * 1000
     beta_rmse = round(beta_rmse, digits = 1 )
     
     this_tau = rbind(beta_bias, beta_rmse)
     this_tau = this_tau[,2:ncol(this_tau)]
     this_tau = rbind(rep(paste("T = ", tau/2, ""), ncol(this_tau)), this_tau)
     
     beta = cbind(beta, this_tau)
  } 
     
     write.xlsx(beta, file = "Table_B4-B6_beta.xlsx",sheetName = dgp, row.names = T, col.names = T, append = T)
}

# TABLE B7 ~ B9, out-of_sample prediction
for (dgp in c("iid","inid","nnd")){
  load(paste(dgp,".Rda", sep = ""))
  
  prd = rep(c("bias_bst", "bias_las",
              "bias_orc", "rmse_bst", "rmse_las", "rmse_orc"), each = 3)
  prd = cbind(prd, s)
  prd = rbind(c("",""), prd)
  prd = data.frame(prd)
  
  for (tau in t_list){
    A = WIW[WIW$tt == tau,]
    A[,c("bias_bst","bias_las", "bias_orc")] = 1000 * A[,c("bias_bst","bias_las", "bias_orc")]

    this_tau = dcast(A, ss ~ nn, value.var = "bias_bst")
     for (cl in c ( "bias_las", "bias_orc", "rmse_bst", "rmse_las", "rmse_orc")){
       this_tau = rbind(this_tau, dcast(A, ss ~ nn, value.var = cl))
     }
    this_tau = round(this_tau, digits = 2)
    
    this_tau = this_tau[,2:ncol(this_tau)]
    this_tau = rbind(rep(paste("T = ", tau/2, ""), ncol(this_tau)), this_tau)
    
    prd = cbind(prd, this_tau)
  }
  
  write.xlsx(prd, file = "Table_B7-B9_prd.xlsx",sheetName = dgp,
             row.names = T, col.names = T, append = T)
}

# TABLE B10, test size and power
for (n in n_list){
  for (s0 in s_list){
    col_names = c("dgp", "model", "T", paste("D", 1:7, sep = ""))
    szpw = as.matrix(t(1:length(col_names)))
    
    for (dgp in c("iid","inid","nnd")){
      load(paste(dgp,".Rda", sep = ""))
      
      A = WIW[WIW$nn == n,]
      A = A[A$ss == s0,]
      A = round(A, 3)
      
      fs = A[,c("tt","t_bst_1", "t_bst_2", "t_bst_3", "t_bst_4","t_bst_5", "t_bst_6", "t_bst_7")]
      fs = cbind("fs", fs)
      fs = as.matrix(fs)
      
      las = A[,c("tt", "t_las_1", "t_las_2", "t_las_3", "t_las_4", "t_las_5", "t_las_6", "t_las_7")]
      las = cbind("las", las)
      las = as.matrix(las)
      
      orc = A[,c("tt", "t_orc_1", "t_orc_2","t_orc_3", "t_orc_4", "t_orc_5", "t_orc_6", "t_orc_7")]
      orc = cbind("orc", orc)
      orc = as.matrix(orc)
      
      this_dgp = rbind(fs, las, orc)
      this_dgp = cbind(dgp, this_dgp)
      colnames(this_dgp) = col_names
      
      szpw = rbind(szpw, this_dgp)
    }
    szpw = szpw[2:nrow(szpw),]
    szpw[, 4:ncol(szpw)] = as.numeric(szpw[, 4:ncol(szpw)])
    szpw = data.frame(szpw)
    
    sn = paste("N=", n, " ", "s=", s0, sep = "")
    write.xlsx(szpw, file = "Table_B10_szpw.xlsx",sheetName = sn,
               row.names = F, col.names = T, append = T)
  }
}
