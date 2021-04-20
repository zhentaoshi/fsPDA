prd = function (d, lvre, shift){
  
  t2 = length(d)
  
  dels = function (shift){
    
    delta = d + shift
    
    if (lvre == "nw"){
      
      delbar = mean(delta)
      v1 = lrvar(delta, type = "Newey-West", prewhite = F, adjust = T, lag = round(t2^(1/3)))
      delsd = sqrt(v1)
      
      # ols = lm(pre.y ~ PRE.X - 1)
      # vc = NeweyWest(ols, lag = round(t1^(1/2)), prewhite = F, adjust = T)
      # x_bar = colMeans(X)
      # v2 = drop (x_bar %*% vc %*% x_bar)
      # 
      # v = v1 + v2
      # delsd = sqrt(v)
      
    } else if (lvre == "ar_css") {
      
      am = arima(delta, order = c(1, 0, 0), method = "ML")
      delbar = am$coef[2]
      delsd = sqrt (diag(am$var.coef)[2])
      
    } else if (lvre == "ar_ols"){
      
      w = delta[2:length(delta)]
      w_1 = delta[1:(length(delta)-1)]
      om = summary( lm(w ~ w_1) )$coefficients
      delbar = om[1,1]
      delsd = om[1,2]
      
    } else if (lvre == "ols"){
    
      delbar = mean(delta)
      v1 = var(delta)/t2
      delsd = sqrt(v1)
      
      # e.hat = pre.y - drop( PRE.X %*% b.hat )
      # vc = var(e.hat) * solve (t(PRE.X) %*% PRE.X)
      # x_bar = colMeans(X)
      # v2 = drop( t(x_bar) %*% vc %*% x_bar )
      # 
      # v = v1 + v2
      # delsd = sqrt(v)
      
    }
    
    stu = delbar/delsd
    
    output = c(delbar, delsd, stu)
    return (output)
  }
  
  DELTA = sapply(shift, FUN = dels)
  DELTA = as.vector(DELTA)
  
  return(DELTA)
}