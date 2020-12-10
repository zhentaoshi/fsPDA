prd = function (d, lvre, shift){
  
  t2 = length(d)
  
  dels = function (shift){
    delta = d + shift
    delbar = mean(delta)
    v1 = lrvar(delta, type = "Newey-West", prewhite = T, adjust = T, lag = round(t2^(1/2))) # use NW?
    delsd = sqrt(v1)
    stu = delbar/delsd
    
    output = c(delbar, delsd, stu)
    return (output)
  }
  
  DELTA = sapply(shift, FUN = dels)
  DELTA = as.vector(DELTA) # length of 21
  
  return(DELTA)
}