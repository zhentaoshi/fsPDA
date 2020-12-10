opt_func_scm <- function(y_0, y_N) {
  
  N <- ncol(y_N)
  
  beta <- Variable(N, 1)
  obj <- Minimize(sum_squares(y_0 - y_N %*% beta))
  constr <- list(
    sum_entries(beta) == 1,
    beta >= 0
  )
  
  problem <- Problem(objective = obj, constraints = constr)
  result <- solve(problem, solver = "ECOS_BB")
  beta_hat <- result$getValue(beta)
  
  return (beta_hat)
}

scm = function(t, t1, y, Y){
  
  pre.y = y[1:t1]
  post.y = y[(t1+1):t]
  PRE.Y = Y[1:t1,]
  POST.Y = Y[(t1+1):t,]
  
  b = opt_func_scm(y_0 = pre.y, y_N = PRE.Y)
  post.y.hat = as.vector(POST.Y %*% b)
  d = post.y - post.y.hat

  return( list(b = b, d = d) )
}  
