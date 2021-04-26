#' Forward-selected panel data approach
#'
#' Estimation and inference by the forward-selected panel data approach in Shi and Huang (2021)
#'
#' @param treated Numeric. A T-by-1 vector of time series of the treated units.
#' @param control Numeric. A T-by-N matrix. Each column is the times series of a control unit.
#' @param treatment_start An integer specifying the period treatment starts.
#'   We suggest setting it between $T/2$ and $T$ to ensure enough pre-treatment observations.
#' @param date Date or numeric. A $T$-dimensional vector of date class or any meaningful numerical sequence.
#'   The default option `NULL` means `1:length(treated)` is used.
#' @param lrvar_lag A non-negative integer specifying the maximum lag with Bartlett kernel
#'   for the Newey-West long-run variance estimator.
#'   The default option `NULL` means `floor((length(treated)-treatment_start+1)^(1/4))` is used.
#'   It cannot be larger than `floor(sqrt(length(treated)-treatment_start+1))`.
#'
#' @return A list is returned of the class `fsPDA`. It contains the following components:
#' \item{select}{A list containing the selected control units,
#'   where `dim` is the number of selected units,
#'   `control` is the vector indicates the selected units,
#'   `coef` contains the coefficient estimates,
#'   and `RSquared` is the in-sample $R^2$.}
#' \item{in_sample}{A data frame with in-sample fitted values.}
#' \item{out_of_sample}{A data frame with out-of-sample counterfactual predicts and treatment effect estimates.}
#' \item{ATE}{A numeric vector containing estimate of average treatment effect (ATE),
#'   its long-run variance, t-statistic, and p-value to test if ATE is statistically 0.}
#' \item{plot}{A `ggplot` object. Some post-plot arguments of `ggplot` can be added additionally, for example, `labs`.}
#'
#' @examples
#' library(fsPDA)
#' data("china_import")
#'
#' treated <- testData$treated
#' control <- testData$control
#' intervention_time <- testData$intervention_time
#'
#' result <- est.fsPDA(treated, control,
#'   treatment_start = which(names(treated) == intervention_time),
#'   date = as.Date(paste(substr(names(treated), 1, 4), "-", substr(names(treated), 5, 6), "-01", sep = ""))
#' )
#'
#' print(result)
#' @export
#'
#'
#' @references
#' Zhentao Shi and Jingyi Huang (2021): "Forward-Selected Panel Data Approach for Program Evaluation,"
#'   forthcoming at the Journal of Econometrics, arXiv: 1908.05894
#'


est.fsPDA <- function(treated, control, treatment_start, date = NULL, lrvar_lag = NULL) {


  # check inputs

  if (is.matrix(control)) {
    N <- ncol(control)
  } else {
    stop("control must be a matrix")
  }

  if (length(treated) == nrow(control)) {
    Tn <- length(treated)
  } else {
    stop("lengths of treated and control units must be the same")
  }

  if (treatment_start == floor(treatment_start) & treatment_start > 0.5 * Tn & treatment_start <= Tn) {
    T1 <- treatment_start - 1
    T2 <- Tn - T1
  } else {
    stop("treatment_start must be an integer no larger than the total sample size with enough pre-treatment periods")
  }

  if (is.null(date)) {
    date <- 1:Tn
  } else {
    if (length(date) != Tn) {
      stop("date must be of the same length with the total sample size")
    }
  }

  if (is.null(lrvar_lag)) {
    lrvar_lag <- floor(T2^(1 / 4))
  } else {
    if (lrvar_lag != floor(lrvar_lag) | lrvar_lag < 0 | lrvar_lag > sqrt(T2)) {
      stop("lrvar_lag must be a non-negative integer no larger than sqrt of post-treated sample size")
    }
  }


  # sample splitting

  y1 <- treated[1:T1]
  y2 <- treated[(T1 + 1):Tn]
  Y1 <- control[1:T1, , drop = FALSE]
  Y2 <- control[(T1 + 1):Tn, , drop = FALSE]


  # forward iteration

  var.resid <- function(j, Y_select) {
    X <- cbind(1, Y_select, Y1[, j])
    XX <- t(X) %*% X
    XX_inv <- try(solve(XX), silent = TRUE)
    if ("try-error" %in% class(XX_inv)) {
      XX_inv <- MASS::ginv(XX)
    }
    b_hat <- XX_inv %*% t(X) %*% y1
    e_hat <- y1 - as.vector(X %*% b_hat)
    sigma2_hat <- mean(e_hat^2)
    return(sigma2_hat)
  }

  B <- log(log(N)) * log(T1) / T1
  IC <- log(var(y1))
  select <- NULL
  R <- 0
  left <- 1:N
  for (r in 1:T1) {
    sigma2_grid <- sapply(left, FUN = var.resid, Y_select = Y1[, select])
    sigma2_min <- min(sigma2_grid)
    select_1 <- left[which(sigma2_grid == sigma2_min)]
    IC_1 <- log(sigma2_min) + B * (R + length(select_1))
    if (IC_1 < IC) {
      IC <- IC_1
      select <- c(select, select_1)
      R <- length(select)
      left <- setdiff(left, select_1)
    } else {
      break
    }
  }


  # post-selection estimation

  X <- cbind(1, Y1[, select, drop = FALSE])
  XX <- t(X) %*% X
  XX_inv <- try(solve(XX), silent = TRUE)
  if ("try-error" %in% class(XX_inv)) {
    XX_inv <- MASS::ginv(XX)
  }
  b_hat <- as.vector(XX_inv %*% t(X) %*% y1)

  beta_hat <- rep(0, N)
  beta_hat[select] <- b_hat[-1]
  beta_hat <- c(b_hat[1], beta_hat)
  y1_hat <- as.vector(X %*% b_hat)
  RSquared <- var(y1_hat) / var(y1)


  # counterfactual and treatment effect

  y2_0 <- as.vector(cbind(1, Y2[, select, drop = FALSE]) %*% b_hat)
  d_hat <- y2 - y2_0
  ATE <- mean(d_hat)


  # test of ATE with Newey-West lrvar (Bartlett kernel)

  if (lrvar_lag > 0) {
    gamma_d <- as.vector(acf(d_hat, lag.max = lrvar_lag, type = "covariance", plot = FALSE)$acf)
    w <- 1 - (1:lrvar_lag) / (lrvar_lag + 1)
    lrvar_d <- gamma_d[1] + 2 * sum(w * gamma_d[-1])
  } else {
    lrvar_d <- var(d_hat)
  }

  Z <- ATE / sqrt(lrvar_d / T2)
  p_value <- 2 * (1 - pnorm(abs(Z)))


  # output

  if (!is.null(colnames(control))) {
    names(beta_hat) <- c("intercept", colnames(control))
    select <- colnames(control)[select]
  }

  result <- list(
    select = list(dim = R, control = select, coef = beta_hat, RSquared = RSquared),
    in_sample = data.frame(date = date[1:T1], observation = y1, fit = y1_hat),
    out_of_sample = data.frame(date = date[(T1 + 1):Tn], observation = y2, counterfactual = y2_0, treatment = d_hat),
    ATE = c(ATE = ATE, lrVar = lrvar_d, t_stat = Z, p_value = p_value)
  )
  class(result) <- "fsPDA"


  return(result)
}