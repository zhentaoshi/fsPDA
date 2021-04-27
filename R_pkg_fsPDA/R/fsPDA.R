#' fsPDA: Package for the forward-selected panel data approach
#'
#' Estimation and inference by the forward-selected panel data approach in Shi and Huang (2021)
#'
#' \code{est.fsPDA} is the main function. It generates a \code{fsPDA} object which can work with
#' the generic methods \code{plot}.
#'
#' @references
#' Zhentao Shi and Jingyi Huang (2021): "Forward-Selected Panel Data Approach for Program Evaluation,"
#'   forthcoming at the Journal of Econometrics, arXiv: 1908.05894
#'
#' @docType package
#' @name fsPDA
NULL





#' Chinese import data
#'
#' @usage data(china_import)
#'
#' @format
#' \itemize{
#'  \item\strong{Release:} {United Nations}
#'  \item\strong{Frequency:} {Annual}
#'  \item\strong{Date Range:} {20xx -- 20xx}
#' }
#'
#' @section Description:
#' This dataset is described in Section 5 of Shi and Huang (2021).
#'
#'
#' @source United Nations Comtrade Database \url{https://comtrade.un.org/}
#'
#' @references
#'
#' Zhentao Shi and Jingyi Huang (2021): "Forward-Selected Panel Data Approach for Program Evaluation,"
#'   forthcoming at the Journal of Econometrics, arXiv: 1908.05894
#'
#'
#' @docType data
"china_import"





#' HCW Data
#'
#' @usage data(HCW)
#'
#' This is the real GDP growth rate data used in Hsiao, Ching, and Wan (2012).
#'

#'
#' @format
#' \itemize{
#'   \item\strong{panel} {A matrix of real GDP growth rate of many countries.}
#'   \item\strong{quarter} {Character. A time sequence related to `panel` labeled by year and quarter.}
#'   \item\strong{T1} {An Integer indicates the length of pre-treated period.}
#'}
#'
#' @references
#' Hsiao, C., S. H. Ching, and S. K. Wan (2012):
#'   “A panel data approach for program evaluation: measuring the benefits of political and economic integration of Hong Kong with mainland China,”
#'   Journal of Applied Econometrics, 27(5), 705–740.
#'
#' @docType data
"HCW"
