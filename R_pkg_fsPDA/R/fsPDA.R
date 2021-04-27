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
#'  \item\strong{Frequency:} {Monthly}
#'  \item\strong{Date Range:} {2010 -- 2015}
#' }
#'
#' @section Description:
#' This dataset is described in Section 5 of Shi and Huang (2021).
#'
#'
#' @source United Nations Comtrade Database \url{https://comtrade.un.org/}
#' This database provides detailed statistics for international commodity trade, and China's monthly import data is unavailable after September of 2012. 
#' To make our data consistent, we sum the export value of other countries to China and take it as import value before and after September of 2012.
#'
#' The treated variable is “watches with case of, or clad with, precious metal”, the control variables are other 88 catrgories.
#' 7 categories commonly consumed as bribe goods or conspicuous consumption are excluded.
#' There are 35 pre-treatment observations ranging from February 2010 to December 2012, and 36 post-treatment observations spanning from January 2013 to December 2015
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
