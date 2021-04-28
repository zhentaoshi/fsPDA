#' fsPDA: Package for the Forward-Selected Panel Data Approach
#'
#' Estimation and inference by the forward-selected panel data approach (Shi and Huang, 2021).
#'
#' \code{est.fsPDA} is the main function. It generates a \code{fsPDA} object which can work with
#' the generic method \code{plot}.
#'
#' @references
#' Zhentao Shi and Jingyi Huang (2021): "Forward-Selected Panel Data Approach for Program Evaluation,"
#'   forthcoming at the Journal of Econometrics, arXiv: 1908.05894
#'
#' @docType package
#' @name fsPDA
NULL





#' Chinese Import Data
#'
#' This dataset is described in Section 5 of Shi and Huang (2021).
#'
#' @usage data(china_import)
#'
#' @format
#' \itemize{
#'  \item\strong{Frequency:} {Monthly}
#'  \item\strong{Date Range:} {2010:Feb -- 2015:Dec}
#'  \item\code{treated} {Time series of luxury watch import, under the official category
#'  name "watches with case of, or clad with, precious metal". Names in the format "yyyymm".}
#'  \item\code{control} {A matrix of commodities as the control units. The rowname is "yyyymm", and the column name
#'  is the identity of the commodity.}
#'  \item\code{intervention_time} {Character. The month when the treatment intervention started, formated in "yyyymm".}
#' }
#'
#' @source United Nations Comtrade Database (\url{https://comtrade.un.org/}).
#' This database provides detailed statistics for international commodity trade.
#'
#' Pre-treatment: 2010:Feb -- 2012:Dec (35 months). Post-treatment: 2013:Jan -- 2015:Dec (36 observations).
#'
#' China's monthly import data are unavailable after September of 2012.
#' To make our data consistent,
#' we sum the export value of other countries to China and take it as import value before and after September of 2012.
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
#' This is the real GDP growth rate data used in Hsiao, Ching, and Wan (2012).
#'
#' @usage data(HCW)
#'
#' @format
#' \itemize{
#'   \item\code{panel} {A matrix of real GDP growth rate of many countries.}
#'   \item\code{quarter} {Character. A time sequence related to \code{panel} labeled by year and quarter.}
#'   \item\code{T1} {An Integer indicates the length of pre-treated period.}
#'}
#'
#' @references
#' Hsiao, C., S. H. Ching, and S. K. Wan (2012):
#'   “A panel data approach for program evaluation: measuring the benefits of political and economic integration of Hong Kong with mainland China,”
#'   Journal of Applied Econometrics, 27(5), 705–740.
#'
#' @docType data
"HCW"
