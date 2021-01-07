#' Dataset: hydrologic metrics calculated from MODFLOW scenarios
#'
#' Hydrologic metrics calculated using all simulation results from monte carlo
#' MODFLOW scenarios
#'
#' @docType data
#'
#' @usage data(MODFLOW_metrics)
#'
#' @format A data frame with the following columns.
#' \describe{
#'   \item{lake}{name of lake, e.g., Pleasant, Long, Plainfield}
#'   \item{metric}{name of hydrologic metrics, e.g. median_level, cv_rise_rate}
#'   \item{variable}{name of variation on metrics, e.g. 10 for 10% exceedance
#'                   level}
#'   \item{value}{value of hydrologic metric}
#'   \item{series}{value based on monthly, seasonal, or annual time series}
#'   \item{scenario}{MODFLOW scenario (e.g., "irr" or "no_irr")}
#'   \item{sim}{id of MODFLOW simulation}
#' }
"MODFLOW_metrics"
