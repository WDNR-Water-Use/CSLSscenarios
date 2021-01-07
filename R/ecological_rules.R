#' Dataset: Ecological rules for significant impact
#'
#' @docType data
#'
#' @usage data(ecological_rules)
#'
#' @format A data frame with the following columns.
#' \describe{
#' \item{hydrology}{type of hydrologic metric (magnitude, frequency, duration,
#'                  rate of change, or timing)}
#' \item{category}{category of ecological indicator (fish, plants, chemistry,
#'                 human_use)}
#' \item{indicator}{ecological indicator (e.g., "volume_habitat",
#'                  "good_spawning")}
#' \item{metric}{name of hydrologic metric related to this ecological indicator}
#' \item{variable}{identifier for different types of metric. For example, the
#'                 metric "exceedance_level" will have variables for each
#'                 exceedance level (10, 25, 50, 75, 90)}
#' \item{bathy_metric}{if applicable, name of child metric that needs to be
#'                     calculated from hydrologic metric before evaluating
#'                     significance}
#' \item{round_digits}{number of digits after the decimal place to round these
#'                     metric values and thresholds to}
#' \item{percent}{percent difference from base value allowed before change is
#'                considered a "significant impact"}
#' \item{difference}{additive difference from base value allowed before change
#'                   is considered a "significant impact"}
#' \item{significant_if}{whether sigificance occurs at "lower" values or
#'                       "higher" values compared to base value}
#' \item{Pleasant}{whether this indicator should be evaluated for Pleasant Lake}
#' \item{Long}{whether this indicator should be evaluated for Long Lake}
#' \item{Plainfield}{whether this indicator should be evaluated for Plainfield
#'                   Lake}
#' }
"ecological_rules"
