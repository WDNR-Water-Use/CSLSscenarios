#' Calculate frequency of mixed (unstratified) conditions
#'
#' Given a data frame with columns for "lake" and "level" as well as additional
#' information about the elevation at which lakes stratify, evaluate the
#' frequency of mixed conditions at all lakes with stratification information.
#'
#' @param this_hydro data frame with the hydrologic metrics (solute_budget) to
#'                   evaluate.
#' @param this_rule data frame with information on "percent", "difference",
#'                  "significant_if" (i.e., "higher" or "lower") for a single
#'                  indicator.
#' @param metric_uncertainty data frame with lake, metric, variable, and
#'                           allowable "difference" due to uncertainty in the
#'                           metric. Currently evaluated as the standard
#'                           deviation in the "no irrigation" scenarios of
#'                           the metric.
#' @param strat_info a data frame with the "elev_m" at which each "lake"
#'                   switches from stratified to mixed.
#' @return this_hydro, a data frame noting thresholds and impact.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @export

check_stratification <- function(this_rule,
                                 this_hydro,
                                 metric_uncertainty,
                                 strat_info = data.frame(lake = "Pleasant",
                                                         elev_m = 297.67)) {
  # Get max allowable difference in median from uncertainty
  impact <- evaluate_impact_rules(this_rule, metric_uncertainty)

  stratification <- NULL
  for (lake in strat_info$lake) {
    this_lake  <- this_hydro %>%
                  filter(.data$lake == !!lake) %>%
                  left_join(impact, by = "lake") %>%
                  mutate(threshold = strat_info$elev_m + .data$diff,
                         diff = .data$value2 - .data$value1,
                         threshold_diff = .data$threshold - .data$value1,
                         lower = ifelse(.data$value2 < .data$threshold,
                                        TRUE, FALSE),
                         higher = ifelse(.data$value2 > .data$threshold,
                                         TRUE, FALSE))
    this_lake$impacted <- this_lake[,impact$significant_if[1]]
    stratification <- rbind(stratification, this_lake)
  }

  return(stratification)
}
