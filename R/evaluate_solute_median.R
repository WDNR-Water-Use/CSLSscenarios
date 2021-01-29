#' Evaluate median solute concentration
#'
#' Given metrics related to solute concentration, evaluate whether new median
#' concentration is lower thant the 90% concentration or higher than the 10%
#' concentration.
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
#'
#' @return this_hydro, a data frame noting thresholds and impact.
#'
#' @export

evaluate_solute_median <- function(this_hydro,
                                   this_rule,
                                   metric_uncertainty) {

  # Get max allowable difference in median from uncertainty
  impact <- evaluate_impact_rules(this_rule, metric_uncertainty)

  if (this_rule$significant_if == "higher") {
    compare_hydro  <- this_hydro %>%
                      filter(.data$variable == "q10") %>%
                      mutate(variable = "median",
                             compare1 = .data$value1)  %>%
                      select(.data$lake, .data$compare1)
    this_hydro    <- left_join(this_hydro, compare_hydro,
                               by = c("lake")) %>%
                     filter(.data$variable == "median")
  } else if (this_rule$significant_if == "lower") {
    compare_hydro  <- this_hydro %>%
                      filter(.data$variable == "q90") %>%
                      mutate(variable = "median",
                             compare1 = .data$value1) %>%
                      select(.data$lake, .data$compare1)
    this_hydro   <- left_join(this_hydro, compare_hydro,
                              by = c("lake")) %>%
                    filter(.data$variable == "median")
  }

  # Evaluate compared to appropriate threshold
  this_hydro <- this_hydro %>%
                left_join(impact, by = "lake") %>%
                mutate(threshold = .data$factor*.data$compare1 + .data$diff,
                       diff = .data$value2 - .data$compare1,
                       threshold_diff = .data$threshold - .data$compare1,
                       lower = ifelse(.data$value2 < .data$threshold,
                                      TRUE, FALSE),
                       higher = ifelse(.data$value2 > .data$threshold,
                                       TRUE, FALSE))
  this_hydro$impacted <- this_hydro[,impact$significant_if[1]]

  return(this_hydro)
}
