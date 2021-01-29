#' Evaluate ecological impact rules to apply to metric values
#'
#' Given information about whether impact is determined by a multiplicative
#' factor (i.e., percent difference), additive difference (i.e., "difference")
#' and whether significance occurs at values higher or lower than the base
#' value, synthesizes this into a list for use in calculations of thresholds.
#'
#' @param this_rule data frame with information on "percent", "difference",
#'                  "significant_if" (i.e., "higher" or "lower") for a single
#'                  indicator.
#' @param metric_uncertainty data frame with lake, metric, variable, and
#'                           allowable "difference" due to uncertainty in the
#'                           metric. Currently evaluated as the standard
#'                           deviation in the "no irrigation" scenarios of
#'                           the metric.
#'
#' @return impact, a list with values for "factor", "difference" and
#'         "significant_if"
#'
#' @export

evaluate_impact_rules <- function(this_rule, metric_uncertainty) {
  impact <- NULL

  # Multiplicative factors
  if (!is.na(this_rule$percent) & this_rule$significant_if == "lower") {
    impact$factor <- (1 - this_rule$percent/100)
  } else if (!is.na(this_rule$percent) & this_rule$significant_if == "higher") {
    impact$factor <- (1 + this_rule$percent/100)
  } else {
    impact$factor <- 1
  }
  # Additive differences
  if (!is.na(this_rule$difference) & this_rule$significant_if == "lower") {
    impact$diff <- -this_rule$difference
  } else if (!is.na(this_rule$difference) & this_rule$significant_if == "higher") {
    impact$diff <- this_rule$difference
  } else {
    impact$diff <- 0
  }


  # No change allowed, allow standard deviation
  this_metric  <- metric_uncertainty %>%
                  filter(.data$metric == this_rule$metric,
                         .data$variable == this_rule$variable)
  impact$lake  <- this_metric$lake
  if (impact$diff == 0 & impact$factor == 1) {
    if (this_rule$significant_if == "lower") {
      impact$diff <- -this_metric$difference
    } else if (this_rule$significant_if == "higher") {
      impact$diff <- this_metric$difference
    }
  }
  impact$significant_if <- this_rule$significant_if
  impact <- as.data.frame(impact)

  # If no change allowed AND standard deviation is zero, allow 1% change
  for (i in 1:nrow(impact)) {
    if (impact$diff[i] == 0 & impact$factor[i] == 1) {
      if (impact$significant_if[i] == "lower") {
        impact$factor[i] <- 0.99
      } else if (impact$significant_if[i] == "higher") {
        impact$diff[i]   <- 1.01
      }
    }
  }



  return(impact)
}
