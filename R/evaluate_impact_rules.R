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
#'
#' @return impact, a list with values for "factor", "difference" and
#'         "significant_if"
#'
#' @export

evaluate_impact_rules <- function(this_rule) {
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

  impact$significant_if <- this_rule$significant_if

  return(impact)
}
