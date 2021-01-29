#' Extrapolate relationship between elevation and bathymetric metrics
#'
#' Assumes ecological rule is a percent change (increase or decrease) from
#' baseline value (e.g., 10% decrease), not a numerical difference (e.g., 2
#' fewer times).
#'
#' Because so many bathymetric relationships are non-monotonic, had to come up
#' with a hacky way of finding correct elevation match for given threshold
#' child bathy parameter. From check on monotonic child parameters, should yield
#' accurate threshold elevations to within 1 cm.
#'
#'
#' @param this_hydro data frame with the hydrologic metrics (exceedance levels) to
#'                 evaluate.
#' @param this_rule data frame with the ecological rules for given parameter to
#'                 evaluate.
#' @param bathymetry data frame with relationships between lake elevation and
#'                   other parameters (e.g., lake area, lake volume, plant area,
#'                   substrate area)
#' @param bathy_metric name of column in bathymetry to evaluate
#' @param metric_uncertainty data frame with lake, metric, variable, and
#'                           allowable "difference" due to uncertainty in the
#'                           metric. Currently evaluated as the standard
#'                           deviation in the "no irrigation" scenarios of
#'                           the metric.
#'
#' @return impact_evaluation, a data frame with the following columns:
#' \item{lake}{name of lake, character}
#' \item{metric}{hydrologic metric, i.e. "exceedance_level"}
#' \item{variable}{hydrologic metric variable, i.e. 10, 25, 50, 75, or 90}
#' \item{value1}{baseline value for hydrologic metric}
#' \item{threshold}{threshold value for hydrologic metric for impact}
#' \item{value2}{scenario value for hydrologic metric}
#' \item{impacted}{logical, indicates whether lake is impacted relative to this
#'                 ecological indicator under this scenario (TRUE) or not
#'                 (FALSE)}
#'
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats approxfun
#' @importFrom dplyr filter mutate select
#' @importFrom stats approx
#'
#' @export

extrapolate_bathymetry <- function(this_hydro,
                                   this_rule,
                                   bathymetry,
                                   bathy_metric,
                                   metric_uncertainty) {

  impact_evaluation <- NULL
  for (lake in unique(this_hydro$lake)) {
    # Filter data frames to lake
    this_lake    <- this_hydro %>% filter(.data$lake == !!lake)
    this_bathy   <- bathymetry %>%
                    filter(.data$lake == !!lake) %>%
                    arrange(desc(.data$elev_m))
    this_bathy   <- this_bathy[,c(bathy_metric, "elev_m")]
    colnames(this_bathy) <- c("x", "y") # For ease in referring to bathy_metric
    this_bathy   <- this_bathy %>% filter(!is.na(.data$x))

    # Not all bathy metrics apply to each lake, only evaluate those that exist
    if (sum(!is.na(this_bathy$x)) > 0) {

      # Convert baseline elevation to child bathy parameter
      f_elev_bathy     <- approxfun(x = this_bathy$y,
                                    y = this_bathy$x)
      this_lake$bathy1 <- f_elev_bathy(this_lake$value1)
      this_lake$bathy2 <- f_elev_bathy(this_lake$value2)

      # Get impact rules
      impact <- evaluate_impact_rules(this_rule, metric_uncertainty) %>%
                filter(.data$lake == !!lake)

      # Threshold for child bathy parameter
      this_lake$bathy_threshold <- impact$factor*this_lake$bathy1+impact$diff

      # Estimate threshold elevations for this lake
      this_lake$threshold <- NA
      for (i in 1:nrow(this_lake)) {
        bathy_threshold <- this_lake$bathy_threshold[i]
        value1          <- this_lake$value1[i]
        # Filter to just elevations below base elevation (search downward)
        # Find where difference switiches from positive to negative for the
        # first time - desired match should be between these values.
        # Then, create tiny approx function just for this section which is
        # guaranteed to be monotonic with respect to x (the bathy metric)
        find_bathy <- this_bathy %>%
                      mutate(lower = bathy_threshold < .data$x) %>%
                      filter(.data$y < value1)
        logical    <- find_bathy$lower[1]
        opposites  <- which(find_bathy$lower != logical)
        # For some metrics (e.g., "upland_decrease") there may not be any point
        # at which you hit a threshold (because upland vegeation always
        # increases, never decreases, as lake levels drop).
        # If so, skip it (assign NA)
        if (length(opposites) > 0) {
          index      <- min(which(find_bathy$lower != logical))
          threshold  <- approx(find_bathy$x[(index-1):index],
                               find_bathy$y[(index-1):index],
                               bathy_threshold)$y
        } else {
          threshold <- NA
        }
        this_lake$threshold[i] <- threshold
      }

      # Impact thresholds
      this_impact <- this_lake %>%
                     mutate(threshold = .data$threshold,
                            value1 = .data$value1,
                            value2 = .data$value2,
                            impacted = ifelse(.data$value2 < .data$threshold,
                                           TRUE, FALSE),
                            diff = .data$value2 - .data$value1,
                            threshold_diff = .data$threshold - .data$value1,
                            bathy_diff = .data$bathy2 - .data$bathy1,
                            bathy_threshold_diff = .data$bathy_threshold - .data$bathy1) %>%
                     select(.data$lake, .data$metric, .data$variable,
                            .data$value1, .data$value2, .data$threshold,
                            .data$diff, .data$threshold_diff, .data$bathy1,
                            .data$bathy2, .data$bathy_threshold,
                            .data$bathy_diff, .data$bathy_threshold_diff,
                            .data$impacted)
      impact_evaluation <- rbind(impact_evaluation, this_impact)
    }
  }

  return(impact_evaluation)
}
