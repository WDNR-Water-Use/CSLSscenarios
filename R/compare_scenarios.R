#' Compare scenarios
#'
#' This function compares two time series and evaluates the second series for
#' ecologically significant differences from the first.
#'
#' @param df1 data frame with baseline hydrologic metrics
#' @param df2 data frame with hydrologic metrics of scenario being evaluated for
#'            significant impact.
#' @param metric_uncertainty data frame with lake, metric, variable, and
#'                           allowable "difference" due to uncertainty in the
#'                           metric. Currently evaluated as the standard
#'                           deviation in the "no irrigation" scenarios of
#'                           the metric.
#' @param rules data frame with ecological rules for ecological indicators
#'              related to hydrologic metrics.
#' @param bathymetry data frame with bathymetric relationships with parameters like
#'                   lake area, lake volume, plant area, etc.
#'
#' @return comparison, a data frame with the following columns:
#' \item{lake}{name of lake}
#' \item{hydrology}{type of hydrologic metric (e.g., magnitude)}
#' \item{metric}{hydrologic metric (e.g., exceedance_level)}
#' \item{variable}{type of hydrologic metric (e.g. "90" for 90th percentile
#'                 exceedance level)}
#' \item{category}{category of ecological indicator (e.g., plants, fish)}
#' \item{indicator}{ecological indicator (e.g., volume_habitat)}
#' \item{impacted}{logical, TRUE if this ecological indicator is impacted from
#'                 baseline under this scenario}
#' \item{significant_if}{notes whether scenario is significant if scenario
#'                       values are "higher" or "lower" than threshold values}
#' \item{value1}{base value of hydrologic metric}
#' \item{threshold}{threshold value of hydrologic metric}
#' \item{value2}{scenario value of hydrologic metric}
#' \item{threshold_diff}{difference between threshold and base value of
#'                       hydrologic metric}
#' \item{diff}{difference between scenario and base value of hydrologic metric}
#' \item{bathy_significant_if}{notes whether scenario is significant if scenario
#'                             child/bathymetric values are "higher" or "lower"
#'                             than child/bathymetric threshold values}
#' \item{bathy1}{base value of child/bathymetric metric (if applicable)}
#' \item{bathy_threshold}{threshold value of child/bathymetric metric (if
#'                        applicable)}
#' \item{bathy2}{scenario value of child/bathymetric metric (if applicable)}
#' \item{bathy_threshold_diff}{difference between bathy_threshold and bathy1}
#' \item{bathy_diff}{difference between bathy2 and bathy1}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_remove
#'
#' @export

compare_scenarios <- function(df1,
                              df2,
                              metric_uncertainty,
                              rules = CSLSscenarios::ecological_rules,
                              bathymetry = CSLSdata::bathymetry) {

  combined           <- left_join(df1, df2, by = c("lake", "metric", "variable"))
  colnames(combined) <- c("lake", "metric", "variable", "value1", "value2")

  comparison <- list()
  i <- 1

  for (indicator in 1:nrow(rules)) {
    # Ecological rules for current ecological indicator ------------------------
    this_rule  <- rules[indicator,]

    # Hydrologic metric related to this ecological indicator -------------------
    # Filter to variable, if specified
    this_hydro <- combined %>%
                  filter(.data$metric == this_rule$metric,
                         .data$variable == this_rule$variable)

    # Determine threshold for impact, assess if value2 is impacted -------------
    if (this_rule$indicator %in% c("low_solute_median","high_solute_median")) {
      this_hydro <- combined %>%
                    filter(.data$metric == this_rule$metric)
      this_hydro <- evaluate_solute_median(this_hydro,
                                           this_rule,
                                           metric_uncertainty)

    } else if (this_rule$indicator == "stratification" &
               this_rule$metric == "exceedance_level") {
      this_hydro <- check_stratification(this_rule,
                                         this_hydro,
                                         metric_uncertainty)

    } else if (this_rule$bathy_metric != "") {
      # Calculate rule on child metric related to bathymetry
      this_hydro <- extrapolate_bathymetry(this_hydro, this_rule,
                                           bathymetry, this_rule$bathy_metric,
                                           metric_uncertainty)
    } else {
      # Calculate rule on hydrologic metric, straight up
      impact     <- evaluate_impact_rules(this_rule, metric_uncertainty)
      this_hydro <- this_hydro %>%
                    left_join(impact, by = "lake") %>%
                    mutate(threshold = .data$factor*.data$value1 + .data$diff,
                           diff = .data$value2 - .data$value1,
                           threshold_diff = .data$threshold - .data$value1,
                           lower = ifelse(.data$value2 < .data$threshold,
                                          TRUE, FALSE),
                           higher = ifelse(.data$value2 > .data$threshold,
                                           TRUE, FALSE))
      this_hydro$impacted <- this_hydro[,impact$significant_if[1]]
    }

    # Add back in additional information about this ecological indicator -------
    vals <- this_hydro %>%
            mutate(hydrology = this_rule$hydrology,
                   category = this_rule$category,
                   indicator = this_rule$indicator,
                   significant_if = this_rule$significant_if)
    comparison[[i]] <- vals; i <- i + 1
  }

  # Combine all indicators -----------------------------------------------------
  # Filter to only those flagged "keep"
  # Track bathymetry significant_if directions
  keep_indicators <- rules %>%
                     select(.data$metric, .data$variable, .data$indicator,
                            .data$significant_if, .data$Pleasant, .data$Long,
                            .data$Plainfield) %>%
                     melt(id.vars = c("metric", "variable", "indicator",
                                      "significant_if"))
  colnames(keep_indicators) <- c("metric", "variable", "indicator",
                                 "significant_if", "lake", "keep")
  comparison <- bind_rows(comparison) %>%
                left_join(keep_indicators,
                          by = c("lake", "metric", "variable", "indicator",
                                 "significant_if")) %>%
                filter(.data$keep) %>%
                mutate(bathy_significant_if = .data$significant_if,
                       significant_if = ifelse(.data$metric == "exceedance_level" &
                                                 !is.na(.data$bathy1),
                                               "lower",
                                               .data$significant_if)) %>%
                select(.data$lake,
                       .data$hydrology,
                       .data$metric,
                       .data$variable,
                       .data$category,
                       .data$indicator,
                       .data$impacted,
                       .data$significant_if,
                       .data$value1,
                       .data$compare1,
                       .data$threshold,
                       .data$value2,
                       .data$threshold_diff,
                       .data$diff,
                       .data$bathy_significant_if,
                       .data$bathy1,
                       .data$bathy_threshold,
                       .data$bathy2,
                       .data$bathy_threshold_diff,
                       .data$bathy_diff)

  # Merge plant increase/decrease thresholds -----------------------------------
  plants_df <- comparison %>%
               filter(.data$metric == "exceedance_level",
                      .data$category == "plants",
                      .data$indicator != "threatened_plant")
  plants_new <- plants_df %>%
                mutate(plant_type = str_remove(str_remove(.data$indicator,
                                                          "_increase"),
                                               "_decrease")) %>%
                filter(!is.na(.data$threshold)) %>%
                group_by(.data$lake, .data$metric, .data$variable,
                         .data$plant_type) %>%
                mutate(threshold_limit = max(.data$threshold)) %>%
                ungroup() %>%
                filter(.data$threshold == .data$threshold_limit) %>%
                mutate(indicator = .data$plant_type) %>%
                select(colnames(comparison))
  comparison <- comparison %>%
                filter(!.data$indicator %in% c(plants_df$indicator)) %>%
                rbind(plants_new)

  return(comparison)
}
