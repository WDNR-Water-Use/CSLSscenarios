# Calculate imputed lake levels

library(tidyverse)
library(lubridate)
library(usethis)
library(CSLSscenarios)
library(NISTunits)
library(reshape2)

# PARAMETERS: Lakes of interest
lakes   <- c("Pleasant", "Long", "Plainfield")

# DATA: Load MODFLOW data for climate runs
# Allow for a burn-in period, nix first 5 years (start at 1986)
MODFLOW <- CSLSdata::MODFLOW %>%
           filter(.data$scenario %in% c("irr", "no_irr"),
                  year(.data$date) >= 1986,
                  year(.data$date) <= 2018) %>%
           select(scenario = .data$scenario,
                  sim = .data$sim,
                  lake = .data$lake,
                  date = .data$date,
                  level = .data$level_m)

# CALCULATIONS (1/2): Calculate hydrologic metrics for each sim for each lake --
# Convert levels to max depths for fairer CV calcs
lake_bottom <- data.frame(lake = lakes,
                          bottom = c(291.1426, 332.8622, 332.0755))
MODFLOW     <- left_join(MODFLOW, lake_bottom, by = "lake") %>%
               mutate(depth = .data$level - .data$bottom)
MODFLOW$bottom <- NULL
MODFLOW$lake   <- factor(MODFLOW$lake, levels = lakes)

# Initialize lists for saving ouptuts
scenario_metrics <- list()
i <- 1
for (scenario in c("no_irr", "irr")) {
  message(sprintf("Starting scenario %s", scenario))
  # for (sim in unique(MODFLOW$sim)) {
  for (sim in c(1)) { # Limit to just base run for now (but ultimately change)
    if (sim %% 20 == 0) {
      message(sprintf("Starting sim %s", sim))
    }
    # Filter to this simulation of this type of scenario
    this_series <- MODFLOW %>%
                   filter(.data$scenario == !!scenario,
                          .data$sim == !!sim)

    # If irr scenario, use no_irr exceedance levels to calculate durations
    if (scenario == "irr") {
      dur_exceeds <- scenario_metrics_no_irr %>%
                     filter(.data$scenario == "no_irr",
                            .data$sim == !!sim,
                            .data$metric == "exceedance_level") %>%
                     select(.data$lake, .data$variable, .data$value,
                            .data$series) %>%
                     dcast(lake+series~variable, value.var = "value")
    } else {
      dur_exceeds <- NULL
    }

    # Calculate hydrologic metrics
    this_metric <- calculate_metrics(this_series,
                                     metrics = c("median_level",
                                                 "cv_level",
                                                 "exceedance_level",
                                                 "volume",
                                                 "area",
                                                 "mean_depth",
                                                 "max_depth",
                                                 "centrarchid_substrate",
                                                 "vegetation_area",
                                                 "exceedance_range",
                                                 "depart_median",
                                                 "median_dur",
                                                 "cv_dur",
                                                 "num_dur",
                                                 "num_2yr",
                                                 "median_rise_rate",
                                                 "cv_rise_rate",
                                                 "median_fall_rate",
                                                 "cv_fall_rate",
                                                 "fast_rise",
                                                 "fast_fall",
                                                 "good_spawning",
                                                 "move_dock",
                                                 "turtle_bay",
                                                 "stratification",
                                                 "paddleboat",
                                                 "motorboat"),
                                     dur_exceeds = dur_exceeds)

    # Track scenario, sim, and add to list
    this_metric$scenario  <- scenario
    this_metric$sim       <- sim
    scenario_metrics[[i]] <- this_metric
    i <- i + 1
  }
  scenario_metrics_no_irr     <- bind_rows(scenario_metrics)
}
MODFLOW_metrics      <- bind_rows(scenario_metrics)
MODFLOW_metrics$lake <- factor(MODFLOW_metrics$lake, levels = lakes)

# CALCULATIONS (2/2): ----------------------------------------------------------------------
comparison <- list()
i <- 1
for (sim in unique(MODFLOW_metrics$sim)) {
  this_base     <- MODFLOW_metrics %>%
                   filter(.data$sim == !!sim,
                          .data$scenario == "no_irr",
                          .data$series == "month") %>%
                   select(.data$lake, .data$metric, .data$variable, .data$value)
  this_scenario <- MODFLOW_metrics %>%
                   filter(.data$sim == !!sim,
                          .data$scenario == "irr",
                          .data$series == "month") %>%
                   select(.data$lake, .data$metric, .data$variable, .data$value)

  this_comparison <- compare_scenarios(this_base, this_scenario)
  comparison[[i]] <- this_comparison
  i <- i + 1
}
MODFLOW_comparison <- bind_rows(comparison)

meter_to_ft <- MODFLOW_comparison %>%
               filter(.data$metric %in% c("exceedance_level",
                                          "exceedance_range")) %>%
               mutate_at(c("value1", "value2", "diff",
                           "threshold", "threshold_diff"),
                         NISTmeterTOft)

MODFLOW_comparison <- MODFLOW_comparison %>%
                      filter(!.data$metric %in% c("exceedance_level",
                                                  "exceedance_range"))
MODFLOW_comparison <- rbind(MODFLOW_comparison,
                            meter_to_ft) %>%
                      arrange(.data$lake, .data$hydrology, .data$metric,
                              .data$variable, .data$category)


# Limiting factors
limiting <- MODFLOW_comparison %>%
            group_by(.data$lake,
                     .data$hydrology,
                     .data$metric,
                     .data$variable,
                     .data$value1,
                     .data$value2,
                     .data$diff,
                     .data$category,
                     .data$significant_if) %>%
            mutate(limit_threshold = ifelse(.data$significant_if == "lower",
                                         max(.data$threshold, na.rm = TRUE),
                                         min(.data$threshold, na.rm = TRUE))) %>%
            filter(.data$limit_threshold == .data$threshold) %>%
            ungroup() %>%
            select(.data$lake,
                   .data$hydrology,
                   .data$metric,
                   .data$variable,
                   .data$category,
                   .data$indicator,
                   .data$impacted,
                   .data$significant_if,
                   .data$value1,
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
write.csv(MODFLOW_comparison, "comparison_all.csv",
          na = "",
          row.names = FALSE)
write.csv(limiting, "comparison_limiting_indicators.csv",
          na = "",
          row.names = FALSE)

# SAVE: Write out
# usethis::use_data(MODFLOW_metrics, overwrite = TRUE, compress = "xz")
