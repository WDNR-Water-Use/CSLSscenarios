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
           filter(.data$scenario %in% c("cur_irr", "no_irr"),
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
for (scenario in c("no_irr", "cur_irr")) {
  message(sprintf("Starting scenario %s", scenario))
  for (sim in unique(MODFLOW$sim)) {
  # for (sim in c(1)) { # Limit to just base run for now (but ultimately change)
    if (sim %% 20 == 0) {
      message(sprintf("Starting sim %s", sim))
    }
    # Filter to this simulation of this type of scenario
    this_series <- MODFLOW %>%
                   filter(.data$scenario == !!scenario,
                          .data$sim == !!sim)

    # Always use no_irr exceedance levels to calculate durations
    if (scenario != "no_irr") {
      dur_exceeds <- scenario_metrics_no_irr %>%
                     filter(.data$scenario == "no_irr",
                            .data$sim == !!sim,
                            .data$metric == "exceedance_level") %>%
                     select(.data$lake, .data$variable, .data$value,
                            .data$series) %>%
                     dcast(lake+series~variable, value.var = "value")
    } else {
      dur_exceeds = NULL
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
                                                 "motorboat",
                                                 "season_compare"),
                                     dur_exceeds = dur_exceeds)

    # Track scenario, sim, and add to list
    this_metric$scenario  <- scenario
    this_metric$sim       <- sim
    scenario_metrics[[i]] <- this_metric
    i <- i + 1
  }
  if (scenario == "no_irr") {
    scenario_metrics_no_irr <- bind_rows(scenario_metrics)
  }
}
MODFLOW_metrics      <- bind_rows(scenario_metrics)
MODFLOW_metrics$lake <- factor(MODFLOW_metrics$lake, levels = lakes)

# SAVE: Write out
usethis::use_data(MODFLOW_metrics, overwrite = TRUE, compress = "xz")
