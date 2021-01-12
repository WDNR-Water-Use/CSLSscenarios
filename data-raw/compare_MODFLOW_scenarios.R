# Evaluate for ecological significance

library(dplyr)
library(reshape2)
library(NISTunits)
library(CSLSscenarios)

# 1. Decide on upper/lower bound scenarios -------------------------------------
MODFLOW_metrics  <- CSLSscenarios::MODFLOW_metrics %>%
                    filter(.data$series == "month")

use_sims <- select_bounds(MODFLOW_metrics,
                          base_scenario = "no_irr",
                          compare_scenario = "irr")

# 2. Compare scenarios ---------------------------------------------------------
# Get standard deviations of metrics, for those with rules = no allowable change
rules              <- CSLSscenarios::ecological_rules
rule_metrics       <- unique(rules[, c("metric", "variable")])
metric_uncertainty <- MODFLOW_metrics %>%
                      filter(.data$scenario == "no_irr") %>%
                      inner_join(rule_metrics, (by = c("metric", "variable"))) %>%
                      group_by(.data$lake, .data$metric, .data$variable) %>%
                      summarise(difference = sd(.data$value, na.rm = TRUE),
                                .groups = "drop")


comparison <- list()
i <- 1
for (sim in use_sims$sim) {
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

  this_comparison          <- compare_scenarios(this_base,
                                                this_scenario,
                                                metric_uncertainty)
  this_comparison$sim      <- sim
  this_comparison$sim_type <- use_sims$sim_type[use_sims$sim == sim]
  comparison[[i]]          <- this_comparison
  i <- i + 1
}
MODFLOW_comparison <- bind_rows(comparison)


# Convert from meters to feet --------------------------------------------------
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
            group_by(.data$sim_type,
                     .data$lake,
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
           select(.data$sim_type,
                  .data$lake,
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

# Most limiting factors
most_limiting <- MODFLOW_comparison %>%
                 group_by(.data$sim_type,
                          .data$lake,
                          .data$hydrology,
                          .data$metric,
                          .data$variable,
                          .data$value1,
                          .data$value2,
                          .data$diff,
                          .data$significant_if) %>%
                 mutate(limit_threshold = ifelse(.data$significant_if == "lower",
                                                 max(.data$threshold, na.rm = TRUE),
                                                 min(.data$threshold, na.rm = TRUE))) %>%
                 filter(.data$limit_threshold == .data$threshold) %>%
                 ungroup() %>%
                 select(.data$sim,
                        .data$sim_type,
                        .data$lake,
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
usethis::use_data(MODFLOW_comparison, overwrite = TRUE, compress = "xz")
