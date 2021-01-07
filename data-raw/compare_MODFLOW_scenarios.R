# Evaluate for ecological significance

library(dplyr)
library(reshape2)
library(NISTunits)

# 1. Decide on upper/lower bound scenarios -------------------------------------
MODFLOW_metrics  <- CSLSscenarios::MODFLOW_metrics
base_scenario    <- "no_irr"
compare_scenario <- "irr"

MODFLOW_pairs   <- left_join(filter(MODFLOW_metrics,
                                    .data$scenario == base_scenario),
                             filter(MODFLOW_metrics,
                                    .data$scenario == compare_scenario),
                             by = c("lake", "sim", "metric", "variable")) %>%
                   filter((.data$metric == "exceedance_level" &
                             .data$variable == "50")) %>%
                   mutate(value = .data$value.y - .data$value.x) %>%
                   select(.data$lake, .data$sim, .data$metric,
                          .data$variable, .data$value)
summary_metrics <- MODFLOW_pairs  %>%
                   group_by(.data$lake, .data$metric, .data$variable) %>%
                   summarise(q10 = quantile(.data$value,
                                            probs = 0.90,
                                            type = 6),
                             q90 = quantile(.data$value,
                                            probs = 0.10,
                                            type = 6),
                             .groups = "drop")
pick_bounds     <- MODFLOW_pairs %>%
                   left_join(summary_metrics) %>%
                   # Squared error (by pair)
                   mutate(SE_q10 = (.data$q10 - .data$value)^2,
                          SE_q90 = (.data$q90 - .data$value)^2) %>%
                   # Mean squared error (by lake)
                   group_by(.data$lake, .data$sim) %>%
                   summarise(MSE_q10 = sum(.data$SE_q10)/n(),
                             MSE_q90 = sum(.data$SE_q90)/n(),
                             .groups = "drop") %>%
                   group_by(.data$sim) %>%
                   # Sum MSE across lakes (by simulation)
                   summarise(MSE_q10 = sum(.data$MSE_q10),
                             MSE_q90 = sum(.data$MSE_q90),
                             .groups = "drop") %>%
                   # Ranked q10
                   arrange(.data$MSE_q10) %>%
                   mutate(rank_q10 = row_number()) %>%
                   # Ranked q90
                   arrange(.data$MSE_q90) %>%
                   mutate(rank_q90 = row_number()) %>%
                   # Pick the closest simulations
                   filter((.data$rank_q10 == 1 | .data$rank_q90 == 1))
use_scenarios   <- pick_bounds %>%
                   mutate(sim_type = ifelse(.data$rank_q10 == 1,
                           "permissive", "conservative")) %>%
                   select(.data$sim, .data$sim_type) %>%
                   rbind(data.frame(sim = 1, sim_type = "base"))

# 2. Compare scenarios ---------------------------------------------------------
comparison <- list()
i <- 1
for (sim in use_scenarios$sim) {
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

  this_comparison          <- compare_scenarios(this_base, this_scenario)
  this_comparison$sim      <- sim
  this_comparison$sim_type <- use_scenarios$sim_type[use_scenarios$sim == sim]
  comparison[[i]]          <- this_comparison
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
write.csv(MODFLOW_comparison, "comparison_all.csv",
          na = "",
          row.names = FALSE)
write.csv(limiting, "comparison_limiting_indicators.csv",
          na = "",
          row.names = FALSE)
