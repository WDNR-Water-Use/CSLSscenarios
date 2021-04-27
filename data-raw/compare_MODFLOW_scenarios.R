# Evaluate for ecological significance
# Compare other scenarios to "no-irrigated-agriculture" scenario metrics

library(dplyr)
library(reshape2)
library(NISTunits)
library(CSLSscenarios)

# 1. Decide on small drawdown/large drawdown simulations -----------------------
MODFLOW_Mg_metrics <- CSLSfluxes::MODFLOW_Mg_metrics %>%
                      mutate(series = "month")
MODFLOW_metrics    <- CSLSscenarios::MODFLOW_metrics %>%
                      filter(.data$series == "month") %>%
                      bind_rows(MODFLOW_Mg_metrics)
use_sims           <- select_bounds(MODFLOW_metrics,
                                    base_scenario = "no_irr",
                                    compare_scenario = "cur_irr")

# 2. CALCULATIONS: Compare scenarios -------------------------------------------
# Get standard deviations of metrics, for those with rules = no allowable change
rules              <- CSLSscenarios::ecological_rules
rule_metrics       <- unique(rules[, c("metric", "variable")])
metric_uncertainty <- MODFLOW_metrics %>%
                      filter(.data$scenario == "no_irr") %>%
                      group_by(.data$lake, .data$metric, .data$variable) %>%
                      summarise(difference = sd(.data$value, na.rm = TRUE),
                                .groups = "drop")


comparison <- list()
i <- 1

# 2a. Current irrigation -------------------------------------------------------
scenario <- "cur_irr"
these_sims <- use_sims$sim
for (sim in these_sims) {
  this_base     <- MODFLOW_metrics %>%
                   filter(.data$sim == !!sim,
                          .data$scenario == "no_irr",
                          .data$series == "month") %>%
                   select(.data$lake, .data$metric, .data$variable, .data$value)
  this_scenario <- MODFLOW_metrics %>%
                   filter(.data$sim == !!sim,
                          .data$scenario == !!scenario,
                          .data$series == "month") %>%
                   select(.data$lake, .data$metric, .data$variable, .data$value)

  this_comparison <- compare_scenarios(this_base,
                                       this_scenario,
                                       metric_uncertainty)
  this_comparison$sim      <- sim
  this_comparison$scenario <- scenario
  this_comparison$sim_type <- use_sims$sim_type[use_sims$sim == sim]
  comparison[[i]] <- this_comparison
  i <- i + 1
}

# 2b. Potential future irrigation ----------------------------------------------
scenario   <- "fut_irr"
these_sims <- 1
for (sim in these_sims) {
  this_base     <- MODFLOW_metrics %>%
                   filter(.data$sim == !!sim,
                          .data$scenario == "no_irr",
                          .data$series == "month") %>%
                   select(.data$lake, .data$metric, .data$variable, .data$value)
  this_scenario <- MODFLOW_metrics %>%
                   filter(.data$sim == !!sim,
                          .data$scenario == !!scenario,
                          .data$series == "month") %>%
                   select(.data$lake, .data$metric, .data$variable, .data$value)

  this_comparison <- compare_scenarios(this_base,
                                       this_scenario,
                                       metric_uncertainty)
  this_comparison$sim      <- sim
  this_comparison$scenario <- scenario
  this_comparison$sim_type <- "base"
  comparison[[i]]          <- this_comparison
  i <- i + 1
}

# 2c. Wells Off ----------------------------------------------------------------
scenario <- "wells_off"
this_scenario <- MODFLOW_metrics %>% filter(.data$scenario == !!scenario)
these_sims    <- unique(this_scenario$sim)
this_base     <- MODFLOW_metrics %>%
                 filter(.data$sim == 1,
                        .data$scenario == "no_irr",
                        .data$series == "month") %>%
                 select(.data$lake, .data$metric, .data$variable, .data$value)
for (sim in these_sims) {
  this_scenario <- MODFLOW_metrics %>%
                   filter(.data$sim == !!sim,
                          .data$scenario == !!scenario,
                          .data$series == "month") %>%
                   select(.data$lake, .data$metric, .data$variable, .data$value)

  this_comparison <- compare_scenarios(this_base,
                                       this_scenario,
                                       metric_uncertainty)
  this_comparison$sim      <- sim
  this_comparison$scenario <- scenario
  this_comparison$sim_type <- "base"
  comparison[[i]] <- this_comparison
  i <- i + 1
}

# Combine
MODFLOW_comparison <- bind_rows(comparison)

# 3. UNIT CONVERSIONS ----------------------------------------------------------
# Convert from meters to feet
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

# 4. SAVE: Write out -----------------------------------------------------------
usethis::use_data(MODFLOW_comparison, overwrite = TRUE, compress = "xz")
write.csv(MODFLOW_comparison,
          file = "inst/csv/MODFLOW_comparison.csv",
          na = "",
          row.names = FALSE)
