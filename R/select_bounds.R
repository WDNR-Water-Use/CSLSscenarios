#' Select upper/lower bounds for comparisons
#'
#' Selects a "conservative" and "permissive" MODFLOW simulation for evaluation
#' of impacts.
#'
#' @param df a data frame with MODFLOW metrics
#' @param base_scenario name of base scenario, defaults to "no_irr"
#' @param compare_scenario name of scenario to evaluate for impacts relative to
#'                         the base scenario, defaults to "irr".
#'
#' @return use_sims, a data frame with the following columns:
#' \item{sim}{simulations numbers to use}
#' \item{sim_type}{notes whether this simulation represents a "conservative" or
#'                 "permissive" determination of impact}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats quantile
#' @import dplyr
#'
#' @export

select_bounds <- function(df,
                          base_scenario = "no_irr",
                          compare_scenario = "irr") {

  pairs_df       <- left_join(filter(df, .data$scenario == base_scenario),
                              filter(df, .data$scenario == compare_scenario),
                              by = c("lake", "sim", "metric", "variable")) %>%
                     filter((.data$metric == "exceedance_level" &
                               .data$variable == "50")) %>%
                     mutate(value = .data$value.y - .data$value.x) %>%
                     select(.data$lake, .data$sim, .data$metric,
                            .data$variable, .data$value)
  summary_metrics <- pairs_df  %>%
                     group_by(.data$lake, .data$metric, .data$variable) %>%
                     summarise(q10 = quantile(.data$value,
                                              probs = 0.90,
                                              type = 6),
                               q90 = quantile(.data$value,
                                              probs = 0.10,
                                              type = 6),
                               .groups = "drop")
  pick_bounds     <- pairs_df %>%
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
  use_sims        <- pick_bounds %>%
                     mutate(sim_type = ifelse(.data$rank_q10 == 1,
                                              "permissive", "conservative")) %>%
                     select(.data$sim, .data$sim_type) %>%
                     rbind(data.frame(sim = 1, sim_type = "base"))

  return(use_sims)
}
