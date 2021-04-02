#' Calculate centrarchid substrate area at given Pleasant Lake elevation
#'
#' Given a data frame with the exceedance level metrics for a single set of
#' simulations at Pleasant Lake, calculate the associated substrate area for
#' centrarchid fish. Uses CSLSdata::fish_substrate, but converts areas from
#' acres to m^2 to be consistent with other area metrics.
#'
#' @param df a data frame with a "lake", "metric", "variable", and "value"
#'          columns where one of the metrics is "exceedance_level".
#'
#' @return substrate, a data frame with "lake", "metric", "variable", and
#'         "value" where the metric is "centrarchid_substrate", the variables
#'         are all the exceedance level probabilities inputted in df, and the
#'         value is the mean substrate hardness at generalized centrarchid
#'         spawning depth.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate select
#' @importFrom NISTunits NISTacreTOsqrMeter
#' @importFrom stats approxfun
#'
#' @export

calculate_substrate <- function(df) {

  # Exceedance levels
  # Only evaluate for Pleasant Lake (only study lake where substrate area
  # matters for fish)
  df <- df %>%
        filter(.data$lake == "Pleasant",
               .data$metric == "exceedance_level") %>%
        mutate(probs = .data$variable)

  # Fish substrate mean hardness at spawning depth
  fish_substrate  <- CSLSdata::bathymetry %>%
                     filter(.data$lake == "Pleasant")
  approx_spawn_Hrd <- approxfun(x = fish_substrate$elev_m,
                               y = fish_substrate$meanSubHrd)
  df$centrarchid_substrate <- approx_spawn_Hrd(df$value)

  substrate <- df %>%
               select(.data$lake, .data$probs, .data$centrarchid_substrate) %>%
               melt(id.vars = c("lake", "probs")) %>%
               select(lake = .data$lake,
                      metric = .data$variable,
                      variable = .data$probs,
                      value = .data$value)
  return(substrate)
}
