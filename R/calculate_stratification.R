#' Calculate frequency of mixed (unstratified) conditions
#'
#' Given a data frame with columns for "lake" and "level" as well as additional
#' information about the elevation at which lakes stratify, evaluate the
#' frequency of mixed conditions at all lakes with stratification information.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param strat_info a data frame with the "elev_m" at which each "lake"
#'                   switches from stratified to mixed.
#' @return a data frame with the number of months and percent of time with
#'         mixed conditions at each lake included in "strat_info".
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @export

calculate_stratification <- function(df,
                                     strat_info = data.frame(lake = "Pleasant",
                                                             elev_m = 297.67)) {
  stratification <- NULL
  for (lake in strat_info$lake) {
    this_df       <- df %>% filter(.data$lake == !!lake)
    this_strat    <- strat_info %>% filter(.data$lake == !!lake)
    num_strat     <- sum(this_df$level >= this_strat$elev_m)
    percent_strat <- num_strat / length(this_df$level) * 100

    this_lake     <- data.frame(lake = lake,
                                metric = "stratification",
                                variable = c("num_strat",
                                             "percent_strat"),
                                value = c(num_strat,
                                          percent_strat))
    stratification <- rbind(stratification, this_lake)
  }

  return(stratification)
}
