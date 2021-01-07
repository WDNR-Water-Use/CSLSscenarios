#' Calculate frequency of good paddleboat conditions
#'
#' Given a data frame with columns for "lake" and "level" as well as additional
#' information about the elevation at which lake becomes suitable for
#' paddleboating, evaluate the frequency of good paddleboating conditions during
#' warm months (April thorugh September) at all lakes with paddleboat
#' information.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param paddle_info a data frame with the "elev_m" at which each "lake"
#'                   becomes good for paddleboating.
#' @return a data frame with the number of months and percent of time with
#'         good paddleboating conditions at each lake included in "paddle_info".
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @export

calculate_paddleboat <- function(df,
                                 paddle_info = data.frame(lake = "Long",
                                                          elev_m = 334.29)) {
  paddleboat <- NULL
  for (lake in paddle_info$lake) {
    this_df       <- df %>%
                     filter(.data$lake == !!lake,
                            .data$month %in% c(4:9))
    this_paddle   <- paddle_info %>% filter(.data$lake == !!lake)

    num_ok        <- sum(this_df$level >= this_paddle$elev_m)
    percent_ok    <- num_ok / length(this_df$level) * 100

    this_lake     <- data.frame(lake = lake,
                                metric = "paddleboat",
                                variable = c("num_ok",
                                             "percent_ok"),
                                value = c(num_ok,
                                          percent_ok))

    paddleboat <- rbind(paddleboat, this_lake)
  }

  return(paddleboat)
}
