#' Calculate frequency of legal motorboat conditions
#'
#' Given a data frame with columns for "lake" and "level" as well as additional
#' information about the elevation at which lakes are suitable for motorboating
#' (lake area > 62.5 acres), evaluate the frequency of legal motorboating
#' conditions at all lakes with motorboat information.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param boat_info a data frame with the "elev_m" at which each "lake"
#'                   switches from legal to not legal for motorboating.
#' @return a data frame with the number of months and percent of time with
#'         mixed conditions at each lake included in "boat_info".
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @export

calculate_motorboat <- function(df,
                                boat_info = data.frame(lake = "Pleasant",
                                                       elev_m = 292.6422)) {
  motorboat <- NULL
  for (lake in boat_info$lake) {
    this_df     <- df %>%
                   filter(.data$lake == !!lake,
                          .data$month %in% c(4:9))
    this_boat   <- boat_info %>% filter(.data$lake == !!lake)

    num_ok      <- sum(this_df$level >= this_boat$elev_m)
    percent_ok  <- num_ok / length(this_df$level) * 100

    this_lake     <- data.frame(lake = lake,
                                metric = "motorboat",
                                variable = c("num_ok",
                                             "percent_ok"),
                                value = c(num_ok,
                                          percent_ok))
    motorboat <- rbind(motorboat, this_lake)
  }

  return(motorboat)
}
