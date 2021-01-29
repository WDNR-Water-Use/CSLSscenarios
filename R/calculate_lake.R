#' Calculate frequency of lake and flood conditions
#'
#' Given a data frame with columns for "lake" and "level" as well as additional
#' information about the elevation at which lake becomes a lake and suitable for
#' paddleboating and the elevation at which the lake floods back floating leaf
#' plants, evaluate the frequency of lake conditions, good paddleboating
#' conditions during warm months (April thorugh September), and flooded
#' conditions at all lakes with lake information.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param lake_info a data frame with the "min_elev_m" and "max_elev_m" at which
#'                  each "lake" becomes a lake/good for paddleboating and floods
#'                  back vegetation.
#' @return a data frame with the number of months and percent of time with
#'         lake, good paddleboating, and flooded vegetation conditions at each
#'         lake included in "lake_info".
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @export

calculate_lake <- function(df,
                           lake_info = data.frame(lake = c("Long",
                                                           "Plainfield"),
                                                  # min_elev_m = 334.29,
                                                  min_elev_m = c(333.8985,
                                                                 333.1118),
                                                  max_elev_m = c(335.536,
                                                                 335.0016))) {
  is_lake <- NULL
  for (lake in lake_info$lake) {
    this_df           <- df %>% filter(.data$lake == !!lake)
    this_info         <- lake_info %>% filter(.data$lake == !!lake)

    num_lake           <- sum(this_df$level >= this_info$min_elev_m)
    percent_lake       <- num_lake / length(this_df$level) * 100
    num_open_lake      <- sum(this_df$level >= this_info$max_elev_m)
    percent_open_lake  <- num_open_lake / length(this_df$level) * 100

    warm_df            <- this_df %>% filter(.data$month %in% c(4:9))
    num_lake_warm      <- sum(warm_df$level >= this_info$min_elev_m)
    percent_lake_warm  <- num_lake_warm / length(warm_df$level) * 100

    this_lake     <- data.frame(lake = lake,
                                metric = "is_lake",
                                variable = c("num_epa_lake",
                                             "percent_epa_lake",
                                             "num_lake_warm",
                                             "percent_lake_warm",
                                             "num_open_lake",
                                             "percent_open_lake"),
                                value = c(num_lake,
                                          percent_lake,
                                          num_lake_warm,
                                          percent_lake_warm,
                                          num_open_lake,
                                          percent_open_lake))

    is_lake <- rbind(is_lake, this_lake)
  }

  return(is_lake)
}
