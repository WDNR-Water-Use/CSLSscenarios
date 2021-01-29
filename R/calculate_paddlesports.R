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

calculate_paddlesports <- function(df,
                                   lake_info = data.frame(lake = "Long",
                                                          elev_m = 334.29)) {
  paddlesports <- NULL
  for (lake in lake_info$lake) {
    this_df        <- df %>%
                      filter(.data$lake == !!lake,
                             .data$month %in% c(4:9))

    this_info      <- lake_info %>% filter(.data$lake == !!lake)

    num_paddle     <- sum(this_df$level >= this_info$elev_m)
    percent_paddle <- num_paddle / length(this_df$level) * 100

    this_lake      <- data.frame(lake = lake,
                                metric = "paddlesports",
                                variable = c("num_paddle",
                                             "percent_paddle"),
                                value = c(num_paddle,
                                          percent_paddle))

    paddlesports <- rbind(paddlesports, this_lake)
  }

  return(paddlesports)
}
