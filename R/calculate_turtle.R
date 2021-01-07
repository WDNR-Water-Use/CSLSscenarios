#' Calculate frequency of connection to Turtle Bay
#'
#' Given a data frame with columns for "lake", "level", "month", and "year" as
#' well as additional information about the elevation of Turtle Bay
#' ("turtle_elev_m") and minimum required clearance for good connection
#' ("turtle_clearance_ft"), calculates frequency of connection at all times and
#' during the growing season (March through August).
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param turtle_elev_m elevation of turtle bay inlet by eye examination of
#'                      bathymetry raster (m).
#' @param turtle_clearance_ft minimum clearnace needed for good connection to
#'                            turtle bay.
#' @return a data frame with the number of months and percent of time with
#'         connection to Turtle Bay during all months and during warm season
#'         (March through August) months.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom NISTunits NISTftTOmeter
#' @importFrom stats approxfun
#' @importFrom dplyr filter
#'
#' @export

calculate_turtle <- function(df,
                             turtle_elev_m = 298.7,
                             turtle_clearance_ft = 1) {
  # Turtle Bay inlet elev from eye examination of bathy raster
  # Plus 1 ft clearance
  tb_threshold_m <- turtle_elev_m + NISTftTOmeter(turtle_clearance_ft)

  tb_df <- df %>% filter(.data$lake == "Pleasant")

  nConnect   <- sum(tb_df$level >= tb_threshold_m)
  perConnect <- nConnect / length(tb_df$level) * 100

  tb_df_warm <- tb_df %>% filter(.data$month %in% 3:8)

  nWarmConnect   <- sum(tb_df_warm$level >= tb_threshold_m)
  perWarmConnect <- nWarmConnect / length(tb_df_warm$level) * 100

  return(data.frame(lake = "Pleasant",
                    metric = "turtle_bay",
                    variable = c("num_connect",
                                 "percent_connect",
                                 "num_connect_warm",
                                 "percent_connect_warm"),
                    value = c(nConnect,
                              perConnect,
                              nWarmConnect,
                              perWarmConnect)))
}
