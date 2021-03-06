#' Calculates number of summers with good/bad conditions for docks
#'
#' Given a data frame with columns for "lake", "level", "month", and "year" as
#' well as a data frame with additional information about the average dock
#' length ("length_ft") and minimum desired depth at the end of the dock
#' ("min_depth_ft") for each "lake", calculates the number of years in a time
#' series where summer levels (July-Sept) dropped below the desired minimum
#' depth, assuming docks were installed in June with the minimum desired depth
#' at the end of the dock.
#'
#' Assumes the average horizontal lake profile for lakes is available within
#' CSLSdata::bathymetry.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param info a data frame with the average dock length (length_ft) for lakes
#'             and minimum desired depth (min_depth_ft) at the end of the dock.
#' @return docks, a data frame with the following columns:
#' \item{lake}{the lake name, e.g. "Pleasant" or "Long"}
#' \item{metric}{name of the hydrologic metric, in this case, "dock"}
#' \item{variable}{"num_no_move" (number of years dock does not need to be moved
#'                 after a successful install), "percent_no_move" (percent of
#'                 years dock does not need to be moved after a successful
#'                 install), "num_install" (number of years a dock is
#'                 successfully installed), "percent_install" (percent of years
#'                 a dock is successfully installed), "percent_good_year"
#'                 (percent of years a dock is both successfully installed and
#'                 does not need to be moved)}
#' \item{value}{value of the metric}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom NISTunits NISTftTOmeter
#' @importFrom stats approxfun
#' @import dplyr
#'
#' @export

calculate_docks <- function(df,
                            info = data.frame(lake = c("Pleasant", "Long"),
                                              length_ft = c(39, 23),
                                              min_depth_ft = c(2, 1))) {
  info$length_m    <- NISTftTOmeter(info$length_ft)
  info$min_depth_m <- NISTftTOmeter(info$min_depth_ft)
  profile          <- CSLSdata::bathymetry

  dock <- NULL
  for (lake in info$lake) {
    this_dock_m   <- info$length_m[info$lake == lake]
    this_depth_m  <- info$min_depth_m[info$lake == lake]
    this_profile  <- profile %>% filter(.data$lake == !!lake)
    this_levels   <- df %>%
                     filter(.data$lake == !!lake,
                            .data$month %in% c(6, 7, 8, 9)) %>%
                     select(date = .data$date,
                            month = .data$month,
                            year = .data$year,
                            elev_m = .data$level)

    # Convert elevations to distance and vice versa
    f_elev_dist  <- approxfun(x = this_profile$elev_m,
                              y = this_profile$horiz_dist_m)
    f_dist_elev  <- approxfun(x = this_profile$horiz_dist_m,
                              y = this_profile$elev_m)
    this_levels$horiz_dist_m <- f_elev_dist(this_levels$elev_m)

    # June levels
    june_docks <- this_levels %>%
                  filter(.data$month == 6) %>%
                  mutate(dock_end = .data$horiz_dist_m + this_dock_m,
                         dock_end_elev = f_dist_elev(.data$dock_end),
                         min_elev = .data$dock_end_elev+this_depth_m) %>%
                  filter(.data$elev_m >= .data$min_elev) %>%
                  select(.data$year, .data$min_elev)

    # Docks get shallow
    bad_drops   <- left_join(this_levels,
                             june_docks,
                             by = "year") %>%
                   mutate(bad_drop = ifelse(.data$elev_m <= .data$min_elev,
                                            TRUE, FALSE)) %>%
                   count(.data$year, .data$bad_drop) %>%
                   filter(.data$bad_drop == TRUE)

    # Summarize values
    num_years         <- length(unique(this_levels$year))
    num_install       <- nrow(june_docks)
    percent_install   <- 100*num_install/num_years
    num_no_move       <- num_install - nrow(bad_drops)
    percent_no_move   <- 100*num_no_move/num_install
    percent_good_year <- 100*num_no_move/num_years

    this_dock <- data.frame(lake = rep(lake, 5),
                            metric = rep("dock", 5),
                            variable = c("num_no_move",
                                         "percent_no_move",
                                         "num_install",
                                         "percent_install",
                                         "percent_good_year"),
                            value = c(num_no_move,
                                      percent_no_move,
                                      num_install,
                                      percent_install,
                                      percent_good_year))
    dock <- rbind(dock, this_dock)
  }

  return(dock)
}
