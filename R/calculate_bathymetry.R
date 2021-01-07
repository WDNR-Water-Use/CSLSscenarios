#' Calculate bathymetric metrics
#'
#' Given a data frame with the metrics for a single set of simulations,
#' calculate the associated volume (m3), area (m2), maximum depth (m), and mean
#' depth (m) for each exceedance_level (m). Uses CSLSdata::bathymetry and
#' CSLSdata::lake_raster, only performs calculations on lakes which have
#' information in all three data sources.
#'
#' @param df a data frame with a "lake", "metric", "variable", and "value"
#'          columns where one of the metrics is "exceedance_level".
#'
#' @return bathymetry, a data frame with "lake", "metric", "variable", and
#'         "value" where the metrics are "volume", "area", "max_depth", and
#'         "mean_depth" and the variables are all the exceedance level
#'         probabilities inputted in df.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#' @importFrom stats approxfun
#' @importFrom raster minValue
#' @import dplyr
#'
#' @export

calculate_bathymetry <- function(df) {

  # Load lake profile and lake raster information
  profile  <- CSLSdata::bathymetry
  lake_raster <- CSLSdata::lake_raster

  # Only evaluate for lakes which have all pieces of information
  lakes1 <- as.character(unique(df$lake))
  lakes2 <- as.character(unique(profile$lake))
  lakes3 <- as.character(names(lake_raster))
  lakes  <- c(lakes1, lakes2)[duplicated(c(lakes1, lakes2))]
  lakes  <- c(lakes, lakes3)[duplicated(c(lakes, lakes3))]

  bathymetry <- NULL
  for (lake in lakes) {
    this_raster  <- lake_raster[[lake]]
    lake_bottom  <- minValue(this_raster)
    this_profile <- profile %>% filter(.data$lake == !!lake)
    this_levels  <- df %>%
                    filter(.data$lake == !!lake,
                           .data$metric == "exceedance_level")
    # Convert elevations to volume and area
    f_elev_vol  <- approxfun(x = this_profile$elev_m,
                             y = this_profile$vol_m3)
    f_elev_area <- approxfun(x = this_profile$elev_m,
                              y = this_profile$area_m2)

    this_levels$volume    <- f_elev_vol(this_levels$value)
    this_levels$area      <- f_elev_area(this_levels$value)

    # Calculate maximum depth
    this_levels$max_depth <- this_levels$value - lake_bottom

    # Calculate mean depth
    this_levels$mean_depth <- this_levels$volume/this_levels$area

    # Summarize this lake
    this_lake <- this_levels %>%
                 dplyr::select(lake = .data$lake,
                        probs = .data$variable,
                        volume = .data$volume,
                        area = .data$area,
                        max_depth = .data$max_depth,
                        mean_depth = .data$mean_depth) %>%
                 melt(id.vars = c("lake", "probs")) %>%
                 dplyr::select(lake = .data$lake,
                        metric = .data$variable,
                        variable = .data$probs,
                        value = .data$value)

    bathymetry <- rbind(bathymetry, this_lake)
  }
  return(bathymetry)
}
