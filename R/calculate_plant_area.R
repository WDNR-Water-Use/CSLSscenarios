#' Calculate area of each vegetation class corresponding to lake elevations
#'
#' Given a metrics data frame with values for exceedance_level, use the
#' information in CSLSdata::bathymetry to calculate the area of each plant
#' community at each exceedance level.
#'
#' @param df a data frame with a "lake", "metric", "variable", and "value"
#'          columns where one of the metrics is "exceedance_level".
#'
#' @return plant_areas, a data frame with "lake", "metric", "variable", and
#'         "value" where the metrics are plant community types and the variables
#'         are all the exceedance level probabilities inputted in df, and the
#'         values are the corresponding plant areas in m^2.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom reshape2 melt
#' @importFrom stats approxfun
#'
#' @export

calculate_plant_area <- function(df) {

  # Load lake profile and plant area information
  bathymetry <- CSLSdata::bathymetry

  # Only evaluate for lakes which have all pieces of information
  lakes1 <- as.character(unique(df$lake))
  lakes2 <- as.character(unique(bathymetry$lake))
  lakes  <- c(lakes1, lakes2)[duplicated(c(lakes1, lakes2))]

  # Plant community names
  all_plants <- unique(CSLSdata::plant_limits[,"variable"])

  plant_areas <- NULL
  for (lake in lakes) {
    this_profile <- bathymetry %>%
                    filter(.data$lake == !!lake)
    # Only keep plant communities which exist at this lake
    check_plants <- colSums(this_profile[,all_plants], na.rm = TRUE)
    plants       <- names(check_plants[check_plants != 0])
    this_profile <- this_profile[, c("lake", "elev_m", plants)]

    # Only evaluate at exceedance probability levels
    this_levels  <- df %>%
                    filter(.data$lake == !!lake,
                           .data$metric == "exceedance_level") %>%
                    mutate(probs = .data$variable)

    #Estimate plant area at each exceedance level
    for (plant in plants) {
      # Plant area (m2)
      area_name     <- sprintf("%s_m2", plant)
      plant_area_fn <- approxfun(this_profile$elev_m,
                                  this_profile[,area_name])
      this_levels[,area_name] <- plant_area_fn(this_levels$value)

      # Percent of lake outline that contains this plant
      pcnt_name     <- sprintf("%s_pcnt", plant)
      plant_pcnt_fn <- approxfun(this_profile$elev_m,
                                 this_profile[,pcnt_name])
      this_levels[,pcnt_name] <- plant_pcnt_fn(this_levels$value)
    }

    this_areas <- this_levels[,c("lake", "probs", plants)] %>%
                  melt(id.vars = c("lake", "probs")) %>%
                  select(lake = .data$lake,
                         metric = .data$variable,
                         variable = .data$probs,
                         value = .data$value)
    plant_areas <- rbind(plant_areas, this_areas)
  }

  return(plant_areas)
}
