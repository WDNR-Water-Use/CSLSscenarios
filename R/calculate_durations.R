#' Calculate durations
#'
#' Given a data frame with a "lake", "date", and "level" columns as well as a
#' vector of desired exceedance probabilities, calculates consecutive months
#' at/above (for probabilities <= 50%) or at/below (for probabilities > 50%) the
#' exceedance levels.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param probs a vector with all exceedance probabilities to calculate.
#'              Defaults to c(10, 25, 75, 90).
#' @param departures logical defaults to FALSE. If TRUE, calculates durations
#'                   1ft above median and 1ft below median
#' @param exceeds defaults to NULL. If provided, should be a data frame with
#'                first column "lake" and subsequent columns corresponding to
#'                exceedance probabilities (e.g., "10", "25", "50", "75", "90")
#'                and associated lake levels.
#' @return durations, a data frame with the following columns:
#' \item{lake}{name of lake}
#' \item{variable}{exceedance probability, e.g., "10", "25", "75", or "90"}
#' \item{value}{one count of number of months levels were consecutively above or
#'              below the given exceedance probability}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr arrange filter
#' @importFrom NISTunits NISTftTOmeter
#'
#' @export

calculate_durations <- function(df,
                                probs = c(10, 25, 50, 75, 90),
                                departures = FALSE,
                                exceeds = NULL) {
  # Compare levels to exceedance probability levels or provided exceedance
  # levels
  if (is.null(exceeds)) {
    exceeds   <- calculate_exceedances(df, probs, melted = FALSE)
  } else {
    probs    <- as.numeric(colnames(exceeds)[2:length(colnames(exceeds))])
  }
  df        <- merge(df, exceeds)
  df        <- arrange(df, .data$date)

  # Flag for duration and calculate lengths
  durations   <- NULL
  prob_levels <- NULL
  for (lake in unique(df$lake)) {
    this_lake <- df %>% filter(.data$lake == !!lake)
    for (prob in probs) {
      prob_name <- sprintf("%s", prob)
      if (prob <= 50) {
        flags <- ifelse(this_lake$level >= this_lake[,prob_name], 1, 0)
      } else {
        flags <- ifelse(this_lake$level <= this_lake[,prob_name], 1, 0)
      }

      # If 50%, note time above ("a") and below ("b")
      value1 <- unname(rle(flags)$lengths[which(rle(flags)$values == 1)])
      value0 <- unname(rle(flags)$lengths[which(rle(flags)$values == 0)])
      if (length(value1) == 0) { value1 <- 0 }
      if (length(value0) == 0) { value0 <- 0 }

      if (prob == 50) {
        prob_levels <- c(prob_levels,
                         sprintf("a%s", prob_name),
                         sprintf("b%s", prob_name))
        duration1  <- data.frame(lake = lake,
                                 variable = sprintf("a%s", prob_name),
                                 value = value1)
        duration2  <- data.frame(lake = lake,
                                 variable = sprintf("b%s", prob_name),
                                 value = value0)
        duration   <- rbind(duration1, duration2)
      } else {
        prob_levels <- c(prob_levels, prob_name)
        duration  <- data.frame(lake = lake,
                                variable = prob_name,
                                value = value1)
      }
      durations <- rbind(durations, duration)
    }

    # Calculate 1ft above/below median, if desired
    if (departures) {
      prob_levels <- c(prob_levels, "a50_1", "b50_1")
      # 1 ft above median
      flags     <- ifelse(this_lake$level >=
                            this_lake[,"50"] + NISTftTOmeter(1),
                          1, 0)
      value1    <- unname(rle(flags)$lengths[which(rle(flags)$values == 1)])
      if (length(value1) == 0) { value1 <- 0 }
      duration  <- data.frame(lake = lake,
                              variable = "a50_1",
                              value = value1)
      durations <- rbind(durations, duration)

      # 1 ft below median
      flags     <- ifelse(this_lake$level <=
                            this_lake[,"50"] - NISTftTOmeter(1),
                          1, 0)
      value1    <- unname(rle(flags)$lengths[which(rle(flags)$values == 1)])
      if (length(value1) == 0) { value1 <- 0 }
      duration  <- data.frame(lake = lake,
                              variable = "b50_1",
                              value = value1)
      durations <- rbind(durations, duration)
    }
  }

  durations$lake     <- factor(durations$lake, levels = levels(df$lake))
  durations$variable <- factor(durations$variable, levels = unique(prob_levels))

  return(durations)
}
