#' Calculate hydrologic metrics evaluated in the CSLS
#'
#' This function calculates hydrologic metrics describing the magnitude,
#' frequency, duration, rate of change, and timing of lake levels.
#'
#' @param df_month data frame to use. Must include columns for "date" (must be
#'                 POSIXct), "lake" (must be factor), and one with the lake
#'                 levels (named in "col_name" argument).
#' @param dts vector of dt values (months) to calculate metrics for, defaults to
#'            1 (monthly), 3 (seasonal), and 12 (annual).
#' @param metrics a list of which metrics to use. Defaults to all of them
#'                c("median_level", "cv_level", "exceedance_level",
#'                "exceedance_range", "median_dur", "cv_dur",
#'                "median_rise_rate", "cv_rise_rate", "median_fall_rate",
#'                "cv_fall_rate").
#' @param dur_exceeds optional data frame with columns for "lake" and exceedance
#'                    probabilities (e.g., "10", "25", "50", "75", "90) and
#'                    associated lake levels. Used to calculate durations
#'                    above/below these levels. Defaults to NULL to calculate
#'                    durations on exceedance proababilities on provided time
#'                    series.
#'
#' @return summary, a data frame with the following columns:
#' \item{lake}{name of lake, character}
#' \item{metric}{name of metric, corresponds with values in inputted "metrics"
#'               argument, character}
#' \item{variable}{identifier for different types of metric. For example, the
#'                 metric "median_rise_rate" will have variables of "1", "3",
#'                 and "12" associated with it indicating the median rate over
#'                 1 month, 3 months, and 12 months, character}
#' \item{value}{calculated value of the metric, numeric}
#' \item{series}{type of time series used (monthly, seasonal, annual)}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom lubridate month year
#' @importFrom stats quantile median sd
#' @import dplyr
#'
#' @export

calculate_metrics <- function(df_month,
                              dts = c(1, 3, 12),
                              metrics = c("median_level",
                                          "cv_level",
                                          "exceedance_level",
                                          "volume",
                                          "area",
                                          "mean_depth",
                                          "max_depth",
                                          "centrarchid_substrate",
                                          "vegetation_area",
                                          "exceedance_range",
                                          "depart_median",
                                          "median_dur",
                                          "cv_dur",
                                          "num_dur",
                                          "num_2yr",
                                          "median_rise_rate",
                                          "cv_rise_rate",
                                          "median_fall_rate",
                                          "cv_fall_rate",
                                          "fast_rise",
                                          "fast_fall",
                                          "good_spawning",
                                          "move_dock",
                                          "turtle_bay",
                                          "stratification",
                                          "is_lake",
                                          "paddlesports",
                                          "motorboat",
                                          "season_compare",
                                          "fast_rise_decade",
                                          "fast_fall_decade",
                                          "num_dur_decade",
                                          "num_2yr_decade",
                                          "move_dock_decade"),
                              dur_exceeds = NULL) {

  # 0. Setup data frames =======================================================
  # Get month and year information
  df_month$month <- month(df_month$date)
  df_month$year  <- year(df_month$date)
  nyrs           <- df_month %>%
                    count(.data$lake) %>%
                    mutate(n = .data$n/12)

  # Also get seasonal (3-mo avg) levels
  df_season <- df_month %>%
               group_by(.data$lake) %>%
               arrange(.data$date) %>%
               mutate(level = as.numeric(stats::filter(.data$level,
                                                       rep(1/3, 3),
                                                       sides = 2)),
                      depth = as.numeric(stats::filter(.data$depth,
                                                       rep(1/3, 3),
                                                       sides = 2))) %>%
               ungroup() %>%
               filter(.data$month %in% c(1,4,7,10))

  # Also get annual levels
  df_annual  <- df_month %>%
                group_by(lake = .data$lake,
                         date = .data$year) %>%
                summarise(level = mean(.data$level, na.rm = TRUE),
                          depth = mean(.data$depth, na.rm = TRUE),
                          .groups = "drop")

  # Get comparison exceedance levels, if using
  if (is.null(dur_exceeds)) {
    dur_exceeds_month  <- NULL
    dur_exceeds_season <- NULL
    dur_exceeds_annual <- NULL
  } else {
    dur_exceeds_month  <- dur_exceeds %>%
                          filter(.data$series == "month") %>%
                          select(-c("series"))
    dur_exceeds_season <- dur_exceeds %>%
                          filter(.data$series == "season") %>%
                          select(-c("series"))
    dur_exceeds_annual <- dur_exceeds %>%
                         filter(.data$series == "annual") %>%
                         select(-c("series"))
  }

  # Initialize summary list
  summary <- list()
  output  <- list()
  i <- 1
  j <- 1

  for (dt in dts) {
    if (dt == 1) {
      df          <- df_month
      dur_exceeds <- dur_exceeds_month
      series      <- "month"
    } else if (dt == 3) {
      df          <- df_season
      dur_exceeds <- dur_exceeds_season
      series <- "season"
    } else if (dt == 12) {
      df          <- df_annual
      dur_exceeds <- dur_exceeds_annual
      series      <- "annual"
    }

    # 1. MAGNITUDE =============================================================
    # 1a. Monthly and overall median levels and cv of levels -------------------
    if ("median_level" %in% metrics | "cv_level" %in% metrics) {
      if (dt %in% c(1, 3)) {
        # monthly median level and cv of levels
        vals <- df %>%
                group_by(.data$lake, .data$month) %>%
                summarise(median_level = median(.data$level, na.rm = TRUE),
                          cv_level = 100*sd(.data$depth, na.rm = TRUE)/
                                     mean(.data$depth, na.rm = TRUE),
                          .groups = "drop") %>%
                mutate(month = as.character(.data$month)) %>%
                melt(id.vars = c("lake", "month")) %>%
                select(lake = .data$lake,
                       metric = .data$variable,
                       variable = .data$month,
                       value = .data$value) %>%
                filter(.data$metric %in% metrics)
        summary[[i]] <- vals; i <- i + 1
      }

      # overall median level and cv of levels (month 0)
      vals <- df %>%
              group_by(.data$lake) %>%
              summarise(median_level = median(.data$level, na.rm = TRUE),
                        cv_level = 100*sd(.data$depth, na.rm = TRUE)/
                                   mean(.data$depth, na.rm = TRUE),
                        .groups = "drop") %>%
              mutate(month = "0") %>%
              melt(id.vars = c("lake", "month")) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$month,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 1b. Exceedance probability levels ----------------------------------------
    if ("exceedance_level" %in% metrics) {
      exceeds <- calculate_exceedances(df, probs = c(10, 25, 50, 75, 90))
      vals    <- exceeds %>%
                 mutate(metric = "exceedance_level",
                        variable = as.character(.data$variable)) %>%
                 select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1

      # 1b1. Bathymetric metrics -----------------------------------------------
      if ("volume" %in% metrics |
          "area" %in% metrics |
          "mean_depth" %in% metrics |
          "max_depth" %in% metrics) {
        vals2 <- calculate_bathymetry(vals) %>%
                 filter(.data$metric %in% metrics)
        summary[[i]] <- vals2; i <- i + 1
      }

      # 1b2. Substrate area ----------------------------------------------------
      if ("centrarchid_substrate" %in% metrics ) {
        vals2 <- calculate_substrate(vals)
        summary[[i]] <- vals2; i <- i + 1
      }

      # 1b3. Vegetation area ---------------------------------------------------
      if ("vegetation_area" %in% metrics ) {
        vals2 <- calculate_plant_area(vals)
        summary[[i]] <- vals2; i <- i + 1
      }
    }

    # 1c. Exceedance probability ranges ----------------------------------------
    if ("exceedance_range" %in% metrics) {
      # by all monthly values
      exceeds <- calculate_exceedances(df,
                                       probs = c(10, 25, 75, 90),
                                       melted = FALSE)
      ranges  <- exceeds %>%
                 group_by(.data$lake) %>%
                 mutate(range_10_90 = .data$`10` - .data$`90`,
                        range_25_75 = .data$`25` - .data$`75`) %>%
                 ungroup() %>%
                 select(.data$lake, .data$range_10_90, .data$range_25_75)
      vals    <- ranges %>%
                 melt(id.vars = "lake") %>%
                 mutate(metric = "exceedance_range",
                        variable = as.character(.data$variable)) %>%
                 select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2. FREQUENCY =============================================================
    durations <- calculate_durations(df,
                                     probs = c(10, 25, 50, 75, 90),
                                     departures = FALSE,
                                     exceeds = dur_exceeds) %>%
                 mutate(value = .data$value*dt)

    # 2a. Departure from median ------------------------------------------------
    if ("depart_median" %in% metrics) {
      probs <- calculate_exceedances(df, departures = c(NISTftTOmeter(1),
                                                          NISTftTOmeter(-1)))
      vals  <- probs %>%
               mutate(metric = "depart_median",
                      variable = as.character(.data$variable)) %>%
               select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2b. Frequency of high/low levels -----------------------------------------
    if ("num_dur" %in% metrics | "num_dur_decade" %in% metrics) {
      vals <- durations %>%
              filter(.data$value != 0) %>%
              count(.data$lake, .data$variable,
                    .drop = FALSE) %>%
              select(lake = .data$lake,
                     probs = .data$variable,
                     num_dur = .data$n) %>%
              left_join(nyrs, by = "lake") %>%
              mutate(num_dur_decade = .data$num_dur/(.data$n/10),
                     probs = as.character(.data$probs)) %>%
              select(.data$lake, .data$num_dur, .data$num_dur_decade,
                     .data$probs) %>%
              melt(id.vars = c("lake", "probs")) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$probs,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    if ("num_2yr" %in% metrics | "num_2yr_decade" %in% metrics) {
      vals <- durations %>%
              filter(.data$value >= 24) %>%
              count(.data$lake, .data$variable,
                    .drop = FALSE) %>%
              select(lake = .data$lake,
                     probs = .data$variable,
                     num_2yr = .data$n) %>%
              left_join(nyrs, by = "lake") %>%
              mutate(num_2yr_decade = .data$num_2yr/(.data$n/10),
                     probs = as.character(.data$probs)) %>%
              select(.data$lake, .data$num_2yr, .data$num_2yr_decade,
                     .data$probs) %>%
              melt(id.vars = c("lake", "probs")) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$probs,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2c. Frequency of moving docks --------------------------------------------
    if ("move_dock" %in% metrics & dt %in% c(1)) {
      vals  <- calculate_docks(df) %>%
               select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2d. Frequency of connection to Turtle Bay --------------------------------
    if ("turtle_bay" %in% metrics & dt %in% c(1, 3)) {
      vals  <- calculate_turtle(df)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2e. Frequency of mixed conditions (at Pleasant Lake) ---------------------
    if ("stratification" %in% metrics) {
      vals    <- calculate_stratification(df)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2f. Frequency of lake/good paddleboat conditions (at Long Lake) ---------------
    if ("is_lake" %in% metrics & dt %in% c(1, 3)) {
      vals  <- calculate_lake(df)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2g. Frequency of lake/good paddleboat conditions (at Long Lake) ---------------
    if ("paddlesports" %in% metrics & dt %in% c(1, 3)) {
      vals  <- calculate_paddlesports(df)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2h. Frequency of legal motorboating conditions (at Pleasant Lake) --------
    if ("motorboat" %in% metrics & dt %in% c(1, 3)) {
      vals  <- calculate_motorboat(df)
      summary[[i]] <- vals; i <- i + 1
    }

    # 3. DURATION ==============================================================
    # 3a. Median duration and CV of duration above/below levels ----------------
    if ("median_dur" %in% metrics | "cv_dur" %in% metrics) {
      vals <- durations %>%
              group_by(lake = .data$lake,
                       probs = .data$variable) %>%
              summarise(median_dur = median(.data$value, na.rm = TRUE),
                        cv_dur = 100*sd(.data$value, na.rm = TRUE)/
                                 mean(.data$value, na.rm = TRUE),
                        .groups = "drop") %>%
              melt(id.vars = c("lake", "probs")) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$probs,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 4. RATE OF CHANGE ========================================================
    periods <- c(1, 3, 12)/dt
    periods <- periods[periods >= 1]
    rates   <- calculate_rates(df, months = periods) %>%
               mutate(time = as.numeric(as.character(.data$variable))) %>%
               mutate(variable = as.character(.data$time*dt)) %>%
               select(.data$lake, .data$variable, .data$value)
    rising  <- rates %>% filter(.data$value > 0)
    falling <- rates %>% filter(.data$value < 0)

    # 4a. Median and CV of rise rates ------------------------------------------
    if ("median_rise_rate" %in% metrics | "cv_rise_rate" %in% metrics) {
      vals <- rising %>%
              group_by(lake = .data$lake,
                       time = .data$variable) %>%
              summarise(median_rise_rate = median(.data$value, na.rm = TRUE),
                        cv_rise_rate = 100*sd(.data$value, na.rm = TRUE)/
                                       mean(.data$value, na.rm = TRUE),
                        .groups = "drop") %>%
              melt(id.vars = c("lake", "time")) %>%
              mutate(time = as.character(.data$time)) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$time,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 4b. Median and CV of fall rates ------------------------------------------
    if ("median_fall_rate" %in% metrics | "cv_fall_rate" %in% metrics) {
      vals <- falling %>%
              group_by(lake = .data$lake,
                       time = .data$variable) %>%
              summarise(median_fall_rate = median(.data$value, na.rm = TRUE),
                        cv_fall_rate = 100*sd(.data$value, na.rm = TRUE)/
                                       mean(.data$value, na.rm = TRUE),
                        .groups = "drop") %>%
              melt(id.vars = c("lake", "time")) %>%
              mutate(time = as.character(.data$time)) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$time,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 4c. Fast rise rates ------------------------------------------------------
    fast <- NISTunits::NISTftTOmeter(1.5)
    really_fast <- NISTunits::NISTftTOmeter(3)
    if ("fast_rise" %in% metrics | "fast_rise_decade" %in% metrics) {
      vals <- rising %>%
              mutate(rate_3ft = ifelse(.data$value >= really_fast, TRUE, FALSE),
                     rate_1_5ft = ifelse(.data$value >= fast, TRUE, FALSE)) %>%
              count(.data$lake, .data$variable, .data$rate_1_5ft, .data$rate_3ft,
                    .drop = FALSE) %>%
              group_by(.data$lake, .data$variable) %>%
              summarise(rate_3ft = sum(.data$n[.data$rate_3ft == TRUE]),
                        rate_1_5ft = sum(.data$n[.data$rate_1_5ft == TRUE]),
                        .groups = "drop") %>%
              select(lake = .data$lake,
                     time = .data$variable,
                     rise_3ft = .data$rate_3ft,
                     rise_1_5ft = .data$rate_1_5ft) %>%
              melt(id.vars = c("lake", "time")) %>%
              mutate(metric = as.character(.data$variable),
                     variable = .data$time) %>%
              select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1

      if ("fast_rise_decade" %in% metrics) {
        vals <- vals %>%
                left_join(nyrs, by = "lake") %>%
                mutate(value = .data$value/(.data$n/10),
                       metric = sprintf("%s_decade", .data$metric)) %>%
                select(.data$lake, .data$metric, .data$variable, .data$value)
        summary[[i]] <- vals; i <- i + 1
      }
    }

    # 4d. Fast fall rates ------------------------------------------------------
    fast <- NISTunits::NISTftTOmeter(-1.5)
    really_fast <- NISTunits::NISTftTOmeter(-3)
    if ("fast_fall" %in% metrics | "fast_fall_decade" %in% metrics) {
      vals <- falling %>%
              mutate(rate_3ft = ifelse(.data$value <= really_fast, TRUE, FALSE),
                     rate_1_5ft = ifelse(.data$value <= fast, TRUE, FALSE)) %>%
              count(.data$lake, .data$variable, .data$rate_1_5ft, .data$rate_3ft,
                    .drop = FALSE) %>%
              group_by(.data$lake, .data$variable) %>%
              summarise(rate_3ft = sum(.data$n[.data$rate_3ft == TRUE]),
                        rate_1_5ft = sum(.data$n[.data$rate_1_5ft == TRUE]),
                        .groups = "drop") %>%
              select(lake = .data$lake,
                     time = .data$variable,
                     fall_3ft = .data$rate_3ft,
                     fall_1_5ft = .data$rate_1_5ft) %>%
              melt(id.vars = c("lake", "time")) %>%
              mutate(metric = as.character(.data$variable),
                     variable = .data$time) %>%
              select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1

      if ("fast_rise_decade" %in% metrics) {
        vals <- vals %>%
                left_join(nyrs, by = "lake") %>%
                mutate(value = .data$value/(.data$n/10),
                       metric = sprintf("%s_decade", .data$metric)) %>%
                select(.data$lake, .data$metric, .data$variable, .data$value)
        summary[[i]] <- vals; i <- i + 1
      }
    }

    # 5. TIMING ================================================================
    if ("season_compare" %in% metrics & dt %in% c(1,3)) {
        vals    <- df_season %>%
                   arrange(.data$date) %>%
                   group_by(.data$lake, .data$month) %>%
                   mutate(diff = .data$level - lag(.data$level),
                          higher = ifelse(.data$diff > 0, TRUE, FALSE)) %>%
                   ungroup() %>%
                   count(.data$lake, .data$month, .data$higher) %>%
                   filter(!is.na(.data$higher)) %>%
                   group_by(.data$lake, .data$month) %>%
                   mutate(nyrs = sum(.data$n)) %>%
                   ungroup() %>%
                   mutate(pcnt = 100*.data$n/.data$nyrs,
                          metric = ifelse(.data$higher,
                                          "season_higher", "season_lower"),
                          variable = as.character(.data$month)) %>%
                   select(lake = .data$lake,
                          metric = .data$metric,
                          variable = .data$variable,
                          value = .data$pcnt)
        summary[[i]] <- vals; i <- i + 1
    }

    if ("good_spawning" %in% metrics & dt %in% c(1, 3)) {
      if (min(nyrs$n) >= 2) {
        spawning <- calculate_spawning(df)
        vals     <- spawning %>%
                    group_by(.data$lake) %>%
                    summarise(high_spring = sum(.data$high_spring,
                                                na.rm = TRUE),
                              steady_summer = sum(.data$steady_summer,
                                                  na.rm = TRUE),
                              good_spawning = sum(.data$good_spawning,
                                                  na.rm = TRUE),
                              .groups ="drop") %>%
                    left_join(nyrs, by = "lake") %>%
                    mutate(high_spring = 100*.data$high_spring/
                                         (.data$n-1),
                           steady_summer = 100*.data$steady_summer/
                                           .data$n,
                           good_spawning = 100*.data$good_spawning/
                                           (.data$n-1)) %>%
                    select(.data$lake, .data$high_spring, .data$steady_summer,
                           .data$good_spawning) %>%
                    melt(id.vars = "lake") %>%
                    mutate(metric = "good_spawning",
                           variable = as.character(.data$variable)) %>%
                    select(.data$lake, .data$metric, .data$variable,
                           .data$value)
      } else {
        lakes    <- unique(df$lake)
        variable <- c("high_spring", "steady_summer", "good_spawning")
        vals     <- expand.grid(lakes, variable)
        colnames(vals) <- c("lake", "variable")
        vals     <- vals %>%
                    mutate(metric = "good_spawning",
                           value = 0,
                           variable = as.character(.data$variable)) %>%
                    select(.data$lake, .data$metric, .data$variable,
                           .data$value)
      }
      summary[[i]] <- vals; i <- i + 1
    }

    # Combine metrics for this type of time series
    summary        <- bind_rows(summary)
    summary$series <- series
    output[[j]]    <- summary; j <- j + 1
    summary        <- list()
    i              <- 1
  }
  output <- bind_rows(output)
  return(output)
}
