# Import Well Rank and Distance

# Loads in csv files with the distance of each well to Long Lake or Pleasant
# Lake. Retains only rank number (aka simulation number) and distance (in meters
# and in miles) for use in scenario analysis.

library(dplyr)
library(NISTunits)

# 1. LOAD DATA -----------------------------------------------------------------
pfl  <- read.csv("data-raw/pfl_dist_ranks.csv") %>%
        mutate(sim = .data$dist_rank + 1,
               dist_m = .data$dist_to_lake,
               lake = "Plainfield") %>%
        select(.data$lake, .data$sim, .data$dist_m)
long <- pfl %>%
        mutate(lake = "Long")
psnt <- read.csv("data-raw/psnt_dist_ranks.csv")%>%
        mutate(sim = .data$dist_rank + 1,
               dist_m = .data$dist_to_lake,
               lake = "Pleasant") %>%
        select(.data$lake, .data$sim, .data$dist_m)

# 2. COMBINE -------------------------------------------------------------------
well_rank_dist <- rbind(pfl, long) %>%
                  rbind(psnt) %>%
                  mutate(dist_mi = NISTmeterTOmile(.data$dist_m))

# 2. SAVE ----------------------------------------------------------------------
usethis::use_data(well_rank_dist, overwrite = TRUE, compress = "xz")
write.csv(well_rank_dist,
          file = "inst/csv/well_rank_dist.csv",
          na = "",
          row.names = FALSE)
