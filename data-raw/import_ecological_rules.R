# Import ecological rules

# Guidelines for allowable change in hydrologic or bathymetry-related metrics.
# Relied upon in "compare_scenarios.R"

ecological_rules <- read.csv("data-raw/ecological_rules.csv")

usethis::use_data(ecological_rules, overwrite = TRUE, compress = "xz")
write.csv(ecological_rules,
          file = "inst/csv/ecological_rules.csv",
          row.names = FALSE)
