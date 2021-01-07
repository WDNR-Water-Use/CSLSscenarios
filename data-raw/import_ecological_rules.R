# Import ecological rules

ecological_rules <- read.csv("data-raw/ecological_rules.csv")

usethis::use_data(ecological_rules, overwrite = TRUE, compress = "xz")
