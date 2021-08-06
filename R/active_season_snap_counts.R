source("R/_helper_functions.R")

s <- 2021

urls <- get_game_urls(s)

for_saving %>%
  write_csv(glue::glue("data/snap_counts_{s}.csv"))

for_saving %>%
  saveRDS(glue::glue("data/snap_counts_{s}.rds"))

