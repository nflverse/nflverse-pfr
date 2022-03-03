pkgload::load_all()
all_combines <- purrr::map_dfr(2000:nflreadr:::most_recent_season(roster = TRUE),
                               purrr::possibly(pfr_combine,otherwise = tibble::tibble()))
saveRDS(all_combines, "data/combine.rds")
readr::write_csv(all_combines, "data/combine.csv")
