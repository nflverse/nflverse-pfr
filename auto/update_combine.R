pkgload::load_all()
all_combines <- purrr::map_dfr(2000:nflreadr:::most_recent_season(roster = TRUE),
                               purrr::possibly(pfr_combine,otherwise = tibble::tibble()))

attr(all_combines, "nflverse_timestamp") <- Sys.time()
attr(all_combines, "nflverse_type") <- "combine measurements"

saveRDS(all_combines, "data/combine.rds")
arrow::write_parquet(all_combines, "data/combine.parquet")
readr::write_csv(all_combines, "data/combine.csv")

list.files("data",pattern = "combine", full.names = TRUE) |> nflversedata::nflverse_upload(tag = "combine")
