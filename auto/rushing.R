df_advstats <- purrr::map(
  2018:nflreadr::most_recent_season(),
  purrr::possibly(pfr_advanced_rushing_season, tibble::tibble(), quiet = FALSE)
) |>
  purrr::list_rbind()

nflversedata::nflverse_save(
  data_frame = df_advstats,
  file_name = "advstats_season_rush",
  nflverse_type = "advanced rushing season stats via PFR",
  release_tag = "pfr_advstats"
)
