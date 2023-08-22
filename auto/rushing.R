if(Sys.getenv("NFLVERSE_REBUILD", "false") == "true"){
  seasons_to_update <- 2018:nflreadr::most_recent_season()
} else {
  seasons_to_update <- nflreadr::most_recent_season()
}

purrr::walk(
  seasons_to_update,
  purrr::possibly(function(season){
    nflversedata::nflverse_save(
      data_frame = pfr_advanced_rushing_season(season),
      file_name = glue::glue("advstats_season_rush_{season}"),
      nflverse_type = "advanced rushing season stats via PFR",
      release_tag = "pfr_advstats",
      file_types = "rds"
    )
  }, quiet = FALSE
  )
)

## NOW COMBINE ALL SEASONS FOR THE FILE nflreadr IS LOADING

combined_advstats <- purrr::map(
  2018:nflreadr::most_recent_season(),
  purrr::possibly(function(season){
    load_from <- glue::glue("https://github.com/nflverse/nflverse-data/releases/download/pfr_advstats/advstats_season_rush_{season}.rds")
    nflreadr::rds_from_url(load_from)
  }, tibble::tibble(), quiet = FALSE),
  .progress = TRUE
) |>
  purrr::list_rbind()

nflversedata::nflverse_save(
  data_frame = combined_advstats,
  file_name = "advstats_season_rush",
  nflverse_type = "advanced rushing season stats via PFR",
  release_tag = "pfr_advstats"
)
