pkgload::load_all()

scrape_snaps <- function(data_path = here::here("data/snap_counts/game")){

  completed_games <- piggyback::pb_download_url(file = "scraped_games.csv",
                                                repo = "nflverse/pfr_scrapR",
                                                tag = "snap_counts_raw") |>
    read.csv()

  game_ids <- nflreadr::load_schedules() %>%
    dplyr::filter(
      !pfr %in% completed_games$pfr_game_id,
      !is.na(result),
      season > 2012
    ) %>%
    dplyr::select(
      game_id,
      pfr_game_id = pfr,
      season,
      game_type,
      week,
      home_team,
      away_team
      )

  if(nrow(game_ids)==0) {
    cli::cli_alert_danger("No new games to scrape!")

    return(FALSE)
  }

  cli::cli_alert("Now scraping {nrow(game_ids)} games")

  scrape_games <- game_ids %>%
    dplyr::mutate(snaps = purrr::map(pfr_game_id,
                                   purrr::possibly(
                                     pfr_game_snap_counts,
                                     otherwise = tibble::tibble())
    )) %>%
    tidyr::unnest(snaps)

  if(nrow(scrape_games)==0) {
    cli::cli_alert_danger("No new data for scrapes!")
    return(FALSE)
  }

  scrape_games <- scrape_games %>%
    dplyr::mutate(team = ifelse(location == "home", home_team, away_team),
                  opponent = ifelse(location == "home", away_team, home_team),
                  home_team = NULL,
                  away_team = NULL,
                  location = NULL,
                  .after = position)

  old_snaps <- nflreadr::rds_from_url(
    "https://github.com/nflverse/pfr_scrapR/releases/download/snap_counts_raw/snap_counts.rds") |>
    dplyr::inner_join(completed_games |> dplyr::select(pfr_game_id), by = "pfr_game_id")

  new_snaps <- dplyr::bind_rows(old_snaps, new_snaps) |>
    dplyr::arrange(game_id)

  attr(new_snaps, "timestamp") <- Sys.time()

  saveRDS(new_snaps, here::here("build/snap_counts.rds"))

  piggyback::pb_upload(file = here::here("build/snap_counts.rds"),repo = "nflverse/pfr_scrapR",tag = "snap_counts_raw")

  new_snaps |>
    distinct(season,week, pfr_game_id,game_id) |>
    write.csv(here::here("build/scraped_games.csv"),quote = TRUE, row.names = FALSE)

  piggyback::pb_upload(file = here::here("build/snap_counts.rds"),repo = "nflverse/pfr_scrapR",tag = "snap_counts_raw")

  cli::cli_alert_success("Finished scraping snap counts for {nrow(game_ids)} games")

  return(TRUE)
}

summarise_snap_counts <- function(season = nflreadr:::most_recent_season()){

  cli::cli_process_start("Uploading snap counts for {season} to nflverse-data")

  season_data <- nflreadr::rds_from_url(
    "https://github.com/nflverse/pfr_scrapR/releases/download/snap_counts_raw/snap_counts.rds"
  ) |>
    dplyr::filter(season == season)

  filename <- glue::glue("build/snap_counts_{season}.")

  saveRDS(season_data, paste0(filename,"rds"))
  readr::write_csv(season_data, paste0(filename,"csv"))
  arrow::write_parquet(season_data, paste0(filename,"parquet"))

  list.files("build", pattern = "snap_counts_*", full.names = TRUE) |>
    nflversedata::nflverse_upload("snap_counts")

  cli::cli_process_done()
}

setwd(here::here())

scrape_result <- scrape_snaps()

if(scrape_result) summarise_snap_counts()
