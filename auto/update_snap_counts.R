pkgload::load_all()

scrape_snaps <- function(){

  completed_games <- nflreadr::csv_from_url(
    "https://github.com/nflverse/nflverse-pfr/releases/download/snap_counts_raw/scraped_games.csv"
  )

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

  cli::cli_alert("Now scraping {nrow(game_ids)} game{?s}")

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
    "https://github.com/nflverse/nflverse-pfr/releases/download/snap_counts_raw/snap_counts.rds"
  ) |>
    dplyr::filter(pfr_game_id %in% completed_games$pfr_game_id)

  new_snaps <- dplyr::bind_rows(old_snaps, scrape_games) |>
    dplyr::distinct() |>
    dplyr::arrange(game_id)

  attr(new_snaps, "timestamp") <- Sys.time()

  saveRDS(new_snaps, here::here("build/snap_counts.rds"))

  piggyback::pb_upload(file = here::here("build/snap_counts.rds"), repo = "nflverse/nflverse-pfr", tag = "snap_counts_raw")

  new_snaps |>
    dplyr::distinct(season, week, pfr_game_id, game_id) |>
    write.csv(here::here("build/scraped_games.csv"), quote = TRUE, row.names = FALSE)

  piggyback::pb_upload(file = here::here("build/scraped_games.csv"),repo = "nflverse/nflverse-pfr",tag = "snap_counts_raw")

  cli::cli_alert_success("Finished scraping snap counts for {nrow(game_ids)} game{?s}")

  return(TRUE)
}

summarise_snap_counts <- function(summary_season = nflreadr::most_recent_season()){

  cli::cli_process_start("Uploading snap counts for {summary_season} to nflverse-data")

  season_data <- readRDS(here::here("build/snap_counts.rds")) |>
    dplyr::filter(season == summary_season)

  nflversedata::nflverse_save(
    data_frame = season_data,
    file_name = glue::glue("snap_counts_{summary_season}"),
    nflverse_type = "snap counts",
    release_tag = "snap_counts"
  )

  cli::cli_process_done()
  invisible(TRUE)
}

setwd(here::here())

scrape_result <- scrape_snaps()

if(scrape_result) summarise_snap_counts()
