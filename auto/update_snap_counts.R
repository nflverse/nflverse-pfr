pkgload::load_all()

scrape_snaps <- function(data_path = here::here("data/snap_counts/game")){

  #' List Completed Scrapes
  completed_games <- list.files(data_path) %>%
    stringr::str_replace_all("([[:alnum:]]+).*","\\1") %>%
    unique()

  game_ids <- nflreadr::load_schedules() %>%
    dplyr::filter(
      !pfr %in% completed_games,
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

  split(scrape_games,scrape_games$pfr_game_id) %>%
    purrr::iwalk(~{
      filename <- glue::glue("data/snap_counts/game/{.y}.")
      readr::write_csv(.x, paste0(filename, "csv"))
      saveRDS(.x,paste0(filename,"rds"))
    })

  cli::cli_alert_success("Finished scraping snap counts for {nrow(game_ids)} games")

  return(TRUE)
}

summarise_snap_counts <- function(season = nflreadr:::most_recent_season()){

  cli::cli_process_start("combining into one file for {season}")

  game_ids <- nflreadr::load_schedules(season) %>%
    dplyr::filter(!is.na(result))

  season_data <- purrr::map_dfr(game_ids$pfr,
                                purrr::possibly(
                                  ~readRDS(glue::glue("data/snap_counts/game/{.x}.rds")),
                                  otherwise = tibble::tibble()))

  filename <- glue::glue("data/snap_counts/weekly/snap_counts_{season}.")

  saveRDS(season_data, paste0(filename,"rds"))
  readr::write_csv(season_data, paste0(filename,"csv"))

  filename <- glue::glue("data/snap_counts_{season}.")

  saveRDS(season_data, paste0(filename,"rds"))
  readr::write_csv(season_data, paste0(filename,"csv"))
  cli::cli_process_done()
}

setwd(here::here())

scrape_result <- scrape_snaps()

if(scrape_result) summarise_snap_counts()
