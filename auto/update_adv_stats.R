#' Automated Scrape for PFR Advanced Stats
pkgload::load_all()

scrape_advstats <- function(data_path = here::here("data/adv_stats/game")){

  #' List Completed Scrapes
  completed_games <- list.files(data_path) %>%
    stringr::str_replace_all("([[:alnum:]]+)_.*","\\1") %>%
    unique()

  game_ids <- nflreadr::load_schedules() %>%
    dplyr::filter(
      ! pfr %in% completed_games,
      !is.na(result),
      season >= 2018
    ) %>%
    dplyr::select(game_id = pfr)

  if(nrow(game_ids)==0) {
    cli::cli_alert_danger("No new games to scrape!")
    return(FALSE)
  }

  cli::cli_alert("Now scraping {nrow(game_ids)} games")

  #' Scrape Incomplete Games
  scrape_games <- game_ids %>%
    dplyr::mutate(adv = purrr::map(game_id,
                                   purrr::possibly(
                                     pfr_game_adv_stats,
                                     otherwise = list(pass = NULL,
                                                      rush = NULL,
                                                      rec = NULL,
                                                      def = NULL))
                                   )) %>%
    tidyr::unnest_longer(adv, indices_to = "stat_type", values_to = "stats") %>%
    dplyr::filter(purrr::map_lgl(stats, ~length(.x) > 0)) %>%
    dplyr::select(game_id, stat_type, stats)

  if(nrow(scrape_games)==0) {
    cli::cli_alert_danger("No new data for scrapes!")
    return(FALSE)
  }

  if(any(!game_ids$game_id %in% scrape_games$game_id)){
    cli::cli_alert_danger("Could not find advanced stats for {paste(game_ids$game_id[!game_ids$game_id %in% scrape_games$game_id], collapse = '\n')}")
  }

  purrr::pwalk(scrape_games,~{
    filename <- glue::glue("data/adv_stats/game/{..1}_{..2}.")
    readr::write_csv(..3, paste0(filename,"csv"))
    saveRDS(..3,paste0(filename,"rds"))
  })

  cli::cli_alert_success("Finished scraping {nrow(game_ids)}")

  return(TRUE)
}

clean_advstats <- function(season = nflreadr:::most_recent_season()){

  schedules <- nflreadr::load_schedules(season) %>%
    dplyr::select(game_id,
                  pfr_game_id = pfr,
                  season,
                  week,
                  game_type,
                  home_team,
                  away_team,
                  result) %>%
    nflreadr::clean_homeaway() %>%
    dplyr::filter(!is.na(result)) %>%
    dplyr::select(-result,-location) %>%
    dplyr::mutate(
      team = nflreadr::clean_team_abbrs(team, current_location = FALSE),
      opponent = nflreadr::clean_team_abbrs(opponent, current_location = FALSE)
    )

  local_data <- tidyr::crossing(pfr_game_id = schedules$pfr_game_id,
                                stat_type = c("pass","rush","rec","def")) %>%
    dplyr::mutate(
      filename = glue::glue("data/adv_stats/game/{pfr_game_id}_{stat_type}.rds"),
      data = suppressWarnings(purrr::map(filename, purrr::possibly(readRDS, otherwise = tibble::tibble()))),
      pfr_game_id = NULL
    )

  pass <- local_data %>%
    dplyr::filter(stat_type == "pass") %>%
    tidyr::unnest(data) %>%
    dplyr::select(
      pfr_game_id,
      pfr_player_name = player_name,
      pfr_player_id,
      team,
      # attempts,
      dplyr::contains("drop"),
      dplyr::contains("bad_throw"),
      dplyr::contains("times_")
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::contains("_pct"),
                    ~ as.character(.x) %>%
                      readr::parse_number() %>%
                      magrittr::divide_by(100) %>%
                      round(3)),
      team = nflreadr::clean_team_abbrs(team, current_location = FALSE)
    ) %>%
    dplyr::inner_join(
      x = schedules,
      by = c("pfr_game_id","team")
    )

  rush <- local_data %>%
    dplyr::filter(stat_type == "rush") %>%
    tidyr::unnest(data) %>%
    dplyr::select(
      pfr_game_id,
      pfr_player_name = player_name,
      pfr_player_id,
      team,
      carries,
      contains("contact"),
      contains("broken_tackles")
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::contains("_pct"),
                    ~ as.character(.x) %>%
                      readr::parse_number() %>%
                      magrittr::divide_by(100) %>%
                      round(3)),
      team = nflreadr::clean_team_abbrs(team, current_location = FALSE)
    ) %>%
    dplyr::inner_join(
      x = schedules,
      by = c("pfr_game_id","team")
    )

  rec <- local_data %>%
    dplyr::filter(stat_type == "rec") %>%
    tidyr::unnest(data) %>%
    dplyr::select(
      pfr_game_id,
      pfr_player_name = player_name,
      pfr_player_id,
      team,
      dplyr::contains("broken_tackles"),
      dplyr::contains("drop"),
      dplyr::contains("receiving_int"),
      dplyr::contains("receiving_rat")
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::contains("_pct"),
                    ~ as.character(.x) %>%
                      readr::parse_number() %>%
                      magrittr::divide_by(100) %>%
                      round(3)),
      team = nflreadr::clean_team_abbrs(team, current_location = FALSE)
    ) %>%
    dplyr::inner_join(
      x = schedules,
      by = c("pfr_game_id","team")
    )

  def <- local_data %>%
    dplyr::filter(stat_type == "def") %>%
    tidyr::unnest(data) %>%
    dplyr::select(
      pfr_game_id,
      pfr_player_name = player_name,
      everything(),
      -adv_stat_category,
      -stat_type,
      -filename
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::contains("_pct"),
                    ~ as.character(.x) %>%
                      readr::parse_number() %>%
                      magrittr::divide_by(100) %>%
                      round(3)),
      team = nflreadr::clean_team_abbrs(team, current_location = FALSE,keep_non_matches = TRUE)
    ) %>%
    dplyr::inner_join(
      x = schedules,
      by = c("pfr_game_id","team")
    )

  list(pass = pass,
       rush = rush,
       rec = rec,
       def = def) %>%
    purrr::iwalk(function(dataframe,stat_type){

      filename <- glue::glue("data/adv_stats/weekly/{stat_type}_{season}.")

      readr::write_csv(dataframe, paste0(filename, "csv"))
      saveRDS(dataframe, paste0(filename, "rds"))
      qs::qsave(dataframe, paste0(filename, "qs"))
    })

  cli::cli_alert_success("Finished cleaning adv stats for {season}!")
}

setwd(here::here())

scrape_result <- scrape_advstats("data/adv_stats/game")

if(scrape_result) clean_advstats()
