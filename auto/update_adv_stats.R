##' INTERNAL DEFINITIONS
##' `GAME` level data refers to box-score type data (i.e. with home/away team designations)
##' `WEEK` level data is same as game level except refers to every player as "team" and their opponent as "opp" (if necessary)
##' `SEASON` level data is (typically) aggregation of `WEEK` level data, except in PFR advstats where PFR provides some stats only at season level, so we scrape season summaries instead of aggregating week data

pkgload::load_all()

scrape_advstats <- function(){

  #' List Completed Scrapes

  completed_games <- piggyback::pb_download_url(
    file = "scraped_games.csv",
    repo = "nflverse/pfr_scrapR",
    tag = "advstats_raw") |>
    data.table::fread()

  game_ids <- nflreadr::load_schedules() %>%
    dplyr::filter(
      ! pfr %in% completed_games$pfr_game_id,
      !is.na(result),
      season >= 2018
    ) %>%
    dplyr::select(pfr_game_id = pfr)

  if(nrow(game_ids)==0) {
    cli::cli_alert_danger("No new games to scrape!")
    return(FALSE)
  }

  cli::cli_alert("Now scraping {nrow(game_ids)} games")

  #' Scrape Incomplete Games
  scrape_games <- game_ids %>%
    dplyr::mutate(adv = purrr::map(pfr_game_id,
                                   purrr::possibly(
                                     pfr_game_adv_stats,
                                     otherwise = list(pass = NULL,
                                                      rush = NULL,
                                                      rec = NULL,
                                                      def = NULL))
                                   )) %>%
    tidyr::unnest_longer(adv, indices_to = "stat_type", values_to = "stats") %>%
    dplyr::filter(purrr::map_lgl(stats, ~length(.x) > 0)) %>%
    dplyr::select(stat_type, stats) |>
    tidyr::unnest(stats) |>
    dplyr::relocate(pfr_game_id,.before = 1)

  if(nrow(scrape_games)==0) {
    cli::cli_alert_danger("No new data for scrapes!")
    return(FALSE)
  }

  if(any(!game_ids$game_id %in% scrape_games$game_id)){
    cli::cli_alert_danger("Could not find advanced stats for {paste(game_ids$game_id[!game_ids$game_id %in% scrape_games$game_id], collapse = '\n')}")
  }

  archived_games <- piggyback::pb_download_url(
    file = "advstats_game.rds",
    repo = "nflverse/pfr_scrapR",
    tag = "advstats_raw") |>
    nflreadr::rds_from_url() |>
    dplyr::filter(!pfr_game_id %in% completed_games$pfr_game_id)

  all_games <- bind_rows(archived_games,scrape_games)

  saveRDS(all_games, "build/advstats_game.rds")

  piggyback::pb_upload(file = "build/advstats_game.rds",
                       repo = "nflverse/pfr_scrapR",
                       tag = "advstats_raw",
                       overwrite = TRUE)

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

  local_data <- readRDS("build/advstats_game.rds") |>
    dplyr::filter(pfr_game_id %in% schedules$pfr_game_id)

  pass <- local_data %>%
    dplyr::filter(stat_type == "pass") %>%
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
    dplyr::select(
      pfr_game_id,
      pfr_player_name = player_name,
      pfr_player_id,
      team,
      starts_with("def")
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

      nflversedata::nflverse_save(
        data_frame = dataframe,
        file_name = glue::glue("advstats_week_{stat_type}_{season}"),
        nflverse_type = glue::glue("advanced {stat_type} weekly stats via PFR"),
        release_tag = "pfr_advstats"
      )

    })

  cli::cli_alert_success("Finished combining and uploading adv stats for {season}!")

  invisible(NULL)
}

setwd(here::here())

scrape_result <- scrape_advstats()

if(scrape_result)  clean_advstats()
