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

  if(nrow(game_ids)==0) return(cli::cli_alert_danger("No new games to scrape!"))

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

  if(nrow(scrape_games)==0) return(cli::cli_alert_danger("No new data for scrapes!"))

  purrr::pwalk(scrape_games,~{
    filename <- glue::glue("data/adv_stats/game/{..1}_{..2}.")


    readr::write_csv(..3, paste0(filename,"csv"))
    saveRDS(..3,paste0(filename,"rds"))
    # qs::qsave(..3,paste0(filename,"qs"))
  })

  cli::cli_alert_success("Finished scraping {nrow(game_ids)}")
}

scrape_advstats(here::here("data/adv_stats/game"))
