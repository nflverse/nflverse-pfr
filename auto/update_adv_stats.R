#' Automated Scrape for PFR Advanced Stats
pkgload::load_all()

scrape_advstats <- function(data_path){

  #' List Completed Games
  completed_games <- list.files(data_path) %>%
    stringr::str_replace_all("([[:alnum:]]+)_.*","\\1") %>%
    unique()

  #' Get Game IDs
  game_ids <- pfr_game_urls(2020) %>%
    tidyr::extract(col = "url",
                   into = "game_id",
                   regex = "boxscores/([[:alnum:]]+).htm") %>%
    dplyr::filter(
      game_id < format(Sys.Date(),format = "%Y%m%d"), # only games before today
      !game_id %in% completed_games # only games we have not yet scraped
    )

  if(nrow(game_ids)==0) return(cli::cli_alert_danger("No new games to scrape!"))

  cli::cli_alert("Now scraping {nrow(game_ids)}")

  #' Scrape Incomplete Games
  scrape_games <- game_ids %>%
    dplyr::mutate(adv = purrr::map(game_id, pfr_game_adv_stats)) %>%
    tidyr::unnest_wider(adv) %>%
    tidyr::pivot_longer(
      c(pass, rush, rec, def),
      names_to = "stat_type",
      values_to = "stats"
    ) %>%
    dplyr::filter(purrr::map_lgl(stats, ~length(.x) > 0))

  if(nrow(scrape_games)==0) return(cli::cli_alert_danger("No new data for scrapes!"))

  purrr::pwalk(scrape_games,~{
    filename <- glue::glue("data/adv_stats/game/{..1}_{..2}.")

    readr::write_csv(..3, paste0(filename,"csv"))
    saveRDS(..3,paste0(filename,"rds"))
    qs::qsave(..3,paste0(filename,"qs"))
  })

  cli::cli_alert_success("Finished scraping {nrow(game_ids)}")
}

scrape_advstats(here::here("data/adv_stats/game"))
