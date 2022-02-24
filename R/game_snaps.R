# game_id <- "201502010sea"

pfr_game_snap_counts <- function(game_id){

  # message(glue::glue("{url}"))

  cli::cli_alert("Starting {game_id}")

  page <- rvest::read_html(glue::glue("https://www.pro-football-reference.com/boxscores/{game_id}.htm")) %>%
    rvest::html_nodes(xpath = '//comment()') %>%
    rvest::html_text() %>%
    paste(collapse = '') %>%
    rvest::read_html()

  if (is.na(page %>% rvest::html_node("#home_snap_counts") %>% rvest::html_text())) {
    cli::cli_alert_danger("No snap data for {game_id}")
    return(tibble::tibble())
  }

  home_ids <- page %>%
    rvest::html_node("#home_snap_counts") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      pfr_player_id = stringr::str_replace_all(value,".+/(.+).htm$","\\1")
    )

  away_ids <- page %>%
    rvest::html_node("#vis_snap_counts") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      pfr_player_id = stringr::str_replace_all(value,".+/(.+).htm$","\\1")
    )

  # first we have to see if the home snap count table is commented out
  home_table <- page %>%
    rvest::html_node("#home_snap_counts") %>%
    rvest::html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    tibble::tibble() %>%
    dplyr::slice(-1) %>%
    # no urls for this player so it would break things
    dplyr::filter(x != "") %>%
    dplyr::bind_cols(home_ids) %>%
    dplyr::mutate(location = "home")

  away_table <- page %>%
    rvest::html_node("#vis_snap_counts") %>%
    rvest::html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    tibble::tibble() %>%
    dplyr::slice(-1) %>%
    # no urls for this player so it would break things
    dplyr::filter(x != "") %>%
    dplyr::bind_cols(away_ids) %>%
    dplyr::mutate(location = "away")

  out <- home_table %>%
    dplyr::bind_rows(away_table) %>%
    dplyr::select(
      player = x,
      pfr_player_id,
      location,
      position = x_2,
      offense_snaps = off,
      offense_pct = off_2,
      defense_snaps = def,
      defense_pct = def_2,
      st_snaps = st,
      st_pct = st_2
    ) %>%
    dplyr::mutate(
      # repair columns
      player = stringr::str_replace(player, "\\*", ""),
      player = stringr::str_replace(player, "\\+", ""),
      offense_pct = stringr::str_replace(offense_pct, "\\%", ""),
      defense_pct = stringr::str_replace(defense_pct, "\\%", ""),
      st_pct = stringr::str_replace(st_pct, "\\%", ""),
      dplyr::across(offense_snaps : st_pct, ~ as.numeric(.x)),
      dplyr::across(dplyr::contains("pct"), ~magrittr::divide_by(.x,100))
    )

  if(game_id %in% nflreadr::csv_from_url("https://github.com/nflverse/pfr_scrapR/releases/download/snap_counts_raw/flip_ids.csv")$flip_ids) {
    out <- out %>%
      dplyr::mutate(location = dplyr::case_when(location == "home" ~ "away", TRUE ~ "home"))
  }

  return(out)
}
