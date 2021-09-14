library(tidyverse)
library(rvest)


# get list of urls for games in each season
get_game_urls <- function(s) {

  url <- glue::glue("https://www.pro-football-reference.com/years/{s}/games.htm")

  fetched <- curl::curl_fetch_memory(url)

  fetched$content %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="games"]') %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    filter(stringr::str_detect(value, "/boxscores/")) %>%
    mutate(
      url = paste0("https://www.pro-football-reference.com", value)
    ) %>%
    select(url)

}



# get snap counts for each player in a given game
get_game_counts <- function(url) {

  # preserve game id
  id <- stringr::str_extract(url, "(?<=boxscores\\/)[:digit:]+[:alpha:]+(?=\\.)")

  message(glue::glue("{url}"))

  fetched <- curl::curl_fetch_memory(url)
  page <- fetched$content %>%
    read_html()

  comments <- page %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse = '') %>%
    read_html()

  home_ids <- comments %>%
    html_node("#home_snap_counts") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    dplyr::rename(url = value)

  away_ids <- comments %>%
    html_node("#vis_snap_counts") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    dplyr::rename(url = value)

  if (is.na(comments %>% html_node("#home_snap_counts") %>% html_text())) {
    return(tibble::tibble())
  }

  # first we have to see if the home snap count table is commented out
  home_table <- comments %>%
    html_node("#home_snap_counts") %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    tibble() %>%
    dplyr::slice(-1) %>%
    # no urls for this player so it would break things
    filter(x != "") %>%
    bind_cols(home_ids) %>%
    dplyr::mutate(type = "home")

  away_table <- comments %>%
    html_node("#vis_snap_counts") %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    tibble() %>%
    dplyr::slice(-1) %>%
    # no urls for this player so it would break things
    filter(x != "") %>%
    bind_cols(away_ids) %>%
    dplyr::mutate(type = "away")

  home_table %>%
    bind_rows(away_table) %>%
    dplyr::select(
      player = x,
      url,
      type,
      position = x_2,
      offense_snaps = off,
      offense_pct = off_2,
      defense_snaps = def,
      defense_pct = def_2,
      st_snaps = st,
      st_pct = st_2
    ) %>%
    mutate(
      # repair columns
      player = str_replace(player, "\\*", ""),
      player = str_replace(player, "\\+", ""),
      offense_pct = str_replace(offense_pct, "\\%", ""),
      defense_pct = str_replace(defense_pct, "\\%", ""),
      st_pct = str_replace(st_pct, "\\%", ""),
      across(offense_snaps : st_pct, ~ as.numeric(.x)),
      pfr_game_id = id
    )

}
