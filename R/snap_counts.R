library(tidyverse)
library(rvest)


# get the pfr master list of 32 teams
get_team_list <- function() {
  
  fetched <- curl::curl_fetch_memory("https://www.pro-football-reference.com/teams/")
  
  teams <- fetched$content %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="teams_active"]') %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    filter(stringr::str_detect(value, "/teams/")) %>%
    mutate(
      id = stringr::str_extract(value, "(?<=teams\\/)[:alpha:]+(?=\\/)")
    ) %>%
    select(id)
  
  teams  
  
}


# get player urls from a team page url
get_player_urls <- function(url) {
  
  # url <- glue::glue("https://www.pro-football-reference.com/teams/{tm}/{s}-snap-counts.htm")
  
  fetched <- curl::curl_fetch_memory(url)
  
  # get the players to iterate over
  players <- fetched$content %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="snap_counts"]') %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    filter(stringr::str_detect(value, "/fantasy/")) %>%
    mutate(
      url = paste0("https://www.pro-football-reference.com", value)
    ) %>%
    select(url)
  
}

# get snap counts from a given player url
get_counts <- function(url) {
  
  # let's try not to pound PFR servers
  # Sys.sleep(1)
  
  # get info from the url
  id <- stringr::str_extract(url, "(?<=\\/)[:alpha:]+[:digit:]+(?=\\/)")
  s <- stringr::str_extract(url, "(?<=fantasy\\/)[:digit:]+")
    
  raw_html <- read_html(url)
  
  raw_html %>% 
    html_table(fill = TRUE) %>% 
    .[[1]] %>% 
    janitor::clean_names() %>% 
    tibble() %>%
    dplyr::slice(-1:-2) %>%
    dplyr::select(
      game_number = x_2,
      game_date = x_3,
      team = x_4,
      opponent = x_6,
      position = x_8,
      offense_snaps = snap_counts,
      offense_pct = snap_counts_2,
      defense_snaps = snap_counts_3,
      defense_pct = snap_counts_4,
      st_snaps = snap_counts_5,
      st_pct = snap_counts_6
    ) %>%
    filter(game_date != "Total") %>%
    mutate(
      # pfr uses different team abbreviations than nflfastR, fix them
      team = case_when(
        team == "GNB" ~ "GB",
        team == "KAN" ~ "KC",
        team == "NOR" ~ "NO",
        team == "NWE" ~ "NE",
        team == "SFO" ~ "SF",
        team == "TAM" ~ "TB",
        TRUE ~ team
      ),
      # repair columns
      offense_pct = str_replace(offense_pct, "\\%", ""),
      defense_pct = str_replace(defense_pct, "\\%", ""),
      st_pct = str_replace(st_pct, "\\%", ""),
      across(offense_snaps : st_pct, ~ as.numeric(.x)),
      page = url,
      season = s
    )
  
}




# get prior seasons (don't want to have this on scheduler) --------------------------------------------------------
if (9 == 10) {
  
  yrs <- 2012:2020
  
  team_pages <- crossing(get_team_list(), yrs) %>%
    mutate(
      url = glue::glue("https://www.pro-football-reference.com/teams/{id}/{yrs}-snap-counts.htm")
    ) %>%
    pull(url)
  
  player_urls <- map_df(team_pages, get_player_urls) %>%
    pull(url)
  
  counts <- map_df(player_urls, get_counts)
  
  counts %>%
    write_csv("data/pfr_snap_counts_historical.csv")
  
}


# get this season --------------------------------------------------------

if (nflfastR:::most_recent_season() == 2021) {
  
  yr <- 2021
  
  team_pages <- crossing(get_team_list(), yr) %>%
    mutate(
      url = glue::glue("https://www.pro-football-reference.com/teams/{id}/{yr}-snap-counts.htm")
    ) %>%
    pull(url)
  
  player_urls <- map_df(team_pages, get_player_urls) %>%
    pull(url)
  
  counts <- map_df(player_urls[1:2], get_counts)
  
  
}

