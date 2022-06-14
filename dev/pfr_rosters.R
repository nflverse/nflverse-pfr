library(rvest)
library(piggyback)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(glue)
library(httr)

scrape_roster <- function(season, team){
  page <-  nflreadr::raw_from_url(as.character(glue::glue("https://www.pro-football-reference.com/teams/{tolower(team)}/{season}_roster.htm"))) |>
    read_html() |>
    html_nodes(xpath = '//comment()') |>
    html_text() |>
    paste(collapse = '') |>
    read_html() |>
    html_element("#roster")

  player_ids <- html_elements(page,"td[data-stat='player']") |> html_attr("data-append-csv")

  tibble(pfr_player_id = player_ids) |>
    bind_cols(html_table(page)) |>
    janitor::clean_names()
}

teams <- data.table::fread("https://github.com/nflverse/nfldata/raw/master/data/teams.csv") |>
  distinct(season,pfr, nfl) |>
  mutate(roster = map2(season,pfr,scrape_roster))

x <- teams |>
  unnest(roster) |>
  left_join(distinct(x, pfr,nfl), by = "pfr") |>
  relocate(nfl, .after = pfr)

x |>
  nflversedata::nflverse_save("pfr_rosters","PFR rosters", "misc")
