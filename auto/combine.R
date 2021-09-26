library(tidyverse)
library(rvest)
library(janitor)
library(glue)
library(nflreadr)

scrape_combine <- function(year){

  page_url <- glue("https://www.pro-football-reference.com/draft/{year}-combine.htm")

  table_node <- read_html(page_url) %>%
    html_element("#combine")

  ids <- tibble(
    pfr_id = table_node %>%
      html_elements("tr th[data-stat='player']") %>%
      html_attr("data-append-csv"),
    cfb_id = table_node %>%
      html_elements("tr th[data-stat='college'],td[data-stat='college']") %>%
      html_element("a") %>%
      html_attr("href") %>%
      str_remove_all("https://www.sports-reference.com/cfb/players/|.html")
  ) %>%
    tail(-1)

  combine <- html_table(table_node,na.strings = "") %>%
    clean_names() %>%
    bind_cols(ids) %>%
    filter(player!="Player") %>%
    separate(drafted_tm_rnd_yr,c("draft_team","draft_round","draft_ovr","draft_year")," / ") %>%
    mutate(
      season = year,
      across(c("wt","x40yd","vertical","bench","broad_jump","x3cone",
               "shuttle","draft_round","draft_ovr","draft_year"),
             ~parse_number(.x) %>% round(2))
    ) %>%
    select(
      season,
      draft_year,
      draft_team,
      draft_round,
      draft_ovr,
      pfr_id,
      cfb_id,
      player_name = player,
      pos,
      school,
      ht,
      wt,
      forty = x40yd,
      bench,
      vertical,
      broad_jump,
      cone = x3cone,
      shuttle,
    )
  return(combine)
}

all_combines <- map_dfr(2000:nflreadr:::most_recent_season(roster = TRUE),possibly(scrape_combine,otherwise = tibble()))
saveRDS(all_combines, "data/combine.rds")
readr::write_csv(all_combines, "data/combine.csv")
