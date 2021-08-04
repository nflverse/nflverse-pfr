library(tidyverse)
library(rvest)

get_passing <- function(s) {
  
  # load page--------------------------------------------------------
  
  raw_url <- glue::glue("https://www.pro-football-reference.com/years/{s}/passing_advanced.htm")
  
  raw_html <- read_html(raw_url)
  
  # read accuracy--------------------------------------------------------
  
  table2 <- raw_html %>% 
    html_table(fill = TRUE) %>% 
    .[[2]] %>% 
    janitor::clean_names() %>% 
    tibble() %>%
    dplyr::slice(-1) %>%
    dplyr::select(
      player = x_2,
      team = x_3,
      pass_attempts = passing_2,
      batted_balls = passing_4,
      throwaways = passing_5,
      spikes = passing_6,
      drops = passing_7,
      drop_pct = passing_8,
      bad_throws = passing_9,
      bad_throw_pct = passing_10,
      on_tgt_throws = passing_11,
      on_tgt_pct = passing_12
    ) %>%
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
      player = str_replace(player, "\\*", ""),
      player = str_replace(player, "\\+", ""),
      bad_throw_pct = str_replace(bad_throw_pct, "\\%", ""),
      on_tgt_pct = str_replace(on_tgt_pct, "\\%", ""),
      drop_pct = str_replace(drop_pct, "\\%", ""),
      season = s,
      across(pass_attempts : on_tgt_pct, ~ as.numeric(.x))
    )
  
  # read pressure--------------------------------------------------------
  
  table3 <- raw_html %>% 
    html_table(fill = TRUE) %>% 
    .[[3]] %>% 
    janitor::clean_names() %>% 
    tibble() %>%
    dplyr::slice(-1) %>%
    dplyr::select(
      player = x_2,
      pocket_time = passing_5,
      times_blitzed = passing_6,
      times_hurried = passing_7,
      times_hit = passing_8,
      times_pressured = passing_9,
      pressure_pct = passing_10
    ) %>%
    mutate(
      # repair columns
      player = str_replace(player, "\\*", ""),
      player = str_replace(player, "\\+", ""),
      pressure_pct = str_replace(pressure_pct, "\\%", ""),
      across(pocket_time : pressure_pct, ~ as.numeric(.x))
    )
  
  # read play type--------------------------------------------------------
  
  table4 <- raw_html %>% 
    html_table(fill = TRUE) %>% 
    .[[4]] %>% 
    janitor::clean_names() %>% 
    tibble() %>%
    dplyr::slice(-1) %>%
    dplyr::select(
      player = x_2,
      rpo_plays = rpo,
      rpo_yards = rpo_2,
      rpo_pass_att = rpo_3,
      rpo_pass_yards = rpo_4,
      rpo_rush_att = rpo_5,
      rpo_rush_yards = rpo_6,
      pa_pass_att = play_action,
      pa_pass_yards = play_action_2
    ) %>%
    mutate(
      # repair columns
      player = str_replace(player, "\\*", ""),
      player = str_replace(player, "\\+", ""),
      across(rpo_plays : pa_pass_yards, ~ as.numeric(.x))
    )
  
  table2 %>%
    full_join(table3, by = "player") %>%
    full_join(table4, by = "player")
  
}

# data seem spotty before 2019
data <- map_df(2019 : nflfastR:::most_recent_season(), get_passing)

data %>%
  write_csv("data/pfr_advanced_passing.csv")



