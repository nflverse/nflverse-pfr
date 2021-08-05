# make crosswalk between lee's games and pfr IDs

source("R/_helper_functions.R")

lee_games <- nflfastR:::load_lees_games() %>%
  select(game_id, season, week, game_type, gameday, home_team, away_team) %>%
  mutate_at(vars(
    "home_team", "away_team"
  ), nflfastR:::team_name_fn)

urls <- map_df(2012 : nflfastR:::most_recent_season(), get_game_urls)

xwalk <- urls %>%
  mutate(
    pfr_id = stringr::str_extract(url, "(?<=boxscores\\/)[:digit:]+[:alpha:]+(?=\\.)"),
    gameday = glue::glue("{substr(pfr_id, 1, 4)}-{substr(pfr_id, 5, 6)}-{substr(pfr_id, 7, 8)}") %>% as.character(),
    home_team = substr(pfr_id, 10, 12) %>% toupper()
  ) %>%
  mutate_at(vars(
    "home_team"
  ), nflfastR:::team_name_fn) %>%
  mutate(
    # pfr uses different team abbreviations than nflfastR, fix them
    home_team = case_when(
      home_team == "GNB" ~ "GB",
      home_team == "KAN" ~ "KC",
      home_team == "CLT" ~ "IND",
      home_team == "NOR" ~ "NO",
      home_team == "NWE" ~ "NE",
      home_team == "SFO" ~ "SF",
      home_team == "TAM" ~ "TB",
      home_team == "RAV" ~ "BAL",
      home_team == "RAM" ~ "LA",
      home_team == "LACG" ~ "LAC",
      home_team == "HTX" ~ "HOU",
      home_team == "CRD" ~ "ARI",
      home_team == "RAI" ~ "LV",
      home_team == "OTI" ~ "TEN",
      TRUE ~ home_team
    )
  ) %>%
  left_join(lee_games, by = c("gameday", "home_team")) %>%
  select(
    game_id, season, pfr_game_id = pfr_id, home_team, away_team
  )

xwalk %>%
  write_csv("data/pfr_game_id_crosswalk.csv")

xwalk %>%
  saveRDS("data/pfr_game_id_crosswalk.rds")

