source("auto/_helper_functions.R")

s <- nflreadr:::most_recent_season()

completed <- nflreadr::load_schedules() %>%
  filter(season == s, !is.na(result)) %>%
  select(game_id) %>%
  mutate(finished = 1)

urls <- get_game_urls(s) %>%
  mutate(
    pfr_game_id = stringr::str_extract(url, "(?<=boxscores\\/)[:digit:]+[:alpha:]+(?=\\.)")
  ) %>%
  left_join(readRDS("data/pfr_game_id_crosswalk.rds"), by = "pfr_game_id") %>%
  inner_join(completed, by = "game_id")

# do the scrape
snaps <- map_df(urls %>% pull(url), get_game_counts) %>%
  left_join(readRDS("data/pfr_game_id_crosswalk.rds")) %>%
  mutate(
    team = ifelse(type == "home", home_team, away_team),
    pfr_id = stringr::str_extract(url, "(?<=[:upper:]\\/).*(?=\\.htm)")
  ) %>%
  select(player, pfr_id, game_id, pfr_game_id, season, team, offense_snaps : st_pct)

snaps %>%
  readr::write_csv(glue::glue("data/snap_counts_{s}.csv"))

snaps %>%
  saveRDS(glue::glue("data/snap_counts_{s}.rds"))

