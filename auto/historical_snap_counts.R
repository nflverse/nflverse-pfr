# note: this will need to be tweaked to handle an ongoing season

source("auto/_helper_functions.R")

# 2012 is supposed to have snap counts but it's buggy
urls <- map_df(2013 : 2020, get_game_urls)

# do the scrape
snaps <- map_df(urls %>% pull(url), get_game_counts) %>%
  left_join(readRDS("data/pfr_game_id_crosswalk.rds")) %>%
  mutate(
    team = ifelse(type == "home", home_team, away_team),
    pfr_id = stringr::str_extract(url, "(?<=[:upper:]\\/).*(?=\\.htm)")
  ) %>%
  select(player, pfr_id, game_id, pfr_game_id, season, team, offense_snaps : st_pct)


seasons <- unique(snaps$season)

# write
walk(seasons, ~{
  message(glue::glue("Doing season {.x}"))
  for_saving <- snaps %>%
    filter(season == .x)

  for_saving %>%
    write_csv(glue::glue("data/snap_counts_{.x}.csv"))

  for_saving %>%
    saveRDS(glue::glue("data/snap_counts_{.x}.rds"))

})



