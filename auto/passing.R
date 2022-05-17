get_passing <- function(s) {

  cli::cli_process_start("Load PASS {.val {s}}")

  # load page--------------------------------------------------------

  raw_url <- glue::glue("https://www.pro-football-reference.com/years/{s}/passing_advanced.htm")

  raw_html <- rvest::read_html(raw_url)

  # get player IDs --------------------------------------------------------

  ids <- raw_html %>%
    rvest::html_nodes(xpath = '//*[@id="advanced_air_yards"]') %>%
    rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      tibble::as_tibble() %>%
      dplyr::rename(url = value) %>%
      dplyr::filter(stringr::str_detect(url, "players")) %>%
      dplyr::mutate(
        pfr_id = stringr::str_extract(url, "(?<=[:upper:]\\/).*(?=\\.htm)")
      ) %>%
      dplyr::select(pfr_id)

  # read accuracy--------------------------------------------------------

  table2 <- raw_html %>%
    rvest::html_table(fill = TRUE) %>%
    .[[2]] %>%
    janitor::clean_names() %>%
    tibble::tibble() %>%
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
    dplyr::mutate(
      # pfr uses different team abbreviations than nflfastR, fix them
      team = dplyr::case_when(
        team == "GNB" ~ "GB",
        team == "KAN" ~ "KC",
        team == "NOR" ~ "NO",
        team == "NWE" ~ "NE",
        team == "SFO" ~ "SF",
        team == "TAM" ~ "TB",
        TRUE ~ team
      ),
      # repair columns
      player = stringr::str_replace(player, "\\*", ""),
      player = stringr::str_replace(player, "\\+", ""),
      bad_throw_pct = stringr::str_replace(bad_throw_pct, "\\%", ""),
      on_tgt_pct = stringr::str_replace(on_tgt_pct, "\\%", ""),
      drop_pct = stringr::str_replace(drop_pct, "\\%", ""),
      season = s,
      dplyr::across(pass_attempts : on_tgt_pct, ~ as.numeric(.x))
    ) %>%
    dplyr::bind_cols(ids)

  # read pressure--------------------------------------------------------

  table3 <- raw_html %>%
    rvest::html_table(fill = TRUE) %>%
    .[[3]] %>%
    janitor::clean_names() %>%
    tibble::tibble() %>%
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
    dplyr::mutate(
      # repair columns
      player = stringr::str_replace(player, "\\*", ""),
      player = stringr::str_replace(player, "\\+", ""),
      pressure_pct = stringr::str_replace(pressure_pct, "\\%", ""),
      dplyr::across(pocket_time : pressure_pct, ~ as.numeric(.x))
    )

  # read play type--------------------------------------------------------

  table4 <- raw_html %>%
    rvest::html_table(fill = TRUE) %>%
    .[[4]] %>%
    janitor::clean_names() %>%
    tibble::tibble() %>%
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
    dplyr::mutate(
      # repair columns
      player = stringr::str_replace(player, "\\*", ""),
      player = stringr::str_replace(player, "\\+", ""),
      dplyr::across(rpo_plays : pa_pass_yards, ~ as.numeric(.x))
    )

  out <- table2 %>%
    dplyr::full_join(table3, by = "player") %>%
    dplyr::full_join(table4, by = "player")

  cli::cli_process_done()

  out
}

# data seem spotty before 2019
df_advstats <- purrr::map_df(2018:nflreadr:::most_recent_season(), purrr::possibly(get_passing,tibble::tibble()))

nflversedata::nflverse_save(
  data_frame = df_advstats,
  file_name = "advstats_season_pass",
  nflverse_type = "advanced passing season stats via PFR",
  release_tag = "pfr_advstats"
)
