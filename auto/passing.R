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
    janitor::row_to_names(1) %>%
    janitor::clean_names() %>%
    dplyr::select(tidyselect::any_of(c(
      "player",
      "team" = "tm",
      "pass_attempts" = "att",
      "batted_balls" = "bats",
      "throwaways" = "th_awy",
      "spikes",
      "drops",
      "drop_pct" = "drop_percent",
      "bad_throws" = "bad_th",
      "bad_throw_pct" = "bad_percent",
      "on_tgt_throws" = "on_tgt",
      "on_tgt_pct" = "on_tgt_percent"
    ))) %>%
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
      dplyr::across(
        .cols = tidyselect::any_of(c("bad_throw_pct", "on_tgt_pct", "drop_pct")),
        .fns = ~ stringr::str_replace(.x, "\\%", "")
      ),
      season = s,
      dplyr::across(c(-player, -team), ~ as.numeric(.x))
    ) %>%
    dplyr::bind_cols(ids)

  # read pressure--------------------------------------------------------

  table3 <- raw_html %>%
    rvest::html_table(fill = TRUE) %>%
    .[[3]] %>%
    janitor::clean_names() %>%
    tibble::tibble() %>%
    janitor::row_to_names(1) %>%
    janitor::clean_names() %>%
    dplyr::select(tidyselect::any_of(c(
      "player",
      "pocket_time" = "pkt_time",
      "times_blitzed" = "bltz",
      "times_hurried" = "hrry",
      "times_hit" = "hits",
      "times_pressured" = "prss",
      "pressure_pct" = "prss_percent"
    ))) %>%
    dplyr::mutate(
      # repair columns
      player = stringr::str_replace(player, "\\*", ""),
      player = stringr::str_replace(player, "\\+", ""),
      pressure_pct = stringr::str_replace(pressure_pct, "\\%", ""),
      dplyr::across(pocket_time : pressure_pct, ~ as.numeric(.x))
    )

  # read play type--------------------------------------------------------

  if (s >= 2019){# tab doesn't exist pre 2019
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
  } else {
    table4 <- tibble::tibble(player = NA)
  }

  out <- table2 %>%
    dplyr::full_join(table3, by = "player") %>%
    dplyr::full_join(table4, by = "player") %>%
    dplyr::filter(!is.na(player))

  cli::cli_process_done()

  out
}

if(Sys.getenv("NFLVERSE_REBUILD", "false") == "true"){
  seasons_to_update <- 2018:nflreadr::most_recent_season()
} else {
  seasons_to_update <- nflreadr::most_recent_season()
}

purrr::walk(
  seasons_to_update,
  purrr::possibly(function(season){
    nflversedata::nflverse_save(
      data_frame = get_passing(season),
      file_name = glue::glue("advstats_season_pass_{season}"),
      nflverse_type = "advanced passing season stats via PFR",
      release_tag = "pfr_advstats",
      file_types = "rds"
    )
  }, quiet = FALSE
  )
)

## NOW COMBINE ALL SEASONS FOR THE FILE nflreadr IS LOADING

combined_advstats <- purrr::map(
  2018:nflreadr::most_recent_season(),
  purrr::possibly(function(season){
    load_from <- glue::glue("https://github.com/nflverse/nflverse-data/releases/download/pfr_advstats/advstats_season_pass_{season}.rds")
    nflreadr::rds_from_url(load_from)
  }, tibble::tibble(), quiet = FALSE),
  .progress = TRUE
) |>
  purrr::list_rbind()

nflversedata::nflverse_save(
  data_frame = combined_advstats,
  file_name = "advstats_season_pass",
  nflverse_type = "advanced passing season stats via PFR",
  release_tag = "pfr_advstats"
)
