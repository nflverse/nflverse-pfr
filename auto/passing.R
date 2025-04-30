get_passing <- function(s) {

  cli::cli_process_start("Load PASS {.val {s}}")

  # load page--------------------------------------------------------

  raw_url <- glue::glue("https://www.pro-football-reference.com/years/{s}/passing_advanced.htm")

  raw_html <- rvest::read_html(raw_url)

  # get player IDs --------------------------------------------------------

  ids <- raw_html |>
    rvest::html_nodes(xpath = '//*[@id="passing_advanced"]') |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    tibble::as_tibble() |>
    dplyr::rename(url = value) |>
    dplyr::filter(stringr::str_detect(url, "players")) |>
    dplyr::mutate(
      pfr_id = stringr::str_extract(url, "(?<=[:upper:]\\/).*(?=\\.htm)")
    ) |>
    dplyr::select(pfr_id)

  # read accuracy--------------------------------------------------------

  out <- raw_html |>
    rvest::html_table(fill = TRUE) |>
    purrr::pluck(1) |>
    .check_pfr_stats_names() |>
    dplyr::filter(rk != "Rk", tolower(player) != "league average") |>
    dplyr::select(tidyselect::any_of(c(
      "player",
      "team" = "team",
      "pass_attempts" = "att",

      # Air Yards
      "intended_air_yards" = "iay",
      "intended_air_yards_per_pass_attempt" = "iay_pa",
      "completed_air_yards" = "cay",
      "completed_air_yards_per_completion" = "cay_cmp",
      "completed_air_yards_per_pass_attempt" = "cay_pa",
      "pass_yards_after_catch" = "yac",
      "pass_yards_after_catch_per_completion" = "yac_cmp",

      # Accuracy
      "batted_balls" = "bats",
      "throwaways" = "th_awy",
      "spikes",
      "drops",
      "drop_pct" = "drop_percent",
      "bad_throws" = "bad_th",
      "bad_throw_pct" = "bad_percent",
      "on_tgt_throws" = "on_tgt",
      "on_tgt_pct" = "on_tgt_percent",

      # Pressure
      "pocket_time" = "pkt_time",
      "times_blitzed" = "bltz",
      "times_hurried" = "hrry",
      "times_hit" = "hits",
      "times_pressured" = "prss",
      "pressure_pct" = "prss_percent",
      "scrambles" = "scrm",
      "scramble_yards_per_attempt" = "yds_scr",

      # RPO
      "rpo_plays" = "plays",
      "rpo_yards" = "yds",
      "rpo_pass_att" = "pass_att",
      "rpo_pass_yards" = "pass_yds",
      "rpo_rush_att" = "rush_att",
      "rpo_rush_yards" = "rush_yds",

      # Play Action
      "pa_pass_att" = "pass_att2",
      "pa_pass_yards" = "pass_yds2"
    ))) |>
    dplyr::mutate(
      # pfr uses different team abbreviations than nflfastR, fix them
      team = suppressWarnings(nflreadr::clean_team_abbrs(team)),
      # repair columns
      player = stringr::str_replace(player, "\\*", ""),
      player = stringr::str_replace(player, "\\+", ""),
      dplyr::across(
        .cols = tidyselect::any_of(c("bad_throw_pct", "on_tgt_pct", "drop_pct", "pressure_pct")),
        .fns = ~ stringr::str_replace(.x, "\\%", "")
      ),
      season = s,
      dplyr::across(c(-player, -team), ~ as.numeric(.x))
    ) |>
    dplyr::bind_cols(ids)

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
