#' Scrape advanced stats from game
#'
#' @param game_id PFR game ID
#'
#' @return a list of four dataframes: passing,rushing, receiving, and defense
#'
#' @export
pfr_game_adv_stats <- function(game_id){

  raw_boxscores <- rvest::read_html(glue::glue("https://www.pro-football-reference.com/boxscores/{game_id}.htm"))

  raw_def <- .pfr_read_advanced("defense", raw_boxscores)

  raw_pass <- .pfr_read_advanced("passing", raw_boxscores)

  raw_rush <- .pfr_read_advanced("rushing", raw_boxscores)

  raw_rec <- .pfr_read_advanced("receiving", raw_boxscores)

  out <- list(pass = raw_pass,
              rush = raw_rush,
              rec = raw_rec,
              def = raw_def) %>%
    purrr::map(.pfr_advstats_rename, game_id)

  return(out)
}

.pfr_read_advanced <- function(adv_stat, raw_boxscores){

  raw_adv_html <- raw_boxscores %>%
    rvest::html_nodes(xpath = '//comment()') %>%
    rvest::html_text() %>%
    paste(collapse = '') %>%
    rvest::read_html() %>%
    rvest::html_node(glue::glue("#div_{adv_stat}_advanced"))

  if(length(raw_adv_html) == 0) return(tibble::tibble())

  raw_table <- rvest::html_table(raw_adv_html) %>%
    janitor::clean_names() %>%
    dplyr::filter(tm != "Tm", player != "")

  raw_players <- raw_adv_html %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    stringr::str_subset("players") %>%
    stringr::str_remove("/players/[A-Z]/") %>%
    stringr::str_remove(".htm")

  suppressMessages(
    raw_table %>%
      dplyr::mutate(
        pfr_player_id = raw_players,
        adv_stat_category = adv_stat,
        .after = player
      ) %>%
      readr::type_convert()
  )
}

.pfr_advstats_rename <- function(raw_df, game_id){

  if(nrow(raw_df)==0) return(raw_df)

  switch(
    raw_df$adv_stat_category[[1]],
    "defense" = names(raw_df) <- new_names_def[names(raw_df)] %c% names(raw_df),
    "rushing" = names(raw_df) <- new_names_rush[names(raw_df)] %c% names(raw_df),
    "receiving" = names(raw_df) <- new_names_rec[names(raw_df)] %c% names(raw_df),
    "passing" = names(raw_df) <- new_names_pass[names(raw_df)] %c% names(raw_df),
  )

  raw_df$pfr_game_id <- game_id

  return(raw_df)
}

new_names_rec <- c(
  "player" = "player_name",
  "player_id" = "pfr_id",
  "tm" = "team",
  "yds" = "receiving_yards",
  "td" = "receiving_td",
  "x1d" = "receiving_fd",
  "ybc" = "receiving_yards_before_catch",
  "ybc_r" = "receiving_yards_before_catch_avg",
  "yac" = "receiving_yards_after_catch",
  "yac_r" = "receiving_yards_after_catch_avg",
  "brk_tkl" = "receiving_broken_tackles",
  "rec_br" = "receiving_per_broken_tackle",
  "drop" = "receiving_drop",
  "drop_percent" = "receiving_drop_pct",
  "int" = "receiving_int",
  "rat" = "receiving_rat"
)

new_names_rush <- c(
  "player" = "player_name",
  "player_id" = "pfr_id",
  "tm" = "team",
  "att" = "carries",
  "yds" = "rushing_yards",
  "x1d" = "rushing_first_downs",
  "ybc" = "rushing_yards_before_contact",
  "ybc_att" = "rushing_yards_before_contact_avg",
  "yac" = "rushing_yards_after_contact",
  "yac_att" = "rushing_yards_after_contact_avg",
  "brk_tkl" = "rushing_broken_tackles"
)

new_names_pass <- c(
  "player" = "player_name",
  "player_id" = "pfr_id",
  "tm" = "team",
  "cmp" = "completions",
  "att" = "attempts",
  "yds" = "passing_yards",
  "x1d" = "passing_first_downs",
  "x1d_percent" = "passing_first_downs_pct",
  "iay" = "intended_air_yards",
  "iay_pa" = "intended_air_yards_per_att",
  "cay" = "completed_air_yards",
  "cay_cmp" = "completed_air_yards_per_cmp",
  "cay_pa" = "completed_air_yards_per_att",
  "yac" = "passing_yards_after_catch",
  "yac_cmp" = "passing_yards_after_catch_per_cmp",
  "drops" = "passing_drops",
  "drop_percent" = "passing_drop_pct",
  "bad_th" = "passing_bad_throws",
  "bad_percent" = "passing_bad_throw_pct",
  "sk" = "times_sacked",
  "bltz" = "times_blitzed",
  "hrry" = "times_hurried",
  "hits" = "times_hit",
  "prss" = "times_pressured",
  "prss_percent" = "times_pressured_pct",
  "scrm" = "scramble_attempts",
  "yds_scr" = "yards_per_scramble"
)

new_names_def <- c(
  "player" = "player_name",
  "player_id" = "pfr_id",
  "tm" = "team",
  "int" = "def_ints",
  "tgt" = "def_targets",
  "cmp" = "def_completions_allowed",
  "cmp_percent" = "def_completion_pct",
  "yds" = "def_yards_allowed",
  "yds_cmp" = "def_yards_allowed_per_cmp",
  "yds_tgt" = "def_yards_allowed_per_tgt",
  "td" = "def_receiving_td_allowed",
  "rat" = "def_passer_rating_allowed",
  "dadot" = "def_adot",
  "air" = "def_air_yards_completed",
  "yac" = "def_yards_after_catch",
  "bltz" = "def_times_blitzed",
  "hrry" = "def_times_hurried",
  "qbkd" = "def_times_hitqb",
  "sk" = "def_sacks",
  "prss" = "def_pressures",
  "comb" = "def_tackles_combined",
  "m_tkl" = "def_missed_tackles",
  "m_tkl_percent" = "def_missed_tackle_pct"
)

# # Example game ID
# game_id <- "202101240kan"
#
# all_boxscores <- df_with_pfr_game_id %>%
#   dplyr::filter(season >= 2018) %>%
#   dplyr::mutate(data = purrr::map(game_id, adv_stats))
#
# # output is a list-column with 4x named listed dfs
# # unnest_wider to get 4x new columns for def, pass, rush, rec
# all_game_advanced <- all_boxscores %>%
#   tidyr::unnest_wider(data)
#
# all_game_advanced %>%
#   write_rds("all_advanced.rds")
#
# # to get a specific stat, need to drop
# # game_id, tidyr::unnest_wider() and then tidyr::unchop()
# # need to do this as 1x dataset/stat as there's very little overlap in columns
#
# def_stats <- all_game_advanced %>%
#   dplyr::select(-game_id) %>%
#   tidyr::unnest_wider(raw_def) %>%
#   tidyr::unchop(player:game_id)
#
