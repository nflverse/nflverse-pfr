#' Scrape PFR Advanced Stats on Season Level
#'
#' @param s Season to scrape
#' @param type Stat type
#'
#' @return A tibble
#' @export
pfr_advanced_stat_season <- function(s, type = c("receiving", "rushing", "defense")) {

  type <- rlang::arg_match(type)

  cli::cli_progress_step("Load advanced {.val {type}} {.val {s}}")

  # Load html and extract relevant part from comments -----------------------

  raw_url <- glue::glue("https://www.pro-football-reference.com/years/{s}/{type}_advanced.htm")
  raw_html <- rvest::read_html(raw_url)
  tbl_html <- xml2::xml_find_all(raw_html, xpath = glue::glue("//div[@id='all_advanced_{type}']/comment()")) |>
    rvest::html_text() |>
    xml2::read_html()


  # Extract Player Information ----------------------------------------------

  player_elements <- tbl_html |>
    xml2::xml_find_all("//td[@data-append-csv]")

  players <- tibble::tibble(
    pfr_id = xml2::xml_attr(player_elements, "data-append-csv"),
    player = xml2::xml_text(player_elements)
  )


  # Extract actual Data -----------------------------------------------------

  out <- rvest::html_table(tbl_html) |>
    purrr::pluck(1) |>
    .check_pfr_stats_names() |>
    janitor::clean_names() |>
    dplyr::filter(rk != "Rk") |>
    dplyr::left_join(players, by = "player") |>
    dplyr::mutate(
      tm = nflreadr::clean_team_abbrs(tm),
      season = s,
      loaded = lubridate::today()
    ) |>
    dplyr::select(season, player, pfr_id, dplyr::everything(), -rk) |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::where(is.character),
        .fns = ~ dplyr::na_if(., "")
      ),
      dplyr::across(
        .cols = tidyselect::contains("percent"),
        .fns = function(x) as.numeric(sub("%","",x)) / 100
      ),
      dplyr::across(
        .cols = !tidyselect::any_of(c("player", "pfr_id", "tm", "pos", "loaded")),
        .fns = as.numeric
      ),
      player = stringr::str_remove_all(player, "\\+|\\*"),
      player = nflreadr::clean_player_names(player),
      pos = toupper(pos)
    )

  cli::cli_progress_done()

  out
}

.check_pfr_stats_names <- function(df){
  if ("Rk" %in% names(df)){
    janitor::clean_names(df)
  } else {
    janitor::row_to_names(df, 1) |>
      janitor::clean_names()
  }
}

#' @export
#' @rdname pfr_advanced_stat_season
pfr_advanced_receiving_season <- function(s) pfr_advanced_stat_season(s = s, type = "receiving")

#' @export
#' @rdname pfr_advanced_stat_season
pfr_advanced_rushing_season <- function(s) pfr_advanced_stat_season(s = s, type = "rushing")

#' @export
#' @rdname pfr_advanced_stat_season
pfr_advanced_defense_season <- function(s) pfr_advanced_stat_season(s = s, type = "defense")
