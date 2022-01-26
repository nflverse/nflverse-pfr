library(rvest)

get_rush_season <- function(s) {

  cli::cli_process_start("Load RUSH {.val {s}}")

  raw_url <- glue::glue("https://widgets.sports-reference.com/wg.fcgi?css=1&site",
                        "=pfr&url=%2Fyears%2F{s}%2Frushing_advanced.htm&div=div_advanced_rushing")

  raw_html <- read_html(raw_url)
  tbl_html <- html_element(raw_html, xpath = '//*[@id="advanced_rushing"]')

  # The "data-append-csv" attribut of the dt tags inherits the pfr player ids
  ids <- tbl_html |>
    html_elements("td") |>
    html_attr("data-append-csv") |>
    na.omit()

  df <- html_table(tbl_html)
  names(df) <- as.character(df[1, ])

  suppressWarnings({
    out <- df |>
      janitor::clean_names() |>
      dplyr::filter(rk != "Rk") |>
      dplyr::mutate(
        pfr_id = ids,
        tm = nflreadr::clean_team_abbrs(tm),
        season = s,
        loaded = lubridate::today()
      ) |>
      dplyr::na_if("") |>
      dplyr::select(season, player, pfr_id, dplyr::everything(), -rk) |>
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::contains("percent"),
          .fns = function(x) as.numeric(sub("%","",x)) / 100
        ),
        dplyr::across(
          .cols = !tidyselect::any_of(c("player", "pfr_id", "tm", "pos", "loaded")),
          .fns = as.numeric
        ),
        player = stringr::str_remove_all(player, "\\+|\\*"),
        pos = toupper(pos)
      )
  })

  cli::cli_process_done()

  out
}

# data starts 2018
data <- purrr::map_dfr(2018:nflreadr:::most_recent_season(), get_rush_season)

readr::write_csv(data, "data/adv_stats/adv_rushing_season.csv")

saveRDS(data, "data/adv_stats/adv_rushing_season.rds")

rm(list = ls())
