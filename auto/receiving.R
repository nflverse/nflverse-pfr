library(rvest)

get_rec_season <- function(s) {

  cli::cli_process_start("Load REC {.val {s}}")

  raw_url <- glue::glue("https://widgets.sports-reference.com/wg.fcgi?css=1&site",
                        "=pfr&url=%2Fyears%2F{s}%2Freceiving_advanced.htm&div=div_advanced_receiving")

  raw_html <- read_html(raw_url)
  tbl_html <- html_element(raw_html, xpath = '//*[@id="advanced_receiving"]')

  # The "data-append-csv" attribut of the dt tags inherits the pfr player ids
  ids <- tbl_html |>
    html_elements("td") |>
    html_attr("data-append-csv") |>
    na.omit()

  df <- html_table(tbl_html)

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

# data seem spotty before 2019
df_advstats <- purrr::map_df(2018:nflreadr:::most_recent_season(), get_rec_season)

attr(df_advstats,"nflverse_type") <- glue::glue("advanced receiving season stats via PFR")
attr(df_advstats,"nflverse_timestamp") <- Sys.time()

data.table::fwrite(df_advstats, "build/advstats_season_rec.csv")
saveRDS(df_advstats, "build/advstats_season_rec.rds")
arrow::write_parquet(df_advstats, "build/advstats_season_rec.parquet")
qs::qsave(df_advstats, "build/advstats_season_rec.qs")
