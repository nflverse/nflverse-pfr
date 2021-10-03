pfr_combine <- function(year){

  page_url <- glue::glue("https://www.pro-football-reference.com/draft/{year}-combine.htm")

  table_node <- rvest::read_html(page_url) %>%
    rvest::html_element("#combine")

  ids <- tibble::tibble(
    pfr_id = table_node %>%
      rvest::html_elements("tr th[data-stat='player']") %>%
      rvest::html_attr("data-append-csv"),
    cfb_id = table_node %>%
      rvest::html_elements("tr th[data-stat='college'],td[data-stat='college']") %>%
      rvest::html_element("a") %>%
      rvest::html_attr("href") %>%
      stringr::str_remove_all("https://www.sports-reference.com/cfb/players/|.html")
  ) %>%
    tail(-1)

  combine <- rvest::html_table(table_node,na.strings = "") %>%
    janitor::clean_names() %>%
    dplyr::bind_cols(ids) %>%
    dplyr::filter(player!="Player") %>%
    tidyr::separate(drafted_tm_rnd_yr,c("draft_team","draft_round","draft_ovr","draft_year")," / ") %>%
    dplyr::mutate(
      season = year,
      dplyr::across(c("wt","x40yd","vertical","bench","broad_jump","x3cone",
               "shuttle","draft_round","draft_ovr","draft_year"),
             ~readr::parse_number(.x) %>% round(2))
    ) %>%
    dplyr::select(
      season,
      draft_year,
      draft_team,
      draft_round,
      draft_ovr,
      pfr_id,
      cfb_id,
      player_name = player,
      pos,
      school,
      ht,
      wt,
      forty = x40yd,
      bench,
      vertical,
      broad_jump,
      cone = x3cone,
      shuttle,
    )
  return(combine)
}
