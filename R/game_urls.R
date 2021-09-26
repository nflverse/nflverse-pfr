#' Game IDs and URLs

pfr_game_urls <- function(season = 2021) {

  url <- glue::glue("https://www.pro-football-reference.com/years/{season}/games.htm")

  fetched <- curl::curl_fetch_memory(url)

  fetched$content %>%
    rvest::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="games"]') %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    tibble::as_tibble() %>%
    dplyr::filter(stringr::str_detect(value, "/boxscores/")) %>%
    dplyr::mutate(url = paste0("https://www.pro-football-reference.com", value)) %>%
    dplyr::select(url)
}

pfr_game_ids <- function(season = 2021){
  pfr_game_urls(season) %>%
    tidyr::extract(col = "url",
                   into = "game_id",
                   regex = "boxscores/([[:alnum:]]+).htm")
}

