library(dplyr)
# library(tibble)
# library(purrr)
# library(tidyr)
# library(rvest)
# library(stringr)
# library(janitor)
# library(glue)
# library(nflreadr)


scrape_draft <- function(year = nflreadr::most_recent_season(roster =  TRUE)) {

  Sys.sleep(1)

  html_scrape <- rvest::read_html(glue::glue("https://www.pro-football-reference.com/years/{year}/draft.htm"))

  table_node <- html_scrape |>
    rvest::html_element("#drafts")

  cfb_ids <- rvest::html_elements(table_node, "tr th[data-stat='college_link'],td[data-stat='college_link']") |>
    rvest::html_element("a") |>
    rvest::html_attr("href") |>
    stringr::str_remove_all(".+sports-reference.com/cfb/players/|.html") |>
    tail(-1)

  pfr_ids <- table_node |>
    rvest::html_elements("tr th[data-stat='player'],td[data-stat='player']") |>
    rvest::html_attr("data-append-csv") |>
    tail(-1)

  roster <- tibble::tibble(gsis_id = character(),
                           pfr_id = character())

  try({
    roster <- nflreadr::load_rosters(seasons = TRUE) |>
      dplyr::filter(!is.na(pfr_id),!is.na(gsis_id)) %>%
      dplyr::distinct(gsis_id, pfr_id)
  },silent = TRUE)

  draft_table <- table_node |>
    rvest::html_table() %>%
    {suppressWarnings(janitor::row_to_names(.,row_number = 1))} |>
    janitor::clean_names() %>%
    dplyr::bind_cols(pfr_player_id = pfr_ids, cfb_player_id = cfb_ids, .) |>
    dplyr::filter(pos != "Pos") |>
    dplyr::left_join(roster, by = c("pfr_player_id"="pfr_id"))

  patch_columns <- c("pfr_player_id", "cfb_player_id", "rnd", "pick", "tm", "player",
                     "pos", "age", "to", "ap1", "pb", "st", "w_av", "dr_av", "g",
                     "cmp", "att", "yds", "td", "int", "att_2", "yds_2", "td_2", "rec",
                     "yds_3", "td_3", "solo", "int_2", "sk", "college_univ", "x",
                     "gsis_id", "car_av")

  patch_columns <- patch_columns[!patch_columns %in% names(draft_table)]

  for(i in patch_columns) draft_table[[i]] <- NA

  draft_table <- draft_table %>%
    dplyr::transmute(
      season = as.integer(year),
      round = as.integer(rnd),
      pick = as.integer(pick),
      team = tm,
      gsis_id,
      pfr_player_id,
      cfb_player_id,
      pfr_player_name = stringr::str_remove_all(player," HOF$"),
      hof = stringr::str_detect(player," HOF$"),
      position = pos,
      category = dplyr::case_when(pos %in% c("RB","HB","FB")~"RB",
                                  pos %in% c("OT","OL","G","C","T") ~ "OL",
                                  pos %in% c("LB","OLB","ILB") ~ "LB",
                                  pos %in% c("DE","DT","NT","DL") ~ "DL",
                                  pos %in% c("DB","CB","S","SAF") ~ "DB",
                                  TRUE ~ pos),
      side = dplyr::case_when(pos %in% c("QB","RB","HB","FB","WR","TE","OT","OL","G","C","T")~"O",
                              pos %in% c("LS","P","K") ~ "S",
                              pos %in% c("DE","LB","DT","DB","CB","NT","DL","OLB","S","ILB","SAF") ~ "D"),
      college = college_univ,
      age = as.numeric(age),
      to = as.integer(to),
      allpro = as.numeric(ap1),
      probowls = as.numeric(pb),
      seasons_started = as.numeric(st),
      w_av = as.numeric(w_av),
      car_av = as.numeric(car_av),
      dr_av = as.numeric(dr_av),
      games = as.numeric(g),
      pass_completions = as.numeric(cmp),
      pass_attempts = as.numeric(att),
      pass_yards = as.numeric(yds),
      pass_tds = as.numeric(td),
      pass_ints = as.numeric(int),
      rush_atts = as.numeric(att_2),
      rush_yards = as.numeric(yds_2),
      rush_tds = as.numeric(td_2),
      receptions = as.numeric(rec),
      rec_yards = as.numeric(yds_3),
      rec_tds = as.numeric(td_3),
      def_solo_tackles = as.numeric(solo),
      def_ints = as.numeric(int_2),
      def_sacks = as.numeric(sk)
    )

  return(draft_table)
}

all_drafts <- purrr::map_dfr(1980:nflreadr::most_recent_season(),
                             purrr::possibly(scrape_draft,otherwise = tibble::tibble()))

# pak::pak("nflverse/nflverse-data")
nflversedata::nflverse_save(
  all_drafts,
  file_name = "draft_picks",
  nflverse_type = "Draft Picks, via Pro Football Reference",
  release_tag =  "draft_picks")
