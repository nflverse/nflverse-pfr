library(dplyr)
library(tibble)
library(purrr)
library(tidyr)
library(rvest)
library(stringr)
library(janitor)
library(glue)
library(nflreadr)


scrape_draft <- function(year = nflreadr::most_recent_season(roster =  TRUE)) {

  html_scrape <- rvest::read_html(glue::glue("https://www.pro-football-reference.com/years/{year}/draft.htm"))

  table_node <- html_scrape %>%
    rvest::html_element("#drafts")

  cfb_ids <- rvest::html_elements(table_node, "tr th[data-stat='college_link'],td[data-stat='college_link']") %>%
    rvest::html_element("a") %>%
    rvest::html_attr("href") %>%
    stringr::str_remove_all(".+sports-reference.com/cfb/players/|.html") %>%
    tail(-1)

  pfr_ids <- table_node %>%
    rvest::html_elements("tr th[data-stat='player'],td[data-stat='player']") %>%
    html_attr("data-append-csv") %>%
    tail(-1)

  draft_table <- table_node %>%
    rvest::html_table() %>%
    {suppressWarnings(janitor::row_to_names(.,row_number = 1))} %>%
    janitor::clean_names() %>%
    dplyr::bind_cols(pfr_player_id = pfr_ids, cfb_player_id = cfb_ids, .) %>%
    dplyr::filter(pos != "Pos") %>%
    dplyr::transmute(
      season = year,
      round = as.integer(rnd),
      pick = as.integer(pick),
      team = tm,
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
      age,
      to,
      allpro = ap1,
      probowls = pb,
      seasons = st,
      w_av,
      dr_av,
      games = g,
      pass_completions = cmp,
      pass_attempts = att,
      pass_yards = yds,
      pass_tds = td,
      pass_ints = int,
      rush_atts = att_2,
      rush_yards = yds_2,
      rush_tds = td_2,
      receptions = rec,
      rec_yards = yds_3,
      rec_tds = td_3,
      def_solo_tackles = solo,
      def_ints = int_2,
      def_sacks = sk
    )

  return(draft_table)
}

all_drafts <- map_dfr(2000:nflreadr::most_recent_season(roster = TRUE),
                      possibly(scrape_draft,otherwise = tibble()))
saveRDS(all_combines, "data/combine.rds")
readr::write_csv(all_combines, "data/combine.csv")
