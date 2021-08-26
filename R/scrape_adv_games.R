library(dplyr)
library(tidyr)
library(glue)
library(rvest)
library(janitor)
library(readr)

# Example game ID
game_id <- "202101240kan"

all_boxscores <- df_with_pfr_game_id %>% 
  filter(season >= 2018) %>%
  mutate(data = map(game_id, adv_stats))

# output is a list-column with 4x named listed dfs
# unnest_wider to get 4x new columns for def, pass, rush, rec
all_game_advanced <- all_boxscores %>% 
  unnest_wider(data)

all_game_advanced %>% 
  write_rds("all_advanced.rds")

# to get a specific stat, need to drop 
# game_id, unnest_wider() and then unchop()
# need to do this as 1x dataset/stat as there's very little overlap in columns

def_stats <- all_game_advanced %>% 
  select(-game_id) %>% 
  unnest_wider(raw_def) %>% 
  unchop(player:game_id)

adv_stats <- function(game_id){
  
  raw_boxscores <- read_html(glue::glue("https://www.pro-football-reference.com/boxscores/{game_id}.htm"))
  
  read_advanced <- function(adv_stat){
    
    raw_adv_html <- raw_boxscores %>% 
      html_nodes(xpath = '//comment()') %>% 
      html_text() %>%
      paste(collapse = '') %>%
      read_html() %>% 
      html_node(glue::glue("#div_{adv_stat}_advanced"))
    
    raw_table <- html_table(raw_adv_html) %>% 
      janitor::clean_names() %>% 
      filter(tm != "Tm")
    
    raw_players <- raw_adv_html %>% 
      html_nodes("a") %>% 
      html_attr("href") %>% 
      str_subset("players") %>% 
      str_remove("/players/[A-Z]/") %>% 
      str_remove(".htm")
    
    suppressMessages(
      raw_table %>% 
      mutate(
        player_id = raw_players, .after = player) %>% 
      readr::type_convert()
    )
    
  }
  
  raw_def <- read_advanced("defense") %>% mutate(game_id = game_id)
  raw_pass <- read_advanced("passing") %>% mutate(game_id = game_id)
  raw_rush <- read_advanced("rushing") %>% mutate(game_id = game_id)
  raw_rec <- read_advanced("receiving") %>% mutate(game_id = game_id)
  
  list(raw_def = raw_def, raw_pass = raw_pass, raw_rush = raw_rush, raw_rec = raw_rec)
  
}
