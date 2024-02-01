library(nflfastR)
library(tidyverse)

years <- 2000:2023

pbp <- nflfastR::load_pbp(years)
games <- fast_scraper_schedules(years)


pbp %>%
  filter(quarter_seconds_remaining == 0, game_id == "2009_11_NYJ_NE") %>%
  View()

score_by_quarter <- pbp %>%
  filter(quarter_seconds_remaining == 0, qtr <=4) %>%
  group_by(game_id, qtr) %>%
  filter(play_id == max(play_id)) %>%
  ungroup() %>%
  transmute(game_id, qtr = paste0("q", qtr), score = paste0(total_home_score, "-", total_away_score)) %>%
  spread(qtr, score) %>%
  separate(q1, into = c("q1_home", "q1_away"))%>%
  separate(q2, into = c("q2_home", "q2_away"))%>%
  separate(q3, into = c("q3_home", "q3_away"))%>%
  separate(q4, into = c("q4_home", "q4_away"))

td_count <- pbp %>%
  filter(touchdown == 1) %>%
  group_by(game_id, td_team = ifelse(td_team == home_team, "home_tds", "away_tds")) %>%
  summarize(tds = sum(touchdown)) %>%
  spread(td_team, tds)

games %>% filter(!game_id %in% td_count$game_id) # games with no TDs

fg_count <- pbp %>%
  filter(field_goal_result == "made") %>%
  group_by(game_id, fg_team = ifelse(posteam == home_team, "home_fgs", "away_fgs")) %>%
  summarize(fgs = n()) %>%
  spread(fg_team, fgs)

games %>% filter(!game_id %in% fg_count$game_id) # games with no FGs

d <- games %>%
  select(game_id, season, game_type, week, gameday, weekday, gametime, 
         away_team, home_team, spread_line, total_line, away_score, home_score) %>%
  left_join(score_by_quarter, by = "game_id") %>%
  left_join(td_count, by = "game_id") %>% 
  left_join(fg_count, by = "game_id") %>%
  mutate(away_tds = coalesce(away_tds, 0),
         home_tds = coalesce(home_tds, 0),
         away_fgs = coalesce(away_fgs, 0),
         home_fgs = coalesce(home_fgs, 0))



d %>% write_csv("C:\\Users\\Walker Harrison\\Documents\\GitHub\\nfl_game_data\\games.csv")
