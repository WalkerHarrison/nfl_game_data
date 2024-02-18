
library(tidyverse)
library(nflfastR)
library(nfl4th)
library(xgboost)
library(furrr)
library(progressr)
library(expm) 

options(future.rng.onMisuse="ignore")
slice <- dplyr::slice

# rounding numbers without messing up sum 
# https://www.r-bloggers.com/2016/07/round-values-while-preserve-their-rounded-sum-in-r/
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

# all PBP data available
years <- 1999:2023
pbp <- load_pbp(years)

# ten years of PBP data for resampling
pbp_modern <- pbp %>% filter(season %in% 2014:2023)

# game results with team name fixing
games <- fast_scraper_schedules(years) %>%
  mutate(winner = ifelse(home_score > away_score, home_team, away_team)) %>%
  mutate(
    winner = case_when(
      winner == 'OAK' ~ 'LV',
      winner == 'SD' ~ 'LAC',
      winner == 'STL' ~ 'LA',
      TRUE ~ winner
    ))

################### simulation functions ###################

# main pbp sample
play_sample <- pbp_modern %>%
  # weeds out timeouts, ends of quarters, kickoffs, etc.
  filter(!is.na(down)) %>%
  group_by(game_id) %>%
  # need following play conditions to seed next play
  mutate(turnover = interception + fumble_lost > 0,
         yardline_cat = ceiling(yardline_100/10),
         next_yardline_100 = lead(yardline_100), 
         yards_on_play = yardline_100 - next_yardline_100,
         next_down = lead(down),
         next_goal_to_go = lead(goal_to_go),
         next_ydstogo = lead(ydstogo),
         turnover = coalesce(turnover, FALSE),
         touchdown = coalesce(touchdown, FALSE)) %>%
  ungroup() %>%
  # try to limit to circumstances where teams are trying to score with no regard for clock
  group_by(game_id, fixed_drive) %>%
  filter((qtr %in% 1:2 & max(half_seconds_remaining) >= 300) | 
           (qtr %in% 3:4 & abs(score_differential) <= 8) & max(half_seconds_remaining) >= 900) %>%
  ungroup() %>%
  # weird play that screws things up
  filter(!str_detect(tolower(desc), "delay of game, declined")) %>%
  # usually just trying to get guys to jump
  filter(down != 4 | penalty != "Delay of Game") %>%
  select(play_id, game_id, fixed_drive, fixed_drive_result, home_team, away_team, season_type, week, posteam, defteam, side_of_field, yardline_100,
         qtr, quarter_seconds_remaining, down, goal_to_go, ydstogo, play_type, desc, yrdln, end_yard_line, yards_gained, touchdown,
         play_type, field_goal_attempt, field_goal_result, interception, fumble_lost, fourth_down_failed,
         penalty, penalty_type, penalty_yards,
         turnover, yardline_cat, next_yardline_100, yards_on_play, next_down, next_goal_to_go, next_ydstogo)

# buffing our sample with more 4th down plays since simulations will run into new territory
extra_fourth_sample <- pbp_modern %>%
  filter(!is.na(down)) %>%
  group_by(game_id) %>%
  mutate(turnover = interception + fumble_lost > 0,
         yardline_cat = ceiling(yardline_100/10),
         next_yardline_100 = lead(yardline_100), 
         yards_on_play = yardline_100 - next_yardline_100,
         next_down = lead(down),
         next_goal_to_go = lead(goal_to_go),
         next_ydstogo = lead(ydstogo),
         turnover = coalesce(turnover, FALSE),
         touchdown = coalesce(touchdown, FALSE)) %>%
  ungroup() %>%
  # find only actual attempts to convert
  filter(down == 4, !is.na(play_type), play_type %in% c('pass', 'run', 'no_play')) %>%
  filter(!str_detect(tolower(desc), "field goal")) %>%
  filter(!str_detect(tolower(desc), "punt")) %>%
  # weird play that screws things up
  filter(!str_detect(tolower(desc), "delay of game, declined")) %>%
  # usually just trying to get guys to jump
  filter(down != 4 | penalty != "Delay of Game") %>%
  select(play_id, game_id, fixed_drive, fixed_drive_result, home_team, away_team, season_type, week, posteam, defteam, side_of_field, yardline_100,
         qtr, quarter_seconds_remaining, down, goal_to_go, ydstogo, play_type, desc, yrdln, end_yard_line, yards_gained, touchdown,
         play_type, field_goal_attempt, field_goal_result, interception, fumble_lost, fourth_down_failed,
         penalty, penalty_type, penalty_yards,
         turnover, yardline_cat, next_yardline_100, yards_on_play, next_down, next_goal_to_go, next_ydstogo) %>% 
  # get rid of final play when clock runs out -- no next play accounting
  filter(touchdown + fourth_down_failed > 0 | !is.na(next_down))

# even more 4th down plays for missing combinations of field possession + distance
fake_fourth_sample <- extra_fourth_sample %>% 
  count(ydstogo, yardline_100, goal_to_go) %>% 
  complete(ydstogo, yardline_100, goal_to_go, fill = list(n = 0)) %>%
  mutate(yardline_cat = ceiling(yardline_100/10)) %>%
  filter(n == 0) %>%
  select(-n) %>%
  inner_join(extra_fourth_sample %>% select(-yardline_100), by = c("ydstogo", "yardline_cat", "goal_to_go"))

# function to simulate being down a FG after first possession
make_drive_down_FG <- function(seed = NA, debug = FALSE){
  
  # useful for tracking down errors
  if(debug) browser()
  set.seed(coalesce(seed, sample(1:2^15, 1)))
  
  # start at 25 for simplicity
  first_play_start <-  data.frame(
    yardline_100 = 75,
    down = 1,
    ydstogo = 10,
    goal_to_go = 0
  )
  
  # opening play possibilities
  first_play_sample <- play_sample %>% 
    filter(!play_type %in% c('punt', 'field_goal')) %>%
    filter(!str_detect(tolower(desc), "field goal")) %>%
    filter(!str_detect(tolower(desc), "punt")) %>%
    filter(!is.na(next_yardline_100) | touchdown == 1)
  
  # sample 1
  first_play <- first_play_start %>%
    inner_join(first_play_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go")) %>%
    sample_n(1) %>%
    mutate(end = turnover | touchdown) %>%
    select(yardline_100, yardline_cat, down, ydstogo, play_type, penalty, penalty_type, penalty_yards, interception, fumble_lost, turnover, touchdown, everything())
  
  # initiate drive
  drive <- first_play
  
  # add plays until drive ends
  while(sum(drive$end) < 1){
    
    # next play starts where previous one ended
    next_play_start <- drive %>%
      slice(nrow(.)) %>%
      select(yardline_100 = next_yardline_100, 
             down = next_down, 
             ydstogo = next_ydstogo,
             goal_to_go = next_goal_to_go) 
    
    if(next_play_start$down == 4){
      
      # 4th down decision model
      next_play_4th <- suppressMessages(next_play_start %>%
                                          # balanced game from spread/total perspective
                                          mutate(
                                            home_team = "HOU",
                                            away_team = "JAX",
                                            posteam = "HOU",
                                            type = "reg",
                                            season = 2023
                                          ) %>%
                                          # decision should be pretty similar to being down 3 w 3 minutes remaining but no TO
                                          # i.e. you have time to do what you want, but you will not get ball back
                                          mutate(qtr = 4,
                                                 quarter_seconds_remaining = 180,
                                                 score_differential = -3,
                                                 home_opening_kickoff = 0,
                                                 posteam_timeouts_remaining = 3,
                                                 defteam_timeouts_remaining = 0) %>%
                                          add_4th_probs() %>%
                                          # no punts
                                          select(go_wp, fg_wp))
      # sort by highest win-prob
      choice <- next_play_4th %>%
        gather(type, val) %>%
        arrange(desc(val)) %>%
        slice(1) %>%
        pull(type)
      
      if(choice == "fg_wp"){
        
        # filter for FG
        next_play_sample <- play_sample %>% filter(play_type == "field_goal") %>% select(-down, -ydstogo, -goal_to_go)
        
        # only need to join on yardline since down/distance is probably irrelevant to FG
        next_play <-  next_play_start %>%
          inner_join(next_play_sample, by = c("yardline_100")) %>%
          sample_n(min(nrow(.), 1)) %>%
          mutate(end = 1)
        
        # if you're kicking from a distance that has never been attempted then we're just going to say you missed
        if(nrow(next_play) == 0) next_play <- next_play_start %>% mutate(fixed_drive_result = "Missed field goal", end = 1)
        
      } else if(choice == "go_wp"){
        
        # figuring out which play sample to use. prefer original, but use extra or fake if need to
        go_exists <- nrow(next_play_start %>% inner_join(first_play_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go"))) > 0
        extra_exists <- nrow(next_play_start %>% inner_join(extra_fourth_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go"))) > 0
        next_play_sample <- if(go_exists) first_play_sample else if (extra_exists) extra_fourth_sample else fake_fourth_sample
        
        # join on down, distance, yardline
        next_play <- next_play_start %>%
          inner_join(next_play_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go")) %>%
          sample_n(min(1, nrow(.))) %>%
          mutate(end = turnover | touchdown | fourth_down_failed)
        
        # if we still can't find a fourth down then you're in a terrible position and we're going to say you fail
        if(nrow(next_play) == 0) next_play <- next_play_start %>% mutate(fixed_drive_result = "Turnover on downs", fourth_down_failed = 1, end = 1)
        
      }
      
    } else{
      
      # for downs 1-3, use original sample
      next_play_sample <- first_play_sample
      
      # join on down, distance, yardline
      next_play <- next_play_start %>%
        inner_join(next_play_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go")) %>%
        sample_n(1) %>%
        mutate(end = turnover | touchdown)
      
    }
    
    # increment drive
    drive <- bind_rows(drive, next_play)
    
  }
  
  drive
  
}

# function to simulate being down a TD after first possession
make_drive_down_TD <- function(seed = NA, debug = FALSE){
  
  # useful for tracking down errors
  if(debug) browser()
  set.seed(coalesce(seed, sample(1:2^15, 1)))
  
  # start at 25 for simplicity
  first_play_start <-  data.frame(
    yardline_100 = 75,
    down = 1,
    ydstogo = 10,
    goal_to_go = 0
  )
  
  # opening play possibilities
  first_play_sample <- play_sample %>% 
    filter(!play_type %in% c('punt', 'field_goal')) %>%
    filter(!str_detect(tolower(desc), "field goal")) %>%
    filter(!str_detect(tolower(desc), "punt")) %>%
    filter(!is.na(next_yardline_100) | touchdown == 1)
  
  # sample 1
  first_play <- first_play_start %>%
    inner_join(first_play_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go")) %>%
    sample_n(1) %>%
    mutate(end = turnover | touchdown) %>%
    select(yardline_100, yardline_cat, down, ydstogo, play_type, penalty, penalty_type, penalty_yards, interception, fumble_lost, turnover, touchdown, everything())
  
  # initiate drive
  drive <- first_play
  
  # add plays until drive ends
  while(sum(drive$end) < 1){
    
    # next play starts where previous one ended
    next_play_start <- drive %>%
      slice(nrow(.)) %>%
      select(yardline_100 = next_yardline_100, 
             down = next_down, 
             ydstogo = next_ydstogo,
             goal_to_go = next_goal_to_go) 
    
    if(next_play_start$down == 4){
      
      # figuring out which play sample to use. prefer original, but use extra or fake if need to
      go_exists <- nrow(next_play_start %>% inner_join(first_play_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go"))) > 0
      extra_exists <- nrow(next_play_start %>% inner_join(extra_fourth_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go"))) > 0
      next_play_sample <- if(go_exists) first_play_sample else if (extra_exists) extra_fourth_sample else fake_fourth_sample
      
      # join on down, distance, yardline
      next_play <- next_play_start %>%
        inner_join(next_play_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go")) %>%
        sample_n(min(1, nrow(.))) %>%
        mutate(end = turnover | touchdown | fourth_down_failed)
      
      # if we still can't find a fourth down then you're in a terrible position and we're going to say you fail
      if(nrow(next_play) == 0) next_play <- next_play_start %>% mutate(fixed_drive_result = "Turnover on downs", fourth_down_failed = 1, end = 1)
      
    } else{
      
      # for downs 1-3, use original sample
      next_play_sample <- first_play_sample
      
      # join on down, distance, yardline
      next_play <- next_play_start %>%
        inner_join(next_play_sample, by = c("down", "ydstogo", "yardline_100", "goal_to_go")) %>%
        sample_n(1) %>%
        mutate(end = turnover | touchdown)
      
    }
    
    # increment drive
    drive <- bind_rows(drive, next_play)
    
  }
  
  drive
  
}

sims_file <- "sims_217.Rdata"

if(is.na(sims_file)){
  # these functions are SLOW. need parallel for large n
  plan(multisession, workers = 8)
  
  n_sims <- 10000
  
  down_FG_sims <- future_map(1:n_sims, ~{
    #print(.x)
    tryCatch(expr = {make_drive_down_FG(seed = .x) %>% mutate(sim  = .x)},
             error = function(e) {data.frame(sim = .x, fixed_drive_result = "error")})
  }) %>% 
    bind_rows()
  
  down_TD_sims <- future_map(1:n_sims, ~{
    #print(.x)
    tryCatch(expr = {make_drive_down_TD(seed = .x) %>% mutate(sim  = .x)},
             error = function(e) {data.frame(sim = .x, fixed_drive_result = "error")})
  })%>% 
    bind_rows()
  
} else{
  load(sims_file)
}



# tallying drive results
second_poss_down3_sim <- down_FG_sims %>%
  filter(end == 1) %>%
  count(fixed_drive_result) %>%
  mutate(p = n/sum(n))

second_poss_down6_sim <- down_TD_sims %>%
  filter(end == 1) %>%
  count(fixed_drive_result) %>%
  mutate(p = n/sum(n))


################### empirical results ###################

# XP success rate in recent season
XP_success_rate <- pbp %>%
  filter(season == 2023) %>%
  filter(play_type == "extra_point") %>%
  summarize(hit = mean(extra_point_result == "good")) %>%
  pull(hit)

# 2pt success rate in last ten years
twopoint_success_rate <- pbp_modern %>%
  filter(two_point_attempt == 1) %>%
  summarize(hit = mean(two_point_conv_result == "success")) %>%
  pull(hit)

# sudden death win rate
OT_receive_win_rate <- pbp %>%
  # old OT rules ended after 2011
  filter(season < 2012) %>%
  # first team to possess ball
  filter(qtr > 4, play_type == "kickoff")  %>%
  transmute(game_id, receive = posteam) %>%
  inner_join(games, by = "game_id") %>%
  summarize(n = n(), win = mean(receive == winner)) %>%
  pull(win)

# approximate first drive scoring with results from PBP sample
first_poss_emp <- play_sample %>%
  group_by(game_id, fixed_drive) %>%
  mutate(yardline_100_start = yardline_100[which.min(play_id)]) %>%
  ungroup() %>%
  # only use drives that started around own 25, as if receiving kick
  filter(yardline_100_start %in% 65:85) %>%
  select(game_id, fixed_drive, fixed_drive_result) %>%
  distinct() %>%
  count(fixed_drive_result) %>%
  mutate(p = n/sum(n))

# old empirical drive results when receiving team doesn't score
second_poss_down0_emp <- pbp %>%
  # old OT rules ended after 2011
  filter(season < 2012) %>%
  # first team to possess ball
  filter(qtr > 4)  %>%
  group_by(game_id) %>%
  # second drive in OT
  filter(fixed_drive == min(fixed_drive) + 1) %>%
  ungroup() %>%
  select(game_id, fixed_drive, fixed_drive_result) %>%
  distinct() %>%
  count(fixed_drive_result) %>%
  mutate(p = n/sum(n))



################### transitions ##################

# empirical possessions
first_poss <- first_poss_emp
second_poss_down0 <- second_poss_down0_emp

# simulated possessions
second_poss_down3 <- second_poss_down3_sim
second_poss_down6 <- second_poss_down6_sim

# find scoring results
first_poss_td <- first_poss$p[first_poss$fixed_drive_result == "Touchdown"]
first_poss_fg <- first_poss$p[first_poss$fixed_drive_result == "Field goal"]

second_poss_down0_td <- second_poss_down0$p[second_poss_down0$fixed_drive_result == "Touchdown"]
second_poss_down0_fg <- second_poss_down0$p[second_poss_down0$fixed_drive_result == "Field goal"]

second_poss_down3_td <- second_poss_down3$p[second_poss_down3$fixed_drive_result == "Touchdown"]
second_poss_down3_fg <- second_poss_down3$p[second_poss_down3$fixed_drive_result == "Field goal"]

second_poss_down6_td <- second_poss_down6$p[second_poss_down6$fixed_drive_result == "Touchdown"]

# create transition matrix
nodes <- c('first_poss', 
           'second_poss_0',
           'second_poss_3',
           'second_poss_6',
           'second_poss_7',
           'sudden_death',
           'kicking_team_win',
           'receiving_team_win')

transition_probs <- matrix(0, length(nodes), length(nodes))
row.names(transition_probs) <- nodes
colnames(transition_probs) <- nodes

# build transitions using scoring results as well as XP/2PT conversion rates
transition_probs[nodes == "first_poss", nodes == "second_poss_7"] <- first_poss_td*XP_success_rate
transition_probs[nodes == "first_poss", nodes == "second_poss_6"] <- first_poss_td*(1-XP_success_rate)
transition_probs[nodes == "first_poss", nodes == "second_poss_3"] <- first_poss_fg
transition_probs[nodes == "first_poss", nodes == "second_poss_0"] <- 1 - first_poss_td - first_poss_fg

transition_probs[nodes == "second_poss_0", nodes == "kicking_team_win"] <- second_poss_down0_td + second_poss_down0_fg
transition_probs[nodes == "second_poss_0", nodes == "sudden_death"] <- 1 - second_poss_down0_td - second_poss_down0_fg

transition_probs[nodes == "second_poss_3", nodes == "kicking_team_win"] <- second_poss_down3_td
transition_probs[nodes == "second_poss_3", nodes == "sudden_death"] <- second_poss_down3_fg
transition_probs[nodes == "second_poss_3", nodes == "receiving_team_win"] <- 1 - second_poss_down3_td - second_poss_down3_fg

transition_probs[nodes == "second_poss_6", nodes == "kicking_team_win"] <- second_poss_down6_td*XP_success_rate
transition_probs[nodes == "second_poss_6", nodes == "sudden_death"] <- second_poss_down6_td*(1-XP_success_rate)
transition_probs[nodes == "second_poss_6", nodes == "receiving_team_win"] <- 1 - second_poss_down6_td 

transition_probs[nodes == "second_poss_7", nodes == "kicking_team_win"] <- second_poss_down6_td*twopoint_success_rate
transition_probs[nodes == "second_poss_7", nodes == "receiving_team_win"] <- second_poss_down6_td*(1-twopoint_success_rate) + (1-second_poss_down6_td)

transition_probs[nodes == "sudden_death", nodes == "kicking_team_win"] <- 1 - OT_receive_win_rate
transition_probs[nodes == "sudden_death", nodes == "receiving_team_win"] <- OT_receive_win_rate

transition_probs[nodes == "kicking_team_win", nodes == "kicking_team_win"] <- 1
transition_probs[nodes == "receiving_team_win", nodes == "receiving_team_win"] <- 1

# must all sum to 1
mean(rowSums(transition_probs) == 1)

# match display on graph
transition_probs_rounded <- t(apply(transition_probs, 1, round_preserve_sum, 2))
mean(rowSums(transition_probs_rounded) == 1)

# get stationary state
stationary_dist <- (transition_probs_rounded %^% 3)[1, ]
names(stationary_dist) <- nodes
stationary_dist  
 