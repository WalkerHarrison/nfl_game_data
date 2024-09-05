library(tidyverse)
library(rvest)
library(nfl4th)

teams <- nflfastR::teams_colors_logos %>% filter(team_abbr != "LA")
teams$team_abbr[teams$team_abbr == "WAS"] <- "WSH"

setwd("/Users/walkerharrison/")

# turn on for initial download, off otherwise
# takes about 5 min
download <- FALSE

###### fetching webpages ######
if(download){
  dir.create("NFLPreseasonScrape2")
  setwd("NFLPreseasonScrape2")
  
  years <- as.character(2023:2024)
  
  for(year in years){
    #year <- years[1]
    print(year)
    dir.create(year)
    setwd(year)
    
    weeks <- 1:4
    
    for(week in weeks){
      print(week)
      #week <- weeks[4]
      dir.create(paste0("Week", week))
      setwd(paste0("Week", week))
      
      week_link <- paste0("https://www.espn.com/nfl/scoreboard/_/week/", week, "/year/", year, "/seasontype/1")
      scoreboard <- read_html(week_link)
      
      urls <- scoreboard %>%
        html_nodes(".mr2") %>%
        keep(~html_text(.)=="Gamecast") %>%
        html_attr("href") %>%
        paste0("https://www.espn.com", .) %>%
        unique()
      
      for(url in urls){
        #url <- urls[1]
        Sys.sleep(2)
        print(url)
        game_id <- str_extract(url, "\\d+")
        file <- paste0(game_id, ".html")
        
        download.file(url, file)
        
      }
      
      setwd("..")
      
    }
    
    setwd("..")
    
  }
  
  
  
}


###### parsing pbp ######
setwd("/Users/walkerharrison/NFLPreseasonScrape2/")

page_files <- list.files(recursive = T) %>% keep(~str_detect(.x, "html"))

pbp_raw <- map(page_files, ~{
  
  #.x <- page_files[1]
  print(.x)
  page <- read_html(.x)
  season <- as.numeric(str_extract(.x, "\\d{4}"))
  week <- as.numeric(str_extract(.x, "(?<=Week)\\d{1}"))
  game_id <- as.numeric(str_extract(.x, "\\d{9}"))
  
  datetime <- page %>%
    html_nodes(".GameInfo__Meta") %>%
    html_text() %>%
    lubridate::as_datetime(format = "%I:%M %p, %B %d, %Y")
  
  raw_JSON_text <- page %>% 
    html_nodes("body") %>%
    html_nodes("script") %>%
    keep(~str_detect(html_text(.x), "allPlys")) %>%
    html_text() %>%
    str_extract("allPlys.+")
  
  char_idxes <- raw_JSON_text %>%
    str_split_fixed(pattern = "", n = nchar(.)) %>%
    as.vector() %>%
    tibble(character = .) %>%
    mutate(idx = row_number(),
           unclosed = cumsum(character == "[") - cumsum(character == "]")) %>%
    filter(idx >= first(which(unclosed==1))) %>%
    mutate(idx2 = row_number()) %>%
    filter(idx2 <= first(which(unclosed==0))) %>%
    slice(c(1, nrow(.))) %>%
    pull(idx)
  
  plays <- substr(raw_JSON_text, char_idxes[1], char_idxes[2]) %>%
    jsonlite::fromJSON() %>% 
    unnest(plays, , names_sep = "_") %>%
    unnest(awayTeam, names_sep = "_") %>%
    unnest(homeTeam, names_sep = "_") %>%
    mutate(season = season,
           week = week,
           datetime = datetime,
           game_id = game_id) %>%
    select(season, week, game_id, everything())
  
}) %>%
  bind_rows()

pbp_raw2 <- pbp_raw %>% 
  group_by(game_id) %>%
  mutate(drive_idx = 1 + cumsum(id != lag(id, default = first(id)))) %>%
  group_by(game_id, drive_idx) %>%
  mutate(play_idx = row_number()) %>%
  ungroup() %>%
  rename(home_team_score = homeTeam_score,
         away_team_score= awayTeam_score,
         dd = description,
         description = plays_description,
         context = plays_headline,
         home_team = homeTeam_name,
         away_team = awayTeam_name) %>%
  left_join(teams %>% select(teamName = team_name, possession = team_abbr), by = "teamName")

pbp <- pbp_raw2 %>%
  group_split(game_id) %>%
  map(~{
    
    # .x <- pbp_raw2 %>% filter(game_id == first(game_id))
    
    .x %>%
      #mutate(home_team_score = as.numeric(home_team_score)) %>%
      mutate(
        home_team_score2 = case_when(
          drive_idx == 1 ~ 0L,
          lag(drive_idx) != drive_idx ~ lag(home_team_score),
          lag(drive_idx) == drive_idx ~ NA_integer_
        ),
        away_team_score2 = case_when(
          drive_idx == 1 ~ 0L,
          lag(drive_idx) != drive_idx ~ lag(away_team_score),
          lag(drive_idx) == drive_idx ~ NA_integer_
        )
      ) %>%
      fill(home_team_score2, .direction = "down") %>%
      fill(away_team_score2, .direction = "down") %>% #View()
      mutate(home_team_score = home_team_score2,
             away_team_score = away_team_score2) %>%
      mutate(time = str_extract(description, "(?<=\\()[^)]*(?=\\))"),
             qtr = as.numeric(str_extract(time, "(?<=- )\\d+")),
             minute = as.numeric(str_extract(time, "\\d+(?=:)")),
             second = as.numeric(str_extract(time, "(?<=:)\\d+")),
             quarter_seconds_remaining = minute*60 + second
      ) %>%
      mutate(down = as.numeric(str_extract(context, "\\d")),
             ydstogo = as.numeric(str_extract(context, "(?<=&\\s)\\d+")),
             yard_line = str_extract(context, "(?<=\\bat\\s).*$"),
             field_side = str_extract(yard_line, ".*?(?=\\s\\d+)"),
             yard_line = as.numeric(str_extract(yard_line, "\\d+")),
             yardline_100 = case_when(
               yard_line == 50 ~ 50,
               possession == field_side ~ 100 - yard_line,
               possession != field_side ~ yard_line
             )) %>%
      mutate(ydstogo = coalesce(ydstogo, yardline_100)) %>%
      mutate(description = str_extract(description, "(?<=\\) )(.+)")) %>%
      group_by(game_id, qtr > 2) %>%
      mutate(timeout = str_detect(description, "Timeout"),
             timeout_team = str_extract(description, "(?<=\\bby\\s)\\w+"),
             home_timeout = timeout & timeout_team == home_team,
             home_timeouts_remaining = 3 - cumsum(coalesce(home_timeout, 0)),
             away_timeout = timeout & timeout_team == away_team,
             away_timeouts_remaining = 3 - cumsum(coalesce(away_timeout, 0))) %>%
      ungroup() %>%
      select(season, datetime, week, game_id, away_team, home_team, away_team_score, home_team_score, drive_idx, play_idx,
             qtr, minute, second, quarter_seconds_remaining, 
             possession, field_side, yard_line, yardline_100, down, ydstogo, 
             home_timeouts_remaining,
             away_timeouts_remaining,
             description, context, timeout) %>%
      mutate(score_differential = ifelse(possession == home_team, home_team_score - away_team_score,
                                         away_team_score - home_team_score),
             home_opening_kickoff = as.numeric(home_team == first(possession)),
             posteam_timeouts_remaining = ifelse(possession == home_team, home_timeouts_remaining, away_timeouts_remaining),
             defteam_timeouts_remaining = ifelse(possession != home_team, home_timeouts_remaining, away_timeouts_remaining),
             
             go = !str_detect(description, "Field Goal|field goal|punt|Punt"))
  }) %>%
  bind_rows()


pbp_4th <- pbp %>%
  filter(down == 4) %>%
  filter(home_timeouts_remaining >= 0, away_timeouts_remaining >= 0, !timeout)

ids <- nflfastR::fast_scraper_schedules(2023) %>%
  filter(game_type == "REG") %>%
  transmute(home_team, away_team, type = "reg", season = 2023)

# turn on for initial fit, off otherwise
# takes about a minute per game (could be parallelized)
fit <- FALSE

if(fit){
  
  pbp_4th_fitted <- pbp_4th %>%
    group_split(game_id) %>%
    map(~{
      
      print(paste0(Sys.time(), ": ", first(.x$game_id)))
      
      a <- .x %>%
        rename(season_actual = season,
               home_team_actual = home_team,
               away_team_actual = away_team) 
      
      b <- a %>%
        crossing(ids) %>%
        mutate(posteam = ifelse(possession == home_team_actual, home_team, away_team)) %>%
        nfl4th::add_4th_probs()
      
      c <- b %>%
        group_by_at(names(a)) %>%
        summarize_if(is.numeric, mean) %>%
        ungroup()
      
    }) %>%
    bind_rows()
  
  save(pbp_4th_fitted, file = "plays_fitted2.Rdata")
  
}

load("plays_fitted2.Rdata")

pbp_4th_fitted2 <- pbp_4th_fitted %>%
  mutate(go_boost = go_wp - (pmax(punt_wp, fg_wp, na.rm = T))) %>%
  mutate(should_go = go_boost > 0) %>%
  mutate(go_wpa_added = ifelse(go, go_boost, -go_boost)) %>%
  mutate(season = lubridate::year(datetime),
         game_id = as.character(game_id)) %>%
  select(season, everything())

pbp_4th_actual <- load_4th_pbp(2022:2023, fast = T) %>%
  filter(down == 4, !is.na(go), !is.na(go_wp), season_type == "REG") %>%
  mutate(go_boost = go_wp - (pmax(punt_wp, fg_wp, na.rm = T))) %>%
  mutate(should_go = go_boost > 0) %>%
  mutate(go_wpa_added = ifelse(go, go_boost, -go_boost),
         go = go/100)

# twitter example
pbp_4th_fitted2 %>% 
  filter(game_id == "401671896", drive_idx == 1) %>% 
  ungroup() %>% 
  transmute(time = "9:56 - 1st", context, score = score_differential,
            go_wpa = go_wp, fg_wpa = fg_wp, punt_wpa = punt_wp, go_boost, should_go) %>% 
  mutate_at(c('go_wpa', 'fg_wpa', 'punt_wpa', 'go_boost'), ~round(.x, 2)) %>% 
  #gather(type, val) %>%
  View()

combo_cols <- intersect(names(pbp_4th_fitted2), names(pbp_4th_actual))

pbp_combo <- bind_rows(
  pbp_4th_fitted2 %>% select(combo_cols) %>% mutate(type = "Preseason 2023-2024"),
  pbp_4th_actual %>% select(combo_cols) %>% mutate(type = "Regular Season 2022-2023")
)

pbp_combo %>%
  group_by(type) %>%
  summarize(mean(go))

pbp_combo %>%
  filter(should_go) %>%
  group_by(type) %>%
  summarize(mean(go))

pbp_combo %>%
  group_by(type) %>%
  summarize(mean(go_wpa_added))

# dims: 585, 390

pbp_combo %>%
  filter(abs(go_boost) < 0.1) %>%
  ggplot(aes(go_boost, col = type)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_density() +
  theme_bw()+
  theme(legend.position=c(.8,.8),
        legend.title=element_blank(),
        legend.box.background = element_rect(color = "black"))  +
  scale_x_continuous(labels = scales::label_percent(style_positive = c("plus")),
                     breaks = seq(-0.1, 0.1, by = 0.02)) +
  labs(x = "Win Prob Added by Going for It")

pbp_combo %>%
  group_by(type, go_boost = round(2*go_boost, 2)/2) %>%
  summarize(n = n(), pgo = mean(go)) %>%
  filter(abs(go_boost) <= 0.06) %>%
  ggplot(aes(go_boost, pgo, col = type)) +
  geom_point(size = 2) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("segment", x = -0.002, y = 0.75, xend = -0.02, yend = 0.75, 
           linewidth = 4, linejoin = "mitre",
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("segment", x = 0.002, y = 0.75, xend = 0.02, yend = 0.75, 
           linewidth = 4, linejoin = "mitre",
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text", x = -0.002, y = 0.7525, label = "Kick!", color = "white",
           hjust = 1.5, size = 3, fontface = "bold") +
  annotate("text", x = 0.023, y = 0.7525, label = "Go for it!", color = "white",
           hjust = 1.5, size = 3, fontface = "bold") +
  scale_x_continuous(labels = scales::label_percent(style_positive = c("plus")),
                     breaks = seq(-0.06, 0.06, by = 0.02)) +
  scale_y_continuous(labels = scales::label_percent(),
                     breaks = seq(0, 1, by = 0.2)) +
  labs(y = "4th Down Attempt Rate",
       x = "Win Prob Added by Going for It") +
  theme_bw() +
  theme(legend.position=c(.8,.17),
        legend.title=element_blank(),
        legend.box.background = element_rect(color = "black"))


