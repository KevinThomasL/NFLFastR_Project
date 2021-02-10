install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggimage")
install.packages("nflfastR")
install.packages("scales")
install.packages("dplyr")

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)

options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

pbp_rp <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa))

pbp_rp %>%
  filter(posteam == "SEA", rush == 1) %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 20)

pbp_rp %>%
  filter(posteam == "SEA", down <=4, play_type == 'run') %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
  ) %>%
  filter(plays > 20)

pbp_rp %>%
  mutate(
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  select(posteam, home_team, home) %>%
  head(10)

pbp_rp %>%
  mutate(
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  group_by(home) %>%
  summarize(epa = mean(epa))

pbp_rp %>%
  filter(!is.na(cp)) %>%
  mutate(
    depth = case_when(
      air_yards < 0 ~ "Negative",
      air_yards >= 0 & air_yards < 10 ~ "Short",
      air_yards >= 10 & air_yards < 20 ~ "Medium",
      air_yards >= 20 ~ "Deep"
    )
  ) %>%
  group_by(depth) %>%
  summarize(cp = mean(cp))

schotty <- pbp_rp %>%
  filter(wp > .20 & wp < .80 & down <= 2 & qtr <= 2 & half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(mean_pass = mean(pass), plays = n()) %>%
  arrange(-mean_pass)
schotty

ggplot(schotty, aes(x=reorder(posteam,-mean_pass), y=mean_pass)) +
  geom_text(aes(label=posteam))

seasons <- 2015:2019
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

pbp %>%
  group_by(season) %>%
  summarize(n = n())

pbp %>%
  group_by(play_type) %>%
  summarize(n = n())

games <- readRDS(url("http://www.habitatring.com/games.rds"))
str(games)

home <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team)
home %>% head(5)

away <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>%
  mutate(result = -result)
away %>% head(5)

results <- bind_rows(home, away) %>%
  arrange(week) %>%
  mutate(
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5
    )
  )

results %>% filter(season == 2019 & team == 'SEA')

team_wins <- results %>%
  group_by(team, season) %>%
  summarize(
    wins = sum(win),
    point_diff = sum(result)) %>%
  ungroup()

team_wins %>%
  arrange(-wins) %>%
  head(5)

seasons <- 1999:2019
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  ) %>%
    filter(rush == 1 | pass == 1, week <= 17, !is.na(epa), !is.na(posteam), posteam != "") %>%
    select(season, posteam, pass, defteam, epa)
})

pbp %>%
  group_by(posteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  head(4)

pbp %>%
  group_by(posteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  head(4)

offense <- pbp %>%
  group_by(posteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(off_pass_epa = `1`, off_rush_epa = `0`)

defense <- pbp %>%
  group_by(defteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(def_pass_epa = `1`, def_rush_epa = `0`)

offense %>%
  arrange(-off_pass_epa) %>%
  head(5)

defense %>%
  arrange(def_pass_epa) %>%
  head(5)

team_wins %>%
  group_by(team) %>%
  summarize(n=n()) %>%
  arrange(n)

team_wins <- team_wins %>%
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )

data <- team_wins %>%
  left_join(offense, by = c('team' = 'posteam', 'season')) %>%
  left_join(defense, by = c('team' = 'defteam', 'season'))

data %>%
  filter(team == 'SEA' & season >= 2012)

data <- data %>% 
  arrange(team, season) %>%
  mutate(
    prior_off_rush_epa = lag(off_rush_epa),
    prior_off_pass_epa = lag(off_pass_epa),
    prior_def_rush_epa = lag(def_rush_epa),
    prior_def_pass_epa = lag(def_pass_epa),
    prior_point_diff = lag(point_diff)
  )

data %>%
  head(5)

data %>% 
  select(-team, -season) %>%
  cor(use="complete.obs") %>%
  round(2)

message("2009 through 2019")
#> 2009 through 2019
data %>% 
  filter(season >= 2009) %>%
  select(wins, point_diff, off_pass_epa, off_rush_epa, prior_point_diff, prior_off_pass_epa, prior_off_rush_epa) %>%
  cor(use="complete.obs") %>%
  round(2)

message("1999 through 2008")
#> 1999 through 2008
data %>% 
  filter(season < 2009) %>%
  select(wins, point_diff, off_pass_epa, off_rush_epa, prior_point_diff, prior_off_pass_epa, prior_off_rush_epa) %>%
  cor(use="complete.obs") %>%
  round(2)

data <- data %>% filter(season >= 2009)

fit <- lm(wins ~ prior_off_pass_epa  + prior_off_rush_epa + prior_def_pass_epa + prior_def_rush_epa, data = data)

summary(fit)

fit2 <- lm(wins ~ prior_point_diff, data = data)

summary(fit2)

preds <- predict(fit, data %>% filter(season == 2020)) %>%
  #was just a vector, need a tibble to bind
  as_tibble() %>%
  #make the column name make sense
  rename(prediction = value) %>%
  round(1) %>%
  #get names
  bind_cols(
    data %>% filter(season == 2020) %>% select(team)
  )

preds %>%
  arrange(-prediction) %>%
  head(5)

preds2 <- predict(fit2, data %>% filter(season == 2020)) %>%
  #was just a vector, need a tibble to bind
  as_tibble() %>%
  #make the column name make sense
  rename(prediction = value) %>%
  round(1) %>%
  #get names
  bind_cols(
    data %>% filter(season == 2020) %>% select(team)
  )

preds2 %>%
  arrange(-prediction) %>%
  head(5)