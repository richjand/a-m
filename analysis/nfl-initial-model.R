library(rstan)
library(tidyverse)
library(stringr)
library(lme4)
library(lubridate)
library(glmnet)
library(purrr)
source('helper-functions.R')

d <- prep_data()
m <- stan_model('futures-game-model.stan')

x <- data.matrix(data.frame(d$div_game,
                            d$future_diff, 
                            d$home_days_between_games - d$away_days_between_games))
y <- d$score_diff

standat <- list(J = nrow(d),
                K = ncol(x),
                y = y,
                x = x)

fit <- sampling(m, data = standat, chains = 4, iter = 4000, cores = 2)

##extract samples
samps_score_diff <- extract(fit)$y_rep
samps_win <- extract(fit)$home_win

##View simulated wins for a particular team/season
hist_team_season('ne', 2017, samples = samps_win, gamedat = d)



d$home_win_pred <- apply(samps_score_diff, 2, function(x) mean(x > 0))

wins <- d %>%
  filter(season_type == 'REG') %>%
  group_by(away_team, year) %>%
  summarise(away_wins_pred = sum(1-home_win_pred),
            away_wins_actual = sum(if_else(away_team_final_score > home_team_final_score, 1, 0))
            ) %>%
  inner_join(
    d %>%
      group_by(home_team, year) %>%
      summarise(home_wins_pred = sum(home_win_pred),
                home_wins_actual = sum(if_else(home_team_final_score > away_team_final_score, 1, 0)),
                vegas_wins = mean(home_team_win_total)
                )
    ,by = c('away_team' = 'home_team', 'year' = 'year')
  ) %>%
  mutate(total_wins_pred = away_wins_pred + home_wins_pred,
         total_wins_actual = away_wins_actual + home_wins_actual) %>%
  inner_join(vegas, by = c('away_team' = 'team_abbrev', 'year' = 'year'))

summary(lm(total_wins_actual ~ total_wins_pred + vegas_wins, data = wins))

wins$bet_over <- ifelse(wins$total_wins_pred > wins$vegas_wins, 1, 0)
wins$bet_under <- ifelse(wins$total_wins_pred < wins$vegas_wins, 1, 0)


wins$won_bet <- case_when(
  wins$total_wins_pred > wins$vegas_wins & wins$total_wins_actual > wins$vegas_wins ~ 1,
  wins$total_wins_pred > wins$vegas_wins & wins$total_wins_actual < wins$vegas_wins ~ 0,
  wins$total_wins_pred < wins$vegas_wins & wins$total_wins_actual < wins$vegas_wins ~ 1,
  wins$total_wins_pred < wins$vegas_wins & wins$total_wins_actual > wins$vegas_wins ~ 0
)

###look at biggest divergences between vegas and us##
wins %>%
  mutate(variance = vegas_wins - total_wins_pred) %>%
  dplyr::select(away_team, year, vegas_wins, total_wins_pred, total_wins_actual, variance, won_bet) %>%
  arrange(desc(abs(variance)))

##How often we would have won bet
mean(wins$won_bet, na.rm=T)

##Which teams we got/missed on a lot more often
wins %>%
  filter(!is.na(won_bet)) %>%
  group_by(away_team) %>%
  summarise(win_pct = sum(won_bet)/n()) %>%
  arrange(desc(win_pct))
