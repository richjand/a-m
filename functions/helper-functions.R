prep_data <- function(){
  scores <- readRDS('nfl-final-scores.RDS') %>%
    mutate(year = as.numeric(str_extract(game_id, "^.{4}")),
           away_team = tolower(away_team),
           home_team = tolower(home_team))
  
  vegas <- read_csv('nfl-preseason-futures.csv', na = '-') %>%
    dplyr::select(-starts_with('X')) %>%
    filter(team != '')
  
  vegas$team_abbrev <- vegas_2_fastr(vegas)
  
  
  ## get pre-game rest for each game ##
  teams <- unique(scores$away_team)
  
  restdat <- list()
  
  for (i in 1:length(teams)){
    games <- filter(scores, away_team == teams[i] | home_team == teams[i])
    games <- games %>% group_by(year) %>%
      mutate(previous_game = lag(game_date),
             days_between_games = as.numeric(game_date - previous_game, units = 'days'),
             day_of_week = wday(game_date, label = T)
      ) %>%
      ungroup()
    g <- games %>%
      mutate(days_between_games = coalesce(days_between_games,7),
             extra_day = if_else(days_between_games == 8, 1, 0),
             short_day = if_else(days_between_games == 6, 1, 0),
             extra_rest = if_else(days_between_games > 8, 1, 0),
             short_rest = if_else(days_between_games < 6, 1, 0)
      )
    restdat[[i]] <- dplyr::select(g, game_id, days_between_games, extra_day, short_day, extra_rest, short_rest) %>%
      mutate(team = teams[i])
  }
  
  rest <- bind_rows(restdat)
  
  
  ## There's a theory that teams from the west struggle when going east to play morning games. Let's test it!
  west_teams <- c('sea','ari','sf','la','lv','den','lac')
  
  df <- scores %>% 
    inner_join(dplyr::select(vegas, team_abbrev, win_total, year), by = c("home_team" = "team_abbrev", "year" = "year")) %>%
    rename(home_team_win_total = win_total) %>% 
    inner_join(dplyr::select(vegas, team_abbrev, win_total, year), by = c("away_team" = "team_abbrev", "year" = "year")) %>%
    rename(away_team_win_total = win_total) %>%
    filter(season_type == 'REG') %>%
    mutate(score_diff = home_team_final_score - away_team_final_score, ##Create DV
           future_diff = home_team_win_total - away_team_win_total, ##Create primary IV
           month = month(game_date, label = T))
  
  ##Define different types of pre-game rest for each team
  
  d <- df %>%
    left_join(rest, by = c('home_team' = 'team', 'game_id' = 'game_id')) %>%
    rename(home_days_between_games = days_between_games,
           home_extra_day = extra_day,
           home_short_day = short_day,
           home_short_rest = short_rest,
           home_extra_rest = extra_rest) %>%
    left_join(rest, by = c('away_team' = 'team', 'game_id' = 'game_id')) %>%
    rename(away_days_between_games = days_between_games,
           away_extra_day = extra_day,
           away_short_day = short_day,
           away_short_rest = short_rest,
           away_extra_rest = extra_rest) %>%
    mutate(west_to_east = if_else(away_team %in% west_teams & as.character(start_time) == '13:00:00',1,0))
  
  return(d)
}

vegas_2_fastr <- function(x){
  vec <- dplyr::case_when(
    x$team == 'Arizona Cardinals' ~ 'ari',
    x$team == 'Atlanta Falcons' ~ 'atl',
    x$team == 'Baltimore Ravens' ~ 'bal',
    x$team == 'Buffalo Bills' ~ 'buf',
    x$team == 'Carolina Panthers' ~ 'car',
    x$team == 'Chicago Bears' ~ 'chi',
    x$team == 'Cincinnati Bengals' ~ 'cin',
    x$team == 'Cleveland Browns' ~ 'cle',
    x$team == 'Dallas Cowboys' ~ 'dal',
    x$team == 'Denver Broncos' ~ 'den',
    x$team == 'Detroit Lions' ~ 'det',
    x$team == 'Green Bay Packers' ~ 'gb',
    x$team == 'Houston Texans' ~ 'hou',
    x$team == 'Indianapolis Colts' ~ 'ind',
    x$team == 'Jacksonville Jaguars' ~ 'jax',
    x$team == 'Kansas City Chiefs' ~ 'kc',
    x$team == 'Los Angeles Chargers' ~ 'lac',
    x$team == 'Los Angeles Rams' ~ 'la',
    x$team == 'Miami Dolphins' ~ 'mia',
    x$team == 'Minnesota Vikings' ~ 'min',
    x$team == 'New England Patriots' ~ 'ne',
    x$team == 'New Orleans Saints' ~ 'no',
    x$team == 'New York Giants' ~ 'nyg',
    x$team == 'New York Jets' ~ 'nyj',
    x$team == 'Oakland Raiders' ~ 'lv',
    x$team == 'Las Vegas Raiders' ~ 'lv',
    x$team == 'Philadelphia Eagles' ~ 'phi',
    x$team == 'Pittsburgh Steelers' ~ 'pit',
    x$team == 'San Diego Chargers' ~ 'lac',
    x$team == 'San Francisco 49ers' ~ 'sf',
    x$team == 'Seattle Seahawks' ~ 'sea',
    x$team == 'St Louis Rams' ~ 'la',
    x$team == 'Tampa Bay Buccaneers' ~ 'tb',
    x$team == 'Tennessee Titans' ~ 'ten',
    x$team == 'Washington Redskins' ~ 'was'
  )
  
  return(vec)
}

hist_team_season <- function(team, year, samples, gamedat){
  vegas_wins <- gamedat[gamedat$away_team == team & gamedat$year == year,]$away_team_win_total[1]
  home_games <- which(gamedat$home_team == team & gamedat$year == year)
  away_games <- which(gamedat$away_team == team & gamedat$year == year)
  home_wins <- apply(samps[,home_games], 1, sum)
  away_wins <- apply(samps[,away_games], 1, function(x) sum(1-x))
  hist(home_wins + away_wins,
       xlab = 'Number of Wins',
       main = stringr::str_c('Simuated Wins for ', team, ', ', year))
  abline(v = vegas_wins, col = 'red')
  abline(v = mean(home_wins + away_wins))
}
