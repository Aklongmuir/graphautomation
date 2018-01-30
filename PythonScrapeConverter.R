library(readr)
library(dplyr)
library(stringr)

file_name <- '~/HockeyStuff/xGGameBreakdowns/2018/20394/20394.csv'

python_pbp_df <- read_delim(file_name, delim = ',')

python_pbp_df <- python_pbp_df %>% rename(event_index = X1, game_id = Game_Id,
                                          game_date = Date, game_period = Period,
                                          event_type = Event, event_description = Description,
                                          game_seconds = Seconds_Elapsed, event_detail = Type,
                                          event_team = Ev_Team, home_team = Home_Team,
                                          away_team = Away_Team, event_player_1 = p1_name,
                                          event_player_2 = p2_name, event_player_3 = p3_name,
                                          away_on_1 = awayPlayer1, away_on_2 = awayPlayer2,
                                          away_on_3 = awayPlayer3, away_on_4 = awayPlayer4,
                                          away_on_4 = awayPlayer4, away_on_5 = awayPlayer5,
                                          away_on_6 = awayPlayer6,
                                          home_on_1 = homePlayer1, home_on_2 = homePlayer2,
                                          home_on_3 = homePlayer3, home_on_4 = homePlayer4,
                                          home_on_4 = homePlayer4, home_on_5 = homePlayer5,
                                          home_on_6 = homePlayer6,
                                          away_skaters = Away_Players, home_skaters = Home_Players,
                                          away_score = Away_Score, home_score = Home_Score,
                                          away_goalie = Away_Goalie, home_goalie = Home_Goalie,
                                          coords_x = xC, coords_y = yC, 
                                          game_strength_state = Strength)

drops <- c('Time_Elapsed', 'Ev_Zone', 'Home_Zone', 'p1_ID', 'p2_ID', 'p3_ID', 
           'awayPlayer1_id', 'awayPlayer2_id', 'awayPlayer3_id', 'awayPlayer4_id',
           'awayPlayer5_id', 'awayPlayer6_id', 'homePlayer1_id', 'homePlayer2_id',
           'homePlayer3_id', 'homePlayer4_id', 'homePlayer5_id', 'homePlayer6_id',
           'Away_Goalie_Id', 'Home_Goalie_Id', 'Home_Coach', 'Away_Coach', 'yC')

python_pbp_df <- python_pbp_df[,!(names(python_pbp_df) %in% drops)]
                                          
python_pbp_df$season <- '20172018'
python_pbp_df$session <- 'R'
python_pbp_df$game_score_state <- paste0(python_pbp_df$away_score, 'v', python_pbp_df$home_score)
python_pbp_df$highlight_code <- 'NA'
python_pbp_df$away_skaters <- python_pbp_df$away_skaters - 1
python_pbp_df$home_skaters <- python_pbp_df$home_skaters - 1
python_pbp_df$event_length <- python_pbp_df$game_seconds - lag(python_pbp_df$game_seconds)
python_pbp_df$players_substituted <- NA
python_pbp_df$game_id <- paste0('20170', python_pbp_df$game_id)

python_pbp_df$event_detail <- ifelse(python_pbp_df$event_detail == 'SNAP SHOT', 'Snap', 
                                     python_pbp_df$event_detail)
python_pbp_df$event_detail <- ifelse(python_pbp_df$event_detail == 'SLAP SHOT', 'Slap', 
                                     python_pbp_df$event_detail)

python_pbp_df$event_detail <- ifelse(python_pbp_df$event_detail == 'WRIST SHOT', 'Wrist', 
                                     python_pbp_df$event_detail)
python_pbp_df$event_detail <- ifelse(python_pbp_df$event_detail == 'BACKHAND', 'Backhand', 
                                     python_pbp_df$event_detail)
python_pbp_df$event_detail <- ifelse(python_pbp_df$event_detail == 'WRAP-AROUND', 'Wrap-around', 
                                     python_pbp_df$event_detail)
python_pbp_df$event_detail <- ifelse(python_pbp_df$event_detail == 'DEFLECTED', 'Deflected', 
                                     python_pbp_df$event_detail)
python_pbp_df$event_detail <- ifelse(python_pbp_df$event_detail == 'TIP-IN', 'Tip-In', 
                                     python_pbp_df$event_detail)

python_pbp_df <- python_pbp_df %>% select(event_index, season, game_id, game_date, session, 
                                          game_period, game_seconds,
                                          event_type, event_description, event_detail,
                                          event_team, event_player_1, event_player_2,
                                          event_player_3, event_length, coords_x,
                                          coords_y, players_substituted, home_on_1,
                                          home_on_2, home_on_3, home_on_4, home_on_5,
                                          home_on_6, away_on_1, away_on_2, away_on_3,
                                          away_on_4, away_on_5, away_on_6, home_goalie,
                                          away_goalie, home_team, away_team, home_skaters,
                                          away_skaters, home_score, away_score, game_score_state,
                                          game_strength_state, highlight_code)

python_pbp_df$event_player_1 <- str_replace_all(python_pbp_df$event_player_1, ' ', '.')
python_pbp_df$event_player_2<- str_replace_all(python_pbp_df$event_player_2, ' ', '.')
python_pbp_df$event_player_3<- str_replace_all(python_pbp_df$event_player_3, ' ', '.')
python_pbp_df$away_on_1 <- str_replace_all(python_pbp_df$away_on_1, ' ', '.')
python_pbp_df$away_on_2 <- str_replace_all(python_pbp_df$away_on_2, ' ', '.')
python_pbp_df$away_on_3 <- str_replace_all(python_pbp_df$away_on_3, ' ', '.')
python_pbp_df$away_on_4 <- str_replace_all(python_pbp_df$away_on_4, ' ', '.')
python_pbp_df$away_on_5 <- str_replace_all(python_pbp_df$away_on_5, ' ', '.')
python_pbp_df$away_on_6 <- str_replace_all(python_pbp_df$away_on_6, ' ', '.')
python_pbp_df$home_on_1 <- str_replace_all(python_pbp_df$home_on_1, ' ', '.')
python_pbp_df$home_on_2 <- str_replace_all(python_pbp_df$home_on_2, ' ', '.')
python_pbp_df$home_on_3 <- str_replace_all(python_pbp_df$home_on_3, ' ', '.')
python_pbp_df$home_on_4 <- str_replace_all(python_pbp_df$home_on_4, ' ', '.')
python_pbp_df$home_on_5 <- str_replace_all(python_pbp_df$home_on_5, ' ', '.')
python_pbp_df$home_on_6 <- str_replace_all(python_pbp_df$home_on_6, ' ', '.')
python_pbp_df$away_goalie  <- str_replace_all(python_pbp_df$away_goalie, ' ', '.')
python_pbp_df$home_goalie <- str_replace_all(python_pbp_df$home_goalie, ' ', '.')

period1 <- subset(python_pbp_df$game_seconds, python_pbp_df$game_period == 1)
period2 <- subset(python_pbp_df$game_seconds, python_pbp_df$game_period == 2) + 1200
period3 <- subset(python_pbp_df$game_seconds, python_pbp_df$game_period == 3) + 2400

gameseconds <- c(period1, period2, period3)
python_pbp_df$game_seconds <- gameseconds
python_pbp_df <- replace_na(python_pbp_df, list(event_length = 0))
python_pbp_df$event_length <- ifelse(python_pbp_df$event_length == -1200, 0, python_pbp_df$event_length)
write_delim(python_pbp_df, '~/HockeyStuff/xGGameBreakdowns/2018/20394/20394', delim = '|')
#columns to drop: Time_Elapsed Ev_Zone Home_Zone p1_ID p2_ID p3_ID awayplayer1_id
#Away_Goalie_Id, Home_Goalie_Id Home_Coach Away_Coach