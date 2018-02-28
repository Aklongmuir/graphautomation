
################################################################################
#All code below here including xGmodel written by Matthew Barlowe @matt_barlowe#
#on twitter#####################################################################

library(magrittr)
library(ggplot2)
library(ggforce)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)

#function to create dummy variables to determine if
#event was committed by the home team
is_home <- function(dataframe){
    dataframe$is_home <- ifelse(dataframe$event_team ==
                                    dataframe$home_team, 1 , 0)
    return(dataframe)
}

games <- c(20001)
seasons <- c(2015)
shots <- c("SHOT", "GOAL")

        
pbp_df <- read_delim('~/HockeyStuff/xGGameBreakdowns/2016/20031/20031', delim = '|')

scoreadj_corsi <- data.frame(matrix(nrow = 7, ncol = 3))
scoreadj_corsi[, 1] <- c(1, 2, 3, 4, 5, 6, 7)
scoreadj_corsi[, 2] <- c(0.840, 0.865, 0.898, 0.970, 1.052, 1.104, 1.138)
scoreadj_corsi[, 3] <- c(1.236, 1.186, 1.128, 1.032, 0.953, 0.914, 0.892)
colnames(scoreadj_corsi) <- c("home_lead", "home_corsi_adj", "away_corsi_adj")



xG_adj_h <- 0.9468472
xG_adj_a <- 1.059477

#loads xGmodel
xGModel <-load('~/ProgrammingStuff/R/Rprojects/xGmodel/xGmodelver2.rda')
home_team <- first(pbp_df$home_team)
away_team <- first(pbp_df$away_team)

#vectors used to subset the pbp data depending on event types
corsi <- c('SHOT', 'MISS', 'BLOCK', 'GOAL')
fenwick <- c('SHOT', 'MISS', 'GOAL')

#game strength states vectors used to determine shooter strength state
even_strength <- c('5v5', '4v4', '3v3', 'EvE')
home_advantage <- c('5v4', '5v3', '4v3')
away_advantage <- c('4v5', '3v5', '3v4')

#removes shootout info from data frame and creates is_home dummy variable
pbp_df <- pbp_df[pbp_df$game_period < 5,]
pbp_df <- is_home(pbp_df)

pbp_df$event_player_1 <- ifelse(pbp_df$event_player_1 == 'SEBASTIAN.AHO' &
                                    pbp_df$event_team == 'NYI', '5EBASTIAN.AHO',
                                pbp_df$event_player_1)
pbp_df$event_player_2 <- ifelse(pbp_df$event_player_2 == 'SEBASTIAN.AHO' &
                                    pbp_df$event_team == 'NYI', '5EBASTIAN.AHO',
                                pbp_df$event_player_2)
pbp_df$event_player_3 <- ifelse(pbp_df$event_player_3 == 'SEBASTIAN.AHO' &
                                    pbp_df$event_team == 'NYI', '5EBASTIAN.AHO',
                                pbp_df$event_player_3)
pbp_df$away_on_1 <- ifelse(pbp_df$away_on_1 == 'SEBASTIAN.AHO' &
                               pbp_df$away_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$away_on_1)
pbp_df$away_on_2 <- ifelse(pbp_df$away_on_2 == 'SEBASTIAN.AHO' &
                               pbp_df$away_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$away_on_2)
pbp_df$away_on_3 <- ifelse(pbp_df$away_on_3 == 'SEBASTIAN.AHO' &
                               pbp_df$away_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$away_on_3)
pbp_df$away_on_4 <- ifelse(pbp_df$away_on_4 == 'SEBASTIAN.AHO' &
                               pbp_df$away_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$away_on_4)
pbp_df$away_on_5 <- ifelse(pbp_df$away_on_5 == 'SEBASTIAN.AHO' &
                               pbp_df$away_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$away_on_5)
pbp_df$away_on_6 <- ifelse(pbp_df$away_on_6 == 'SEBASTIAN.AHO' &
                               pbp_df$away_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$away_on_6)
pbp_df$home_on_1 <- ifelse(pbp_df$home_on_1 == 'SEBASTIAN.AHO' &
                               pbp_df$home_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$home_on_1)
pbp_df$home_on_2 <- ifelse(pbp_df$home_on_2 == 'SEBASTIAN.AHO' &
                               pbp_df$home_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$home_on_2)
pbp_df$home_on_3 <- ifelse(pbp_df$home_on_3 == 'SEBASTIAN.AHO' &
                               pbp_df$home_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$home_on_3)
pbp_df$home_on_4 <- ifelse(pbp_df$home_on_4 == 'SEBASTIAN.AHO' &
                               pbp_df$home_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$home_on_4)
pbp_df$home_on_5 <- ifelse(pbp_df$home_on_5 == 'SEBASTIAN.AHO' &
                               pbp_df$home_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$home_on_5)
pbp_df$home_on_6 <- ifelse(pbp_df$home_on_6 == 'SEBASTIAN.AHO' &
                               pbp_df$home_team == 'NYI', '5EBASTIAN.AHO',
                           pbp_df$home_on_6)



############################################################################
##Creating the model features including distance, angle, time diff between##
##events, is_rebound, is shot on the rush, and shooter strength state.    ##
############################################################################

#calculating the time difference between each event in the game by seconds
#this will be used later to calculate whether shot is rebound or on the rush
pbp_df <- pbp_df %>% mutate(time_diff = game_seconds - lag(game_seconds))

#creates the is_rebound dummy variable for each even and turns any NAs to zeros
#just in case so the model doesn't throw an error
pbp_df$is_rebound <- ifelse(pbp_df$time_diff < 3 &
                                pbp_df$event_type %in% corsi &
                                pbp_df$event_team ==
                                lag(pbp_df$event_team),
                            1, 0)
pbp_df$is_rebound[is.na(pbp_df$is_rebound)] <- 0

#creates is_rush variable just like is_rebound
pbp_df$is_rush <- ifelse(pbp_df$time_diff < 4 &
                             lag(abs(pbp_df$coords_x)) < 25 &
                             pbp_df$event_type %in% corsi,
                         1, 0)
pbp_df$is_rush[is.na(pbp_df$is_rush)] <- 0

#creating shot_angle, distance, and shot angle for shots taken below each goal
#line in the second calculation which requires a different trignometric strategy
#distance is calculated from the goal
pbp_df$shot_angle <- (asin(abs(pbp_df$coords_y)/sqrt((87.95
                                                      - abs(pbp_df$coords_x))^2
                                                     + pbp_df$coords_y^2))*180)/ 3.14

pbp_df$shot_angle <- ifelse(pbp_df$coords_x > 88 | pbp_df$coords_x < -88, 90 +
                                (180-(90 + pbp_df$shot_angle)), pbp_df$shot_angle)

#calculates distance from net using pythagorean theorem
pbp_df$distance <- sqrt((87.95 - abs(pbp_df$coords_x))^2 + pbp_df$coords_y^2)

#if statements to determine shooter strength state based on vectors declared
#at beginning of file
pbp_df$shooter_strength <- ifelse(pbp_df$home_skaters == pbp_df$away_skaters,
                                  'EV', NA)
pbp_df$shooter_strength <- ifelse(pbp_df$home_skaters > pbp_df$away_skaters
                                  & pbp_df$is_home == 1, 'PP', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$home_skaters < pbp_df$away_skaters
                                  & pbp_df$is_home == 1, 'SH', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$home_skaters > pbp_df$away_skaters
                                  & pbp_df$is_home == 0, 'SH', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$home_skaters < pbp_df$away_skaters
                                  & pbp_df$is_home == 0, 'PP', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in%
                                      c('Ev0', '0vE'),
                                  'PS', pbp_df$shooter_strength)

pbp_df$coords_x[is.na(pbp_df$coords_x)] <- 0
pbp_df$coords_y[is.na(pbp_df$coords_y)] <- 0
############################################################################
##Calculates xG values for each event from the fenwick events subset of   ##
## the pbp_df and then merges it back to the pbp_df.                      ##
############################################################################

#filters pbp dataframe to fenwick events and predict the xG of each event
fenwick_pbp<- filter(pbp_df, event_type %in% c("SHOT", "MISS", "GOAL"))
fenwick_pbp$xG <-predict(xGmodel, fenwick_pbp, type = 'response')

#merge the fenwick PbP xG values back with the main PbP dataframe and
#replacing any NAs with zeros
pbp_df <- merge(pbp_df, fenwick_pbp[, c('event_index', 'xG')],
                by = 'event_index', all.x = TRUE)

pbp_df <- pbp_df %>% replace_na(list(xG = 0))

#creates new columns that seperates xG into home and away values to calculate
#a running sum
pbp_df$home_xG <- ifelse(pbp_df$is_home == 1 &
                             pbp_df$event_type %in% fenwick, pbp_df$xG, 0)
pbp_df$away_xG <- ifelse(pbp_df$is_home == 0 &
                             pbp_df$event_type %in% fenwick, pbp_df$xG, 0)

#creating 5v5 xg values
pbp_df$home_5v5_xG <- ifelse(pbp_df$is_home == 1 &
                                 pbp_df$home_skaters == 5 &
                                 pbp_df$away_skaters == 5 &
                                 pbp_df$event_type %in%
                                 fenwick, pbp_df$xG, 0)

pbp_df$away_5v5_xG <- ifelse(pbp_df$is_home == 0 &
                                 pbp_df$home_skaters == 5 &
                                 pbp_df$away_skaters == 5 &
                                 pbp_df$event_type %in%
                                 fenwick, pbp_df$xG, 0)
############################################################################

############################################################################
##Calculate home and away corsi and fenwick and running values for each.  ##
############################################################################

#creating corsi and running corsi totals for each team
pbp_df$home_corsi <- ifelse(pbp_df$is_home == 1 &
                                pbp_df$event_type %in% corsi, 1, 0)
pbp_df$away_corsi <- ifelse(pbp_df$is_home == 0 &
                                pbp_df$event_type %in% corsi, 1, 0)
pbp_df <- pbp_df %>% mutate(home_corsi_total = cumsum(home_corsi))
pbp_df <- pbp_df %>% mutate(away_corsi_total = cumsum(away_corsi))

pbp_df$home_goal <- ifelse(pbp_df$is_home == 1 & pbp_df$event_type == "GOAL",
                           1, 0)

pbp_df$away_goal <- ifelse(pbp_df$is_home == 0 & pbp_df$event_type == "GOAL",
                           1, 0)
#creating fenwick running fenwick totals for each team
pbp_df$home_fenwick <- ifelse(pbp_df$is_home == 1 &
                                  pbp_df$event_type %in% fenwick, 1, 0)
pbp_df$away_fenwick <- ifelse(pbp_df$is_home == 0 &
                                  pbp_df$event_type %in% fenwick, 1, 0)
pbp_df <- pbp_df %>% mutate(home_fenwick_total = cumsum(home_fenwick))
pbp_df <- pbp_df %>% mutate(away_fenwick_total = cumsum(away_fenwick))

############################################################################

############################################################################
##Calculates score/venue adjusted corsi for both the home and away team   ##
##from the adjustment dataframe and adjusted xG.                          ##
############################################################################

#create score differential from home persepective and add four so it matches
#the column in my corsi adjustment dataframe
pbp_df$score_diff <- pbp_df$home_score - pbp_df$away_score
pbp_df$score_diff <- ifelse(pbp_df$score_diff > 3, 3, pbp_df$score_diff)
pbp_df$score_diff <- ifelse(pbp_df$score_diff < -3, -3, pbp_df$score_diff)
pbp_df$score_diff <- pbp_df$score_diff + 4

#creates adjusted corsi columns by joining the score differential with the
#adjusted dataframe to create home and away adjustment vectors to multiple
#the home and away corsi columns by
score_df <- data.frame(pbp_df$score_diff)
score_df <- inner_join(score_df, scoreadj_corsi,
                       by = c("pbp_df.score_diff"="home_lead"))
pbp_df$home_corsi_adj <- pbp_df$home_corsi * score_df$home_corsi_adj
pbp_df$away_corsi_adj <- pbp_df$away_corsi * score_df$away_corsi_adj

pbp_df$home_fenwick_adj <- pbp_df$home_fenwick * score_df$home_corsi_adj
pbp_df$away_fenwick_adj <- pbp_df$away_fenwick * score_df$away_corsi_adj

pbp_df$home_xG_adj <- pbp_df$home_xG * xG_adj_h
pbp_df$away_xG_adj <- pbp_df$away_xG * xG_adj_a

#calculates the running sum for the step graphs for all situations and 5v5
pbp_df <- mutate(pbp_df, run_home_xg = cumsum(home_xG))
pbp_df <- mutate(pbp_df, run_away_xg = cumsum(away_xG))
pbp_df <- mutate(pbp_df, run_home_5v5_xg = cumsum(home_5v5_xG))
pbp_df <- mutate(pbp_df, run_away_5v5_xg = cumsum(away_5v5_xG))


#create unique key for each row
pbp_df$db_key <- paste0(as.character(pbp_df$game_id),
                        as.character(pbp_df$event_index))

#create LD, MD, and HD dummy variables
pbp_df$HD <- ifelse(abs(pbp_df$coords_x)<87.95 & 
                        abs(pbp_df$coords_x) > 67.95 &
                        pbp_df$coords_y<8 & pbp_df$coords_y > -8, 1, 0) 
pbp_df$MD <- ifelse(abs(pbp_df$coords_x < 87.95) & 
                    abs(pbp_df$coords_y > 67.95) & 
                    abs(pbp_df$coords_y) < (-0.7*abs(pbp_df$coords_x) 
                                            + 69.565) &
                        abs(pbp_df$coords_y) > 8, 
                    1, 0)
pbp_df$MD <- ifelse(abs(pbp_df$coords_x < 67.95) & 
                    abs(pbp_df$coords_x > 52.95) &
                    pbp_df$coords_y < 22 & pbp_df$coords_y > -22, 
                    1, pbp_df$MD)

pbp_df$MD <- ifelse(abs(pbp_df$coords_x > 25) & 
                        abs(pbp_df$coords_x < 52.95) &
                        pbp_df$coords_y < 8 & pbp_df$coords_y > -8, 
                    1, pbp_df$MD)

pbp_df$LD <- ifelse(pbp_df$MD == 0 & pbp_df$HD == 0, 1, 0)


