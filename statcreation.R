
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

#Loops through game numbers in the daily_games vector and scrapes the data
#for the game with that game id
for (season in seasons){
    for(game_number in games){
        
        pbp_df <- read_delim(paste0('~/HockeyStuff/xGGameBreakdowns/',season, 
                                    '/', game_number, '/',
                                    game_number), delim = '|')
    
        print(game_number)
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
    
    
        ############################################################################
        ##Calculate home and away goals and assists stats and store in a dataframe##
        ##to join later with the other individual stats player dataframe such as  ##
        #ixG                                                                      ##
        ############################################################################
    
        #creates xG leaders for Each team
        fenwick_pbp <- filter(pbp_df, event_type %in% c("SHOT", "MISS", "GOAL"))
        home_adj_groupd_player_xg <- subset(fenwick_pbp,
                                       fenwick_pbp$event_team == home_team) %>%
            group_by(event_player_1) %>%
            dplyr::summarise(ixG = sum(home_xG_adj)) %>%
            rename(player = event_player_1)
    
        away_adj_groupd_player_xg <- subset(fenwick_pbp,
                                            fenwick_pbp$event_team == away_team) %>%
            group_by(event_player_1) %>%
            dplyr::summarise(ixG = sum(away_xG_adj)) %>%
            rename(player = event_player_1)
    
        home_groupd_player_xg <- subset(fenwick_pbp,
                                            fenwick_pbp$event_team == home_team) %>%
            group_by(event_player_1) %>%
            dplyr::summarise(ixG = sum(home_xG)) %>%
            rename(player = event_player_1)
    
        away_groupd_player_xg <- subset(fenwick_pbp,
                                            fenwick_pbp$event_team == away_team) %>%
            group_by(event_player_1) %>%
            dplyr::summarise(ixG = sum(away_xG)) %>%
            rename(player = event_player_1)
    
    
        individual_home_goals <- subset(pbp_df, pbp_df$event_team == home_team
                                        & pbp_df$event_type == "GOAL") %>%
            group_by(event_player_1) %>%
            summarise(G = sum(home_goal)) %>% rename(player = event_player_1)
    
        individual_home_a1 <- subset(pbp_df, pbp_df$event_team == home_team
                                     & pbp_df$event_type == "GOAL") %>%
            group_by(event_player_2) %>% summarise(A1 = sum(home_goal)) %>%
            rename(player = event_player_2)
    
        individual_home_a2 <- subset(pbp_df, pbp_df$event_team == home_team
                                     & pbp_df$event_type == "GOAL") %>%
            group_by(event_player_3) %>% summarise(A2 = sum(home_goal)) %>%
            rename(player = event_player_3)
    
        home_individual_stats <- full_join(individual_home_goals, individual_home_a1,
                                           by = c("player")) %>%
                                    full_join(individual_home_a2, by = c("player"))
    
        individual_away_goals <- subset(pbp_df, pbp_df$event_team == away_team
                                        & pbp_df$event_type == "GOAL") %>%
            group_by(event_player_1) %>%
            summarise(G = sum(away_goal)) %>% rename(player = event_player_1)
    
        individual_away_a1 <- subset(pbp_df, pbp_df$event_team == away_team
                                     & pbp_df$event_type == "GOAL") %>%
            group_by(event_player_2) %>% summarise(A1 = sum(away_goal)) %>%
            rename(player = event_player_2)
    
        individual_away_a2 <- subset(pbp_df, pbp_df$event_team == away_team
                                     & pbp_df$event_type == "GOAL") %>%
            group_by(event_player_3) %>% summarise(A2 = sum(away_goal)) %>%
            rename(player = event_player_3)
    
        away_individual_stats <- full_join(individual_away_goals,
                                           individual_away_a1,cby = c("player")) %>%
            full_join(individual_away_a2, by = c("player"))
    
        #Create goal and assist stats for 5v5 play
        fenwick_pbp_5v5 <- subset(fenwick_pbp, fenwick_pbp$home_skaters == 5 &
                                  fenwick_pbp$away_skaters == 5)
    
        pbp_df_5v5 <- subset(pbp_df, pbp_df$home_skaters == 5 &
                                 pbp_df$away_skaters == 5)
    
        home_adj_groupd_player_xg_5v5 <- subset(fenwick_pbp_5v5,
                                            fenwick_pbp_5v5$event_team == home_team) %>%
            group_by(event_player_1) %>%
            dplyr::summarise(ixG = sum(home_xG_adj)) %>%
            rename(player = event_player_1)
    
        away_adj_groupd_player_xg_5v5 <- subset(fenwick_pbp_5v5,
                                            fenwick_pbp_5v5$event_team == away_team) %>%
            group_by(event_player_1) %>%
            dplyr::summarise(ixG = sum(away_xG_adj)) %>%
            rename(player = event_player_1)
    
        home_groupd_player_xg_5v5 <- subset(fenwick_pbp_5v5,
                                                fenwick_pbp_5v5$event_team == home_team) %>%
            group_by(event_player_1) %>%
            dplyr::summarise(ixG = sum(home_xG)) %>%
            rename(player = event_player_1)
    
        away_groupd_player_xg_5v5 <- subset(fenwick_pbp_5v5,
                                                fenwick_pbp_5v5$event_team == away_team) %>%
            group_by(event_player_1) %>%
            dplyr::summarise(ixG = sum(away_xG)) %>%
            rename(player = event_player_1)
    
        individual_home_goals_5v5 <- subset(pbp_df_5v5, pbp_df_5v5$event_team == home_team
                                        & pbp_df_5v5$event_type == "GOAL") %>%
            group_by(event_player_1) %>%
            summarise(G = sum(home_goal)) %>% rename(player = event_player_1)
    
        individual_home_a1_5v5 <- subset(pbp_df_5v5, pbp_df_5v5$event_team == home_team
                                     & pbp_df_5v5$event_type == "GOAL") %>%
            group_by(event_player_2) %>% summarise(A1 = sum(home_goal)) %>%
            rename(player = event_player_2)
    
        individual_home_a2_5v5 <- subset(pbp_df_5v5, pbp_df_5v5$event_team == home_team
                                     & pbp_df_5v5$event_type == "GOAL") %>%
            group_by(event_player_3) %>% summarise(A2 = sum(home_goal)) %>%
            rename(player = event_player_3)
    
        home_individual_stats_5v5 <- full_join(individual_home_goals_5v5,
                                               individual_home_a1_5v5,
                                           by = c("player")) %>%
            full_join(individual_home_a2_5v5, by = c("player"))
    
        individual_away_goals_5v5 <- subset(pbp_df_5v5, pbp_df_5v5$event_team == away_team
                                        & pbp_df_5v5$event_type == "GOAL") %>%
            group_by(event_player_1) %>%
            summarise(G = sum(away_goal)) %>% rename(player = event_player_1)
    
        individual_away_a1_5v5 <- subset(pbp_df_5v5, pbp_df_5v5$event_team == away_team
                                     & pbp_df_5v5$event_type == "GOAL") %>%
            group_by(event_player_2) %>% summarise(A1 = sum(away_goal)) %>%
            rename(player = event_player_2)
    
        individual_away_a2_5v5 <- subset(pbp_df_5v5, pbp_df_5v5$event_team == away_team
                                     & pbp_df_5v5$event_type == "GOAL") %>%
            group_by(event_player_3) %>% summarise(A2 = sum(away_goal)) %>%
            rename(player = event_player_3)
    
        away_individual_stats_5v5 <- full_join(individual_away_goals_5v5,
                                           individual_away_a1_5v5,cby = c("player")) %>%
            full_join(individual_away_a2_5v5, by = c("player"))
    
        #clear NAs for goals that don't have an assist or second assist
        home_individual_stats <-
            home_individual_stats[!is.na(home_individual_stats$player),]
        home_individual_stats_5v5 <-
            home_individual_stats_5v5[!is.na(home_individual_stats_5v5$player),]
        away_individual_stats <-
            away_individual_stats[!is.na(away_individual_stats$player),]
        away_individual_stats_5v5 <-
            away_individual_stats_5v5[!is.na(away_individual_stats_5v5$player),]
        ############################################################################
        ##Creating all situations dataframe for adjusted corsi, fenwick, xG also  ##
        ##add in goals for as well although that isn't adjusted                   ##
        ############################################################################
        
        home1_all_sits_adj <- pbp_df %>% group_by(home_on_1) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_1)
        
        home2_all_sits_adj <- pbp_df %>% group_by(home_on_2) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_2)
        
        home3_all_sits_adj <- pbp_df %>% group_by(home_on_3) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_3)
        
        home3_all_sits_adj <- pbp_df %>% group_by(home_on_3) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_3)
        
        home4_all_sits_adj <- pbp_df %>% group_by(home_on_4) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_4)
        
        home5_all_sits_adj <- pbp_df %>% group_by(home_on_5) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_5)
        
        home6_all_sits_adj <- pbp_df %>% group_by(home_on_6) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_6)
        
        home_all_sits_adj <- bind_rows(home1_all_sits_adj, home2_all_sits_adj,
                                       home3_all_sits_adj, home4_all_sits_adj,
                                       home5_all_sits_adj, home6_all_sits_adj)
        
        home_all_sits_adj$team <- first(pbp_df$home_team)
        
        home_all_sits_adj <- home_all_sits_adj %>% group_by(player, team) %>%
            summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA),
                      C_plus_minus = sum(C_plus_minus),
                      FF = sum(FF), FA = sum(FA),
                      xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA))
        
        #join rate stats with goals and assists and ixG states and calculate per 60
        #stats for each one
        
        home_all_sits_adj <- full_join(home_all_sits_adj, home_individual_stats,
                                       by = c("player")) %>%
            full_join(home_adj_groupd_player_xg, by = c("player"))
        home_all_sits_adj <- replace_na(home_all_sits_adj,
                                        list(G = 0, A1 = 0, A2 = 0, ixG = 0))
        
        home_all_sits_adj <- home_all_sits_adj %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            G60 = (G/TOI) * 60,
            A160 = (A1/TOI) * 60,
            A260 = (A2/TOI) * 60,
            P60 = ((G + A1 + A2)/TOI) * 60,
            ixG60 = (ixG/TOI) * 60
        )
        
        #add date and game id to the stats
        home_all_sits_adj$game_id <- first(pbp_df$game_id)
        home_all_sits_adj$game_date <- first(pbp_df$game_date)
        
        #Repeating the process above but this time for the away team
        
        away1_all_sits_adj <- pbp_df %>% group_by(away_on_1) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_1)
        
        away2_all_sits_adj <- pbp_df %>% group_by(away_on_2) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_2)
        
        away3_all_sits_adj <- pbp_df %>% group_by(away_on_3) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_3)
        
        away4_all_sits_adj <- pbp_df %>% group_by(away_on_4) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_4)
        
        away5_all_sits_adj <- pbp_df %>% group_by(away_on_5) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_5)
        
        away6_all_sits_adj <- pbp_df %>% group_by(away_on_6) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_6)
        
        away_all_sits_adj <- bind_rows(away1_all_sits_adj, away2_all_sits_adj,
                                       away3_all_sits_adj, away4_all_sits_adj,
                                       away5_all_sits_adj, away6_all_sits_adj)
        
        away_all_sits_adj$team <- first(pbp_df$away_team)
        
        away_all_sits_adj <- away_all_sits_adj %>% group_by(player, team) %>%
            summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA), C_plus_minus = sum(C_plus_minus),
                      FF = sum(FF), FA = sum(FA),
                      xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA))
        
        #join rate stats with goals and assists and ixG states and calculate per 60
        #stats for each one
        
        away_all_sits_adj <- full_join(away_all_sits_adj, away_individual_stats,
                                       by = c("player")) %>%
            full_join(away_adj_groupd_player_xg, by = c("player"))
        away_all_sits_adj <- replace_na(away_all_sits_adj,
                                        list(G = 0, A1 = 0, A2 = 0, ixG = 0))
        
        away_all_sits_adj <- away_all_sits_adj %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            G60 = (G/TOI) * 60,
            A160 = (A1/TOI) * 60,
            A260 = (A2/TOI) * 60,
            P60 = ((G + A1 + A2)/TOI) * 60,
            ixG60 = (ixG/TOI) * 60
        )
        
        #add date and game id to the stats
        away_all_sits_adj$game_id <- first(pbp_df$game_id)
        away_all_sits_adj$game_date <- first(pbp_df$game_date)
        
        player_all_sits_adj <- rbind(away_all_sits_adj, home_all_sits_adj)
        player_all_sits_adj$season <- first(pbp_df$season)
        player_all_sits_adj$session <- first(pbp_df$session)
        
        ############################################################################
        ############################################################################
        
        ############################################################################
        ##Create total team adjusted statistics all situations.                   ##
        ############################################################################
        
        #compile stats for home team
        home_team_adj_stats_all_sits <- pbp_df %>%
            summarise(Team = first(pbp_df$home_team), game_id = first(pbp_df$game_id),
                      game_date = first(game_date),
                      TOI = last(game_seconds)/60, CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = CF - CA,
                      FF = sum(home_fenwick_adj), FA = sum(away_fenwick_adj),
                      xGF = sum(home_xG_adj), xGA = sum(away_xG_adj),
                      GF = sum(home_goal), GA = sum(away_goal))
        
        #compile stats for away team
        away_team_adj_stats_all_sits <- pbp_df %>%
            summarise(Team = first(pbp_df$away_team), game_id = first(pbp_df$game_id),
                      game_date = first(game_date),
                      TOI = last(game_seconds)/60, CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = CF - CA,
                      FF = sum(away_fenwick_adj), FA = sum(home_fenwick_adj),
                      xGF = sum(away_xG_adj), xGA = sum(home_xG_adj),
                      GF = sum(away_goal), GA = sum(home_goal))
        
        #combine home and away into one dataframe
        team_adj_stats_all_sits <- rbind(home_team_adj_stats_all_sits,
                                         away_team_adj_stats_all_sits)
        
        #calculate rate stats
        team_adj_stats_all_sits <- team_adj_stats_all_sits %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60
        )
        
        #add date and game id to the stats and denote which was home team
        team_adj_stats_all_sits$is_home <- ifelse(team_adj_stats_all_sits$Team ==
                                                      first(pbp_df$home_team), 1, 0)
        team_adj_stats_all_sits$season <- first(pbp_df$season)
        team_adj_stats_all_sits$session <- first(pbp_df$session)
        
        ############################################################################
        ############################################################################
        
        ############################################################################
        ##Create 5v5 stats for players home and away adjusted                     ##
        ############################################################################
        
        home1_adj_5v5 <- pbp_df_5v5 %>% group_by(home_on_1) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_1)
        
        home2_adj_5v5 <- pbp_df_5v5 %>% group_by(home_on_2) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_2)
        
        home3_adj_5v5 <- pbp_df_5v5 %>% group_by(home_on_3) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_3)
        
        home4_adj_5v5 <- pbp_df_5v5 %>% group_by(home_on_4) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_4)
        
        home5_adj_5v5 <- pbp_df_5v5 %>% group_by(home_on_5) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_5)
        
        home6_adj_5v5 <- pbp_df_5v5 %>% group_by(home_on_6) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG_adj),
                      xGA = sum(away_xG_adj), CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = (sum(home_corsi_adj) - sum(away_corsi_adj)),
                      FF = sum(home_fenwick_adj),
                      FA = sum(away_fenwick_adj), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_6)
        
        home_adj_5v5 <- bind_rows(home1_adj_5v5, home2_adj_5v5,
                                  home3_adj_5v5, home4_adj_5v5,
                                  home5_adj_5v5, home6_adj_5v5)
        
        home_adj_5v5$team <- first(pbp_df$home_team)
        
        home_adj_5v5 <- home_adj_5v5 %>% group_by(player, team) %>%
            summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA),
                      C_plus_minus = sum(C_plus_minus),
                      FF = sum(FF), FA = sum(FA),
                      xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA))
        
        #join rate stats with goals and assists and ixG states and calculate per 60
        #stats for each one
        
        home_adj_5v5 <- full_join(home_adj_5v5, home_individual_stats_5v5,
                                  by = c("player")) %>%
            full_join(home_adj_groupd_player_xg_5v5, by = c("player"))
        home_adj_5v5 <- replace_na(home_adj_5v5,
                                   list(G = 0, A1 = 0, A2 = 0, ixG = 0))
        
        home_adj_5v5 <- home_adj_5v5 %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            G60 = (G/TOI) * 60,
            A160 = (A1/TOI) * 60,
            A260 = (A2/TOI) * 60,
            P60 = ((G + A1 + A2)/TOI) * 60,
            ixG60 = (ixG/TOI) * 60
        )
        
        #add game id and date
        home_adj_5v5$game_id <- first(pbp_df$game_id)
        home_adj_5v5$game_date <- first(pbp_df$game_date)
        
        #5v5 adjusted away team
        
        away1_adj_5v5 <- pbp_df_5v5 %>% group_by(away_on_1) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_1)
        
        away2_adj_5v5 <- pbp_df_5v5 %>% group_by(away_on_2) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_2)
        
        away3_adj_5v5 <- pbp_df_5v5 %>% group_by(away_on_3) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_3)
        
        away4_adj_5v5 <- pbp_df_5v5 %>% group_by(away_on_4) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_4)
        
        away5_adj_5v5 <- pbp_df_5v5 %>% group_by(away_on_5) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_5)
        
        away6_adj_5v5 <- pbp_df_5v5 %>% group_by(away_on_6) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG_adj),
                      xGA = sum(home_xG_adj), CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = (sum(away_corsi_adj) - sum(home_corsi_adj)),
                      FF = sum(away_fenwick_adj),
                      FA = sum(home_fenwick_adj), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_6)
        
        
        away_adj_5v5 <- bind_rows(away1_adj_5v5, away2_adj_5v5,
                                  away3_adj_5v5, away4_adj_5v5,
                                  away5_adj_5v5, away6_adj_5v5)
        
        away_adj_5v5$team <- first(pbp_df$away_team)
        
        away_adj_5v5 <- away_adj_5v5 %>% group_by(player, team) %>%
            summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA),
                      C_plus_minus = sum(C_plus_minus),
                      FF = sum(FF), FA = sum(FA),
                      xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA))
        
        #join rate stats with goals and assists and ixG states and calculate per 60
        #stats for each one
        
        away_adj_5v5 <- full_join(away_adj_5v5, away_individual_stats_5v5,
                                  by = c("player")) %>%
            full_join(away_adj_groupd_player_xg_5v5, by = c("player"))
        away_adj_5v5 <- replace_na(away_adj_5v5,
                                   list(G = 0, A1 = 0, A2 = 0, ixG = 0))
        
        away_adj_5v5 <- away_adj_5v5 %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            G60 = (G/TOI) * 60,
            A160 = (A1/TOI) * 60,
            A260 = (A2/TOI) * 60,
            P60 = ((G + A1 + A2)/TOI) * 60,
            ixG60 = (ixG/TOI) * 60
        )
        
        #add game id and date
        away_adj_5v5$game_id <- first(pbp_df$game_id)
        away_adj_5v5$game_date <- first(pbp_df$game_date)
        
        player_5v5_adj <- rbind(away_adj_5v5, home_adj_5v5)
        player_5v5_adj$season <- first(pbp_df$season)
        player_5v5_adj$session <- first(pbp_df$session)
        
        ############################################################################
        ##Create total team adjusted statistics 5v5.                   ##
        ############################################################################
        
        #compile stats for home team
        home_team_adj_stats_5v5 <- pbp_df_5v5 %>%
            summarise(Team = first(pbp_df$home_team), game_id = first(pbp_df$game_id),
                      game_date = first(game_date),
                      TOI = sum(event_length)/60, CF = sum(home_corsi_adj),
                      CA = sum(away_corsi_adj),
                      C_plus_minus = CF - CA,
                      FF = sum(home_fenwick_adj), FA = sum(away_fenwick_adj),
                      xGF = sum(home_xG_adj), xGA = sum(away_xG_adj),
                      GF = sum(home_goal), GA = sum(away_goal))
        
        #compile stats for away team
        away_team_adj_stats_5v5 <- pbp_df_5v5 %>%
            summarise(Team = first(pbp_df$away_team), game_id = first(pbp_df$game_id),
                      game_date = first(game_date),
                      TOI = sum(event_length)/60, CF = sum(away_corsi_adj),
                      CA = sum(home_corsi_adj),
                      C_plus_minus = CF - CA,
                      FF = sum(away_fenwick_adj), FA = sum(home_fenwick_adj),
                      xGF = sum(away_xG_adj), xGA = sum(home_xG_adj),
                      GF = sum(away_goal), GA = sum(home_goal))
        
        #combine home and away into one dataframe
        team_adj_stats_5v5 <- rbind(home_team_adj_stats_5v5,
                                    away_team_adj_stats_5v5)
        
        #calculate rate stats
        team_adj_stats_5v5 <- team_adj_stats_5v5 %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60
        )
        
        #add date and game id to the stats and denote which was home team
        team_adj_stats_5v5$is_home <- ifelse(team_adj_stats_all_sits$Team ==
                                                 first(pbp_df$home_team), 1, 0)
        team_adj_stats_5v5$season <- first(pbp_df$season)
        team_adj_stats_5v5$session <- first(pbp_df$session)
        
        ############################################################################
        
        ############################################################################
        ##Create unadjusted all situations player stats                           ##
        ############################################################################
        
        home1_all_sits <- pbp_df %>% group_by(home_on_1) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_1)
        
        home2_all_sits <- pbp_df %>% group_by(home_on_2) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_2)
        
        home3_all_sits <- pbp_df %>% group_by(home_on_3) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_3)
        
        home4_all_sits <- pbp_df %>% group_by(home_on_4) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_4)
        
        home5_all_sits <- pbp_df %>% group_by(home_on_5) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_5)
        
        home6_all_sits <- pbp_df %>% group_by(home_on_6) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_6)
        
        home_all_sits <- bind_rows(home1_all_sits, home2_all_sits,
                                   home3_all_sits, home4_all_sits,
                                   home5_all_sits, home6_all_sits)
        
        home_all_sits$team <- first(pbp_df$home_team)
        
        home_all_sits <- home_all_sits %>% group_by(player, team) %>%
            summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA),
                      C_plus_minus = sum(C_plus_minus),
                      FF = sum(FF), FA = sum(FA),
                      xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA))
        
        #join rate stats with goals and assists and ixG states and calculate per 60
        #stats for each one
        
        home_all_sits <- full_join(home_all_sits, home_individual_stats,
                                   by = c("player")) %>%
            full_join(home_groupd_player_xg, by = c("player"))
        home_all_sits <- replace_na(home_all_sits,
                                    list(G = 0, A1 = 0, A2 = 0, ixG = 0))
        
        home_all_sits <- home_all_sits %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            G60 = (G/TOI) * 60,
            A160 = (A1/TOI) * 60,
            A260 = (A2/TOI) * 60,
            P60 = ((G + A1 + A2)/TOI) * 60,
            ixG60 = (ixG/TOI) * 60
        )
        
        #add game id and date
        home_all_sits$game_id <- first(pbp_df$game_id)
        home_all_sits$game_date <- first(pbp_df$game_date)
       
        #compile away stats all sits unadjusted
        away1_all_sits <- pbp_df %>% group_by(away_on_1) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_1)
        
        away2_all_sits <- pbp_df %>% group_by(away_on_2) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_2)
        
        away3_all_sits <- pbp_df %>% group_by(away_on_3) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_3)
        
        away4_all_sits <- pbp_df %>% group_by(away_on_4) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_4)
        
        away5_all_sits <- pbp_df %>% group_by(away_on_5) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_5)
        
        away6_all_sits <- pbp_df %>% group_by(away_on_6) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_6)
        
        
        away_all_sits <- bind_rows(away1_all_sits, away2_all_sits,
                                   away3_all_sits, away4_all_sits,
                                   away5_all_sits, away6_all_sits)
        
        away_all_sits$team <- first(pbp_df$away_team)
        
        away_all_sits <- away_all_sits %>% group_by(player, team) %>%
            summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA),
                      C_plus_minus = sum(C_plus_minus),
                      FF = sum(FF), FA = sum(FA),
                      xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA))
        
        #join rate stats with goals and assists and ixG states and calculate per 60
        #stats for each one
        
        away_all_sits <- full_join(away_all_sits, away_individual_stats,
                                   by = c("player")) %>%
            full_join(away_groupd_player_xg, by = c("player"))
        away_all_sits <- replace_na(away_all_sits,
                                    list(G = 0, A1 = 0, A2 = 0, ixG = 0))
        
        away_all_sits <- away_all_sits %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            G60 = (G/TOI) * 60,
            A160 = (A1/TOI) * 60,
            A260 = (A2/TOI) * 60,
            P60 = ((G + A1 + A2)/TOI) * 60,
            ixG60 = (ixG/TOI) * 60
        )
        
        #add game id and date
        away_all_sits$game_id <- first(pbp_df$game_id)
        away_all_sits$game_date <- first(pbp_df$game_date)
        
        player_stats <- rbind(away_all_sits, home_all_sits)
        player_stats$season <- first(pbp_df$season)
        player_stats$session <- first(pbp_df$session)
        
        ############################################################################
        ##Create Team stats all situations unadjusted.                            ##
        ############################################################################
        #compile stats for home team
        home_team_stats_all_sits <- pbp_df %>%
            summarise(Team = first(pbp_df$home_team), game_id = first(pbp_df$game_id),
                      game_date = first(game_date),
                      TOI = last(game_seconds)/60, CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = CF - CA,
                      FF = sum(home_fenwick), FA = sum(away_fenwick),
                      xGF = sum(home_xG), xGA = sum(away_xG),
                      GF = sum(home_goal), GA = sum(away_goal))
        
        #compile stats for away team
        away_team_stats_all_sits <- pbp_df %>%
            summarise(Team = first(pbp_df$away_team), game_id = first(pbp_df$game_id),
                      game_date = first(game_date),
                      TOI = last(game_seconds)/60, CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = CF - CA,
                      FF = sum(away_fenwick), FA = sum(home_fenwick),
                      xGF = sum(away_xG), xGA = sum(home_xG),
                      GF = sum(away_goal), GA = sum(home_goal))
        
        #combine home and away into one dataframe
        team_stats_all_sits <- rbind(home_team_stats_all_sits,
                                     away_team_stats_all_sits)
        
        #calculate rate stats
        team_stats_all_sits <- team_stats_all_sits %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60
        )
        
        #add date and game id to the stats and denote which was home team
        team_stats_all_sits$is_home <- ifelse(team_stats_all_sits$Team ==
                                                  first(pbp_df$home_team), 1, 0)
        team_stats_all_sits$season <- first(pbp_df$season)
        team_stats_all_sits$session <- first(pbp_df$session)
        
        ############################################################################
        ##Create stats for players 5v5 unadjusted.                                ##
        ############################################################################
        home1_5v5 <- pbp_df_5v5 %>% group_by(home_on_1) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_1)
        
        home2_5v5 <- pbp_df_5v5 %>% group_by(home_on_2) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_2)
        
        home3_5v5 <- pbp_df_5v5 %>% group_by(home_on_3) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_3)
        
        home4_5v5 <- pbp_df_5v5 %>% group_by(home_on_4) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_4)
        
        home5_5v5 <- pbp_df_5v5 %>% group_by(home_on_5) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_5)
        
        home6_5v5 <- pbp_df_5v5 %>% group_by(home_on_6) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(home_xG),
                      xGA = sum(away_xG), CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = (sum(home_corsi) - sum(away_corsi)),
                      FF = sum(home_fenwick),
                      FA = sum(away_fenwick), GF = sum(home_goal),
                      GA = sum(away_goal)) %>% na.omit() %>%
            rename(player = home_on_6)
        
        home_5v5 <- bind_rows(home1_5v5, home2_5v5,
                              home3_5v5, home4_5v5,
                              home5_5v5, home6_5v5)
        
        home_5v5$team <- first(pbp_df$home_team)
        
        home_5v5 <- home_5v5 %>% group_by(player, team) %>%
            summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA),
                      C_plus_minus = sum(C_plus_minus),
                      FF = sum(FF), FA = sum(FA),
                      xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA))
        
        #join rate stats with goals and assists and ixG states and calculate per 60
        #stats for each one
        
        home_5v5 <- full_join(home_5v5, home_individual_stats_5v5,
                              by = c("player")) %>%
            full_join(home_groupd_player_xg_5v5, by = c("player"))
        home_5v5 <- replace_na(home_5v5,
                               list(G = 0, A1 = 0, A2 = 0, ixG = 0))
        
        home_5v5 <- home_5v5 %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            G60 = (G/TOI) * 60,
            A160 = (A1/TOI) * 60,
            A260 = (A2/TOI) * 60,
            P60 = ((G + A1 + A2)/TOI) * 60,
            ixG60 = (ixG/TOI) * 60
        )
        
        #add game id and date
        home_5v5$game_id <- first(pbp_df$game_id)
        home_5v5$game_date <- first(pbp_df$game_date)
        
        #5v5 adjusted away team
        
        away1_5v5 <- pbp_df_5v5 %>% group_by(away_on_1) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_1)
        
        away2_5v5 <- pbp_df_5v5 %>% group_by(away_on_2) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_2)
        
        away3_5v5 <- pbp_df_5v5 %>% group_by(away_on_3) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_3)
        
        away4_5v5 <- pbp_df_5v5 %>% group_by(away_on_4) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_4)
        
        away5_5v5 <- pbp_df_5v5 %>% group_by(away_on_5) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_5)
        
        away6_5v5 <- pbp_df_5v5 %>% group_by(away_on_6) %>%
            summarise(TOI = sum(event_length)/60, xGF = sum(away_xG),
                      xGA = sum(home_xG), CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = (sum(away_corsi) - sum(home_corsi)),
                      FF = sum(away_fenwick),
                      FA = sum(home_fenwick), GF = sum(away_goal),
                      GA = sum(home_goal)) %>% na.omit() %>%
            rename(player = away_on_6)
        
        
        away_5v5 <- bind_rows(away1_5v5, away2_5v5,
                              away3_5v5, away4_5v5,
                              away5_5v5, away6_5v5)
        
        away_5v5$team <- first(pbp_df$away_team)
        
        away_5v5 <- away_5v5 %>% group_by(player, team) %>%
            summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA),
                      C_plus_minus = sum(C_plus_minus),
                      FF = sum(FF), FA = sum(FA),
                      xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA))
        
        #join rate stats with goals and assists and ixG states and calculate per 60
        #stats for each one
        
        away_5v5 <- full_join(away_5v5, away_individual_stats_5v5,
                              by = c("player")) %>%
            full_join(away_groupd_player_xg_5v5, by = c("player"))
        away_5v5 <- replace_na(away_5v5,
                               list(G = 0, A1 = 0, A2 = 0, ixG = 0))
        
        away_5v5 <- away_5v5 %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            G60 = (G/TOI) * 60,
            A160 = (A1/TOI) * 60,
            A260 = (A2/TOI) * 60,
            P60 = ((G + A1 + A2)/TOI) * 60,
            ixG60 = (ixG/TOI) * 60
        )
        
        #add game id and date
        away_5v5$game_id <- first(pbp_df$game_id)
        away_5v5$game_date <- first(pbp_df$game_date)
        
        player_stats_5v5 <- rbind(away_5v5, home_5v5)
        player_stats_5v5$season <- first(pbp_df$season)
        player_stats_5v5$session <- first(pbp_df$session)
        
        ############################################################################
        ##Create total team statistics 5v5.                                       ##
        ############################################################################
        
        #compile stats for home team
        home_team_stats_5v5 <- pbp_df_5v5 %>%
            summarise(Team = first(pbp_df$home_team), game_id = first(pbp_df$game_id),
                      game_date = first(game_date),
                      TOI = sum(event_length)/60, CF = sum(home_corsi),
                      CA = sum(away_corsi),
                      C_plus_minus = CF - CA,
                      FF = sum(home_fenwick), FA = sum(away_fenwick),
                      xGF = sum(home_xG), xGA = sum(away_xG),
                      GF = sum(home_goal), GA = sum(away_goal))
        
        #compile stats for away team
        away_team_stats_5v5 <- pbp_df_5v5 %>%
            summarise(Team = first(pbp_df$away_team), game_id = first(pbp_df$game_id),
                      game_date = first(game_date),
                      TOI = sum(event_length)/60, CF = sum(away_corsi),
                      CA = sum(home_corsi),
                      C_plus_minus = CF - CA,
                      FF = sum(away_fenwick), FA = sum(home_fenwick),
                      xGF = sum(away_xG), xGA = sum(home_xG),
                      GF = sum(away_goal), GA = sum(home_goal))
        
        #combine home and away into one dataframe
        team_stats_5v5 <- rbind(home_team_stats_5v5,
                                away_team_stats_5v5)
        
        #calculate rate stats
        team_stats_5v5 <- team_stats_5v5 %>% mutate(
            CF60 = (CF/TOI) * 60,
            CA60 = (CA/TOI) * 60,
            CF_per = (CF/(CF + CA)) * 100,
            FF60 = (FF/TOI) * 60,
            FA60 = (FA/TOI) * 60,
            FF_per = (FF/(FF + FA)) * 100,
            GF_per = (GF/(GF + GA)) * 100,
            xGF60 = (xGF/TOI) * 60,
            xGA60 = (xGA/TOI) * 60,
            xGF_per = (xGF/(xGF + xGA)) * 100,
            GF60 = (GF/TOI) * 60,
            GA60 = (GA/TOI) * 60
        )
        
        #add date and game id to the stats and denote which was home team
        team_stats_5v5$is_home <- ifelse(team_stats_5v5$Team ==
                                             first(pbp_df$home_team), 1, 0)
        team_stats_5v5$season <- first(pbp_df$season)
        team_stats_5v5$session <- first(pbp_df$session)
        
        #calculate goalie stats
        home_goalie_pbp <- subset(pbp_df, !is.na(pbp_df$home_goalie))
        away_goalie_pbp <- subset(pbp_df, !is.na(pbp_df$away_goalie))
        home_goalie_pbp_5v5 <- pbp_df_5v5
        away_goalie_pbp_5v5 <- pbp_df_5v5
        
        #calculate all situation goalie stats
        home_goalie_stats <- home_goalie_pbp %>% group_by(home_goalie)
            summarise(
                      game_id = first(home_goalie_pbp$game_id),
                      game_date = first(home_goalie_pbp$game_date),
                      season = first(home_goalie_pbp$season),
                      team = first(home_goalie_pbp$home_team),
                      TOI = sum(event_length)/60, CF = sum(home_corsi),
                      CA = sum(away_corsi), FA = sum(away_corsi), xGA = sum(away_xG),
                      SA = sum(ifelse(event_team == away_team & 
                                          (event_type == "GOAL" | 
                                           event_type == 'SHOT'), 1, 0)), 
                      GA = sum(ifelse(event_team == away_team & 
                                          event_type == "GOAL", 1, 0)),
                      LDGA = sum(ifelse(event_team == away_team & 
                                            event_type == "GOAL" &
                                            LD == 1, 1, 0)),
                      MDGA = sum(ifelse(event_team == away_team & 
                                            event_type == "GOAL" &
                                            MD == 1, 1, 0)),
                      HDGA = sum(ifelse(event_team == away_team & 
                                            event_type == "GOAL" &
                                            HD == 1, 1, 0)),
                      LDA = sum(ifelse(event_team == away_team &
                                           LD == 1 &
                                           (event_type == "GOAL" |
                                           event_type == "SHOT"), 1, 0)),
                      MDA = sum(ifelse(event_team == away_team &
                                     MD == 1 &
                                     (event_type == "GOAL" |
                                      event_type == "SHOT"), 1, 0)),
                      HDA = sum(ifelse(event_team == away_team &
                                           HD == 1 &
                                           (event_type == "GOAL" |
                                            event_type == "SHOT"), 1, 0))
                      )
        
        home_goalie_stats <- home_goalie_stats %>%
            mutate(sv_percent = round(1-(GA/SA), 3), 
                   fsv_percent = round(1-(GA/FA), 3),
                   xfsv_percent = round(1-(xGA/FA), 3), 
                   dfsv_percent = fsv_percent-xfsv_percent,
                   hdsv_percent = round(1-(HDGA/HDA),3),
                   mdsv_percent = round(1-(MDGA/MDA),3),
                   ldsv_percent = round(1-(LDGA/LDA),3))
        
        away_goalie_stats <- away_goalie_pbp %>% group_by(away_goalie) %>%
            summarise( 
                      game_id = first(home_goalie_pbp$game_id),
                      game_date = first(home_goalie_pbp$game_date),
                      season = first(home_goalie_pbp$season),
                      team = first(home_goalie_pbp$away_team),
                      TOI = sum(event_length)/60, CF = sum(away_corsi),
                      CA = sum(home_corsi), FA = sum(home_corsi), xGA = sum(home_xG),
                      SA = sum(ifelse(event_team == home_team & 
                                          (event_type == "GOAL" | 
                                               event_type == 'SHOT'), 1, 0)), 
                      GA = sum(ifelse(event_team == home_team & 
                                          event_type == "GOAL", 1, 0)),
                      LDGA = sum(ifelse(event_team == home_team & 
                                            event_type == "GOAL" &
                                            LD == 1, 1, 0)),
                      MDGA = sum(ifelse(event_team == home_team & 
                                            event_type == "GOAL" &
                                            MD == 1, 1, 0)),
                      HDGA = sum(ifelse(event_team == home_team & 
                                            event_type == "GOAL" &
                                            HD == 1, 1, 0)),
                      LDA = sum(ifelse(event_team == home_team &
                                           LD == 1 &
                                           (event_type == "GOAL" |
                                                event_type == "SHOT"), 1, 0)),
                      MDA = sum(ifelse(event_team == home_team &
                                           MD == 1 &
                                           (event_type == "GOAL" |
                                                event_type == "SHOT"), 1, 0)),
                      HDA = sum(ifelse(event_team == home_team &
                                           HD == 1 &
                                           (event_type == "GOAL" |
                                                event_type == "SHOT"), 1, 0))
            )
        
        away_goalie_stats <- away_goalie_stats %>%
            mutate(sv_percent = round(1-(GA/SA), 3), 
                   fsv_percent = round(1-(GA/FA), 3),
                   xfsv_percent = round(1-(xGA/FA), 3), 
                   dfsv_percent = fsv_percent-xfsv_percent,
                   hdsv_percent = round(1-(HDGA/HDA),3),
                   mdsv_percent = round(1-(MDGA/MDA),3),
                   ldsv_percent = round(1-(LDGA/LDA),3))
        
        goalie_stats_all_sits <- rbind(home_goalie_stats, away_goalie_stats)
        
        #calculate goalie 5v5 stats
        home_goalie_stats_5v5 <- home_goalie_pbp_5v5 %>% group_by(home_goalie) %>%
            summarise( 
                      game_id = first(home_goalie_pbp$game_id),
                      game_date = first(home_goalie_pbp$game_date),
                      season = first(home_goalie_pbp$season),
                      team = first(home_goalie_pbp$home_team),
                      TOI = sum(event_length)/60, CF = sum(home_corsi),
                      CA = sum(away_corsi), FA = sum(away_corsi), xGA = sum(away_xG),
                      SA = sum(ifelse(event_team == away_team & 
                                          (event_type == "GOAL" | 
                                               event_type == 'SHOT'), 1, 0)), 
                      GA = sum(ifelse(event_team == away_team & 
                                          event_type == "GOAL", 1, 0)),
                      LDGA = sum(ifelse(event_team == away_team & 
                                            event_type == "GOAL" &
                                            LD == 1, 1, 0)),
                      MDGA = sum(ifelse(event_team == away_team & 
                                            event_type == "GOAL" &
                                            MD == 1, 1, 0)),
                      HDGA = sum(ifelse(event_team == away_team & 
                                            event_type == "GOAL" &
                                            HD == 1, 1, 0)),
                      LDA = sum(ifelse(event_team == away_team &
                                           LD == 1 &
                                           (event_type == "GOAL" |
                                                event_type == "SHOT"), 1, 0)),
                      MDA = sum(ifelse(event_team == away_team &
                                           MD == 1 &
                                           (event_type == "GOAL" |
                                                event_type == "SHOT"), 1, 0)),
                      HDA = sum(ifelse(event_team == away_team &
                                           HD == 1 &
                                           (event_type == "GOAL" |
                                                event_type == "SHOT"), 1, 0))
            )
        
        home_goalie_stats_5v5 <- home_goalie_stats_5v5 %>%
            mutate(sv_percent = round(1-(GA/SA), 3), 
                   fsv_percent = round(1-(GA/FA), 3),
                   xfsv_percent = round(1-(xGA/FA), 3), 
                   dfsv_percent = fsv_percent-xfsv_percent,
                   hdsv_percent = round(1-(HDGA/HDA),3),
                   mdsv_percent = round(1-(MDGA/MDA),3),
                   ldsv_percent = round(1-(LDGA/LDA),3))
        
        away_goalie_stats_5v5 <- away_goalie_pbp_5v5 %>% group_by(away_goalie) %>%
            summarise( 
                      game_id = first(home_goalie_pbp$game_id),
                      game_date = first(home_goalie_pbp$game_date),
                      season = first(home_goalie_pbp$season),
                      team = first(home_goalie_pbp$away_team),
                      TOI = sum(event_length)/60, CF = sum(away_corsi),
                      CA = sum(home_corsi), FA = sum(home_corsi), xGA = sum(home_xG),
                      SA = sum(ifelse(event_team == home_team & 
                                          (event_type == "GOAL" | 
                                               event_type == 'SHOT'), 1, 0)), 
                      GA = sum(ifelse(event_team == home_team & 
                                          event_type == "GOAL", 1, 0)),
                      LDGA = sum(ifelse(event_team == home_team & 
                                            event_type == "GOAL" &
                                            LD == 1, 1, 0)),
                      MDGA = sum(ifelse(event_team == home_team & 
                                            event_type == "GOAL" &
                                            MD == 1, 1, 0)),
                      HDGA = sum(ifelse(event_team == home_team & 
                                            event_type == "GOAL" &
                                            HD == 1, 1, 0)),
                      LDA = sum(ifelse(event_team == home_team &
                                           LD == 1 &
                                           (event_type == "GOAL" |
                                                event_type == "SHOT"), 1, 0)),
                      MDA = sum(ifelse(event_team == home_team &
                                           MD == 1 &
                                           (event_type == "GOAL" |
                                                event_type == "SHOT"), 1, 0)),
                      HDA = sum(ifelse(event_team == home_team &
                                           HD == 1 &
                                           (event_type == "GOAL" |
                                                event_type == "SHOT"), 1, 0))
            )
        
        away_goalie_stats_5v5 <- away_goalie_stats_5v5 %>%
            mutate(sv_percent = round(1-(GA/SA), 3), 
                   fsv_percent = round(1-(GA/FA), 3),
                   xfsv_percent = round(1-(xGA/FA), 3), 
                   dfsv_percent = fsv_percent-xfsv_percent,
                   hdsv_percent = round(1-(HDGA/HDA),3),
                   mdsv_percent = round(1-(MDGA/MDA),3),
                   ldsv_percent = round(1-(LDGA/LDA),3))
        
        goalie_stats_all_sits <- rbind(home_goalie_stats, away_goalie_stats)
        goalie_stats_5v5 <- rbind(home_goalie_stats_5v5, away_goalie_stats_5v5)
        
        #create unique keys for each table 
        
        goalie_stats_all_sits$db_key <- paste0(goalie_stats_all_sits$goalie,
                                               goalie_stats_all_sits$game_date,
                                               goalie_stats_all_sits$game_id)
        
        goalie_stats_5v5$db_key <- paste0(goalie_stats_5v5$goalie,
                                               goalie_stats_5v5$game_date,
                                               goalie_stats_5v5$game_id)
        
        player_5v5_adj$db_key <- paste0(player_5v5_adj$player, 
                                        player_5v5_adj$game_date,
                                        player_5v5_adj$game_id,
                                        player_5v5_adj$season)
        
        player_all_sits_adj$db_key <- paste0(player_all_sits_adj$player, 
                                             player_all_sits_adj$game_date,
                                             player_all_sits_adj$game_id,
                                             player_all_sits_adj$season)
        
        player_stats$db_key <- paste0(player_stats$player, 
                                      player_stats$game_date,
                                      player_stats$game_id,
                                      player_stats$season)
        
        player_stats_5v5$db_key <- paste0(player_stats_5v5$player, 
                                          player_stats_5v5$game_date,
                                          player_stats_5v5$game_id,
                                          player_stats_5v5$season)
        
        team_adj_stats_5v5$db_key <- paste0(team_adj_stats_5v5$Team,
                                            team_adj_stats_5v5$game_date,
                                            team_adj_stats_5v5$game_id,
                                            team_adj_stats_5v5$season)
        
        team_adj_stats_all_sits$db_key <- paste0(team_adj_stats_all_sits$Team,
                                                 team_adj_stats_all_sits$game_date,
                                                 team_adj_stats_all_sits$game_id,
                                                 team_adj_stats_all_sits$season)
        
        team_stats_5v5$db_key <- paste0(team_stats_5v5$Team,
                                        team_stats_5v5$game_date,
                                        team_stats_5v5$game_id,
                                        team_stats_5v5$season)
        
        team_stats_all_sits$db_key <- paste0(team_stats_all_sits$Team,
                                             team_stats_all_sits$game_date,
                                             team_stats_all_sits$game_id,
                                             team_stats_all_sits$season)
        
        
        
        #saves all the plots to png files and folder labeled with game number
        
        setwd(paste0('~/HockeyStuff/xGGameBreakdowns/',
                     as.character(season), '/', as.character(game_number)))
        
    
        write_delim(pbp_df, toString(game_number), delim = '|')
        write_delim(player_all_sits_adj, paste(toString(game_number),
                                               'playerstatsadj'), delim = '|')
        write_delim(player_5v5_adj, paste(toString(game_number),
                                               'playerstatsadj5v5'), delim = '|')
        write_delim(player_stats, paste(toString(game_number),
                                               'playerstats'), delim = '|')
        write_delim(player_stats_5v5, paste(toString(game_number),
                                               'playerstats5v5'), delim = '|')
        write_delim(team_adj_stats_all_sits, paste(toString(game_number),
                                                   'teamstatsadj'), delim = '|')
        write_delim(team_adj_stats_5v5, paste(toString(game_number),
                                               'teamstatsadj5v5'), delim = '|')
        write_delim(team_stats_5v5, paste(toString(game_number),
                                              'teamstats5v5'), delim = '|')
        write_delim(team_stats_all_sits, paste(toString(game_number),
                                              'teamstats'), delim = '|')
        write_delim(goalie_stats_all_sits, 'goaliestats', delim = '|')
        write_delim(goalie_stats_5v5, 'goaliestats5v5', delim = '|')
    
    
    
        
    }
}


