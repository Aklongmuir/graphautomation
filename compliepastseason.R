################################################################################
#All code below here including xGmodel written by Matthew Barlowe @matt_barlowe#
#on twitter#####################################################################

library(magrittr)

library(readr)
library(tidyr)
library(dplyr)




#function to create dummy variables to determine if
#event was committed by the home team
is_home <- function(dataframe){
    dataframe$is_home <- ifelse(dataframe$event_team ==
                                    dataframe$home_team, 1 , 0)
    return(dataframe)
}



seasons_to_parse <- c('2017', '2016', '2015')
gamenumbers <- c(20702:21230)
scoreadj_corsi <- data.frame(matrix(nrow = 7, ncol = 3))
scoreadj_corsi[, 1] <- c(1, 2, 3, 4, 5, 6, 7)
scoreadj_corsi[, 2] <- c(0.840, 0.865, 0.898, 0.970, 1.052, 1.104, 1.138)
scoreadj_corsi[, 3] <- c(1.236, 1.186, 1.128, 1.032, 0.953, 0.914, 0.892)
colnames(scoreadj_corsi) <- c("home_lead", "home_corsi_adj", "away_corsi_adj")



xG_adj_h <- 0.9468472
xG_adj_a <- 1.059477

#loads xGmodel
xGModel <-load('~/ProgrammingStuff/R/Rprojects/xGmodel/xGmodelver2.rda')


#vectors used to subset the pbp data depending on event types
corsi <- c('SHOT', 'MISS', 'BLOCK', 'GOAL')
fenwick <- c('SHOT', 'MISS', 'GOAL')

#game strength states vectors used to determine shooter strength state
even_strength <- c('5v5', '4v4', '3v3', 'EvE')
home_advantage <- c('5v4', '5v3', '4v3')
away_advantage <- c('4v5', '3v5', '3v4')





total_pbp_df <- read_delim(paste0('~/HockeyStuff/CompleteNHLPbPData/', 
                            '2017', 'NHLPbP.csv'), delim = ',')

for(number in gamenumbers){
    print(as.character(number))
    pbp_df <- subset(total_pbp_df, total_pbp_df$game_id == as.numeric(paste0('20160', as.character(number))))
    pbp_df <- pbp_df[order(pbp_df$event_index),]
    #removes shootout info from data frame and creates is_home dummy variable
    pbp_df <- pbp_df[pbp_df$game_period < 5,]
    pbp_df <- is_home(pbp_df)
    
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
    print('features created')
    ############################################################################
    ##Calculates xG values for each event from the fenwick events subset of   ##
    ## the pbp_df and then merges it back to the pbp_df.                      ##
    ############################################################################
    
    #filters pbp dataframe to fenwick events and predict the xG of each event
    fenwick_pbp<- filter(pbp_df, event_type %in% c("SHOT", "MISS", "GOAL"))
    fenwick_pbp$xG <-predict(xGmodel, fenwick_pbp, type = 'response')
    
    #merge the fenwick PbP xG values back with the main PbP dataframe and
    #replacing any NAs with zeros
    pbp_df <- left_join(pbp_df, fenwick_pbp[, c('event_index', 'xG')],
                    by = c('event_index'), all.x = TRUE)
    
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
    print('xG and corsi/fenwick calculated')
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
    print('adjusted corsi and xg calculated')
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
    print('goals and assits calculated')
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
    
    print('all sits stats calculated')
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
    
    print('5v5 adjusted stats calculated')
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
    
    print('all sits stats calculated')
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
    
    print('5v5 stats calculated')
    #calculates the running sum for the step graphs for all situations and 5v5
    pbp_df <- mutate(pbp_df, run_home_xg = cumsum(home_xG))
    pbp_df <- mutate(pbp_df, run_away_xg = cumsum(away_xG))
    pbp_df <- mutate(pbp_df, run_home_5v5_xg = cumsum(home_5v5_xG))
    pbp_df <- mutate(pbp_df, run_away_5v5_xg = cumsum(away_5v5_xG))
    
    #saves all the plots to png files and folder labeled with game number
    dir.create(paste0('~/HockeyStuff/xGGameBreakdowns/2017/', as.character(number)))
    setwd(paste0('~/HockeyStuff/xGGameBreakdowns/2015/20600/',as.character(number)))
    
    write_delim(pbp_df, toString(number), delim = '|')
    write_delim(player_all_sits_adj, paste(toString(number),
                                           'playerstatsadj'), delim = '|')
    write_delim(player_5v5_adj, paste(toString(number),
                                      'playerstatsadj5v5'), delim = '|')
    write_delim(player_stats, paste(toString(number),
                                    'playerstats'), delim = '|')
    write_delim(player_stats_5v5, paste(toString(number),
                                        'playerstats5v5'), delim = '|')
    write_delim(team_adj_stats_all_sits, paste(toString(number),
                                               'teamstatsadj'), delim = '|')
    write_delim(team_adj_stats_5v5, paste(toString(number),
                                          'teamstatsadj5v5'), delim = '|')
    write_delim(team_stats_5v5, paste(toString(number),
                                      'teamstats5v5'), delim = '|')
    write_delim(team_stats_all_sits, paste(toString(number),
                                           'teamstats'), delim = '|')
}
    
    
    
    
    
    
    
    
    
    
