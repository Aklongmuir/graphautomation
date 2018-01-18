
################################################################################
#All code below here including xGmodel written by Matthew Barlowe @matt_barlowe#
#on twitter#####################################################################

library(magrittr)
library(ggplot2)
library(ggforce)
library(readr)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(stringr)

test <- (1:95)
#stores yesterday's date in a variable

date <- Sys.Date()-1

#daily_games stores each games results in goals and xG to write to text file for
#twitter bot and daily_pbp will be the dataframe that holds the complete play by
#play from every game the day before that will be written to a text file for sql
#insertion
daily_games <- c()
daily_pbp <- NULL
daily_adjusted_player_stats <- NULL
daily_team_adjusted_stats <- NULL
daily_adj_player_stats_5v5 <- NULL
daily_adj_team_stats_5v5 <- NULL
daily_player_stats <- NULL
daily_team_stats <- NULL
daily_player_stats_5v5 <- NULL
daily_team_stats_5v5 <- NULL
unscraped_games <-c()

#assigns colors to each team for graphing purposes
team_colors <- c('black', 'darkred', 'gold', 'royalblue4', 'red3', 'firebrick1',
                 'black', 'navy', 'maroon', 'red', 'darkgreen', 'navyblue',
                 'orange', 'mediumblue', 'slategrey', 'limegreen', 'darkgreen',
                 'darkorange1', 'yellow2', 'mediumblue', 'steelblue4', 'red2',
                 'lightseagreen', 'darkorange1', 'dodgerblue4', 'gold', 'gold',
                 'dodgerblue3', 'dodgerblue3', 'navyblue', 'red1')
names(team_colors) <- c('ANA', 'ARI','BOS', 'BUF', 'CGY', 'CAR', 'CHI', 'CBJ',
                        'COL', 'DET', 'DAL', 'FLA', 'EDM', 'MTL', 'L.A', 'N.J',
                        'MIN', 'NYI', 'NSH', 'NYR', 'STL', 'OTT', 'S.J', 'PHI',
                        'VAN', 'PIT', 'VGK', 'T.B', 'TOR', 'WPG', 'WSH')

team_hashtags <- c('#LetsGoDucks', '#Yotes', '#NHLBruins', '#Sabres', '#CofRed',
                   '#Redvolution', '#Blackhawks', '#CBJ', '#GoAvsGo', '#LGRW',
                   '#GoStars', '#FlaPanthers', '#LetsGoOilers', '#GoHabsGo',
                   '#GoKingsGo', '#NJDevils', '#mnwild', '#Isles', '#Preds',
                   '#NYR', '#AllTogetherNowSTL', '#Sens', '#SJSharks',
                   '#LetsGoFlyers', '#Canucks', '#LetsGoPens', '#VegasBorn',
                   '#GoBolts', '#TMLtalk', '#GoJetsGo', '#ALLCAPS')

names(team_hashtags) <- c('ANA', 'ARI','BOS', 'BUF', 'CGY', 'CAR', 'CHI', 'CBJ',
                          'COL', 'DET', 'DAL', 'FLA', 'EDM', 'MTL', 'L.A', 'N.J',
                          'MIN', 'NYI', 'NSH', 'NYR', 'STL', 'OTT', 'S.J', 'PHI',
                          'VAN', 'PIT', 'VGK', 'T.B', 'TOR', 'WPG', 'WSH')

get_games <- function(yesterday){
    #This fuction returns a list of game ids and the nhl season from the day
    #before to pass to the scraper in the loop below

    games <- c()
    season <-c()

    #scrapes all the games from the day before and stores in a list
    list_of_games <- ds.get_schedule(yesterday,yesterday)

    #retrieves season from scheduled games
    season <- list_of_games$dates[[1]]$games[[1]]$season
    #pulls out each game number from the scheduled games and stores it in a vector
    for (i in 1:length(list_of_games$dates[[1]]$games)) {
        games <- append(games, list_of_games$dates[[1]]$games[[i]]$gamePk)}
    games <- substr(games, 6, 10)
    return(list(games, season))
}

#function to create dummy variables to determine if
#event was committed by the home team
is_home <- function(dataframe){
    dataframe$is_home <- ifelse(dataframe$event_team ==
                                    dataframe$home_team, 1 , 0)
    return(dataframe)
}


#Try/Except block for when no games are played it will write "No games today"
#which the python scripts for sql insertion and twitter bot will catch and
#break execution
#tryCatch(games <- get_games(date),
#         error = function(cond) {
#             message("No Games Today")
#             return(NULL)
#         }
#)


games <- c(20394)

csv_games <-c(20540)
#Loops through game numbers in the daily_games vector and scrapes the data
#for the game with that game id
for(game_number in games){
    #scrapes NHL data for given game_number and stores it in a list
    #pbp_list <- NULL
    #pbp_list <- tryCatch(
    #      {
    #          pbp_list<-ds.compile_games(games = as.character(game_number),
    #                              season = games[[2]],
    #                              pause = 2,
    #                              try_tolerance = 5,
    #                              agents = ds.user_agents)
    # 
    #      },
    #          error = function(cond) {
    #             message("Error scraping game")
    #             return(NULL)
    #             }
    # )
    # if(length(pbp_list) == 0){
    #     unscraped_games <- c(as.character(game_number), unscraped_games)
    #     next
    #     }


#pulls out the actual pbp data from the list
    pbp_df <- read_delim(paste0('~/HockeyStuff/xGGameBreakdowns/2018/', game_number, '/',
                                game_number), delim = '|')

    print(game_number)


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

    daily_adjusted_player_stats <- rbind(home_all_sits_adj,
                                         daily_adjusted_player_stats)

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

    #adding the adjusted player stats to the dataframe that will be written to
    #text file for sql insertion

    daily_adjusted_player_stats <- rbind(away_all_sits_adj,
                                         daily_adjusted_player_stats)




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

    #add to the daily team dataframe to write to text file for sql insertion
    daily_team_adjusted_stats <- rbind(team_adj_stats_all_sits,
                                       daily_team_adjusted_stats)
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

    #bind player 5v5 to the daily 5v5 adjusted dataframe
    daily_adj_player_stats_5v5 <- rbind(home_adj_5v5, daily_adj_player_stats_5v5)

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

    #bind player 5v5 to the daily 5v5 adjusted dataframe
    daily_adj_player_stats_5v5 <- rbind(away_adj_5v5, daily_adj_player_stats_5v5)

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

    #add to the daily team dataframe to write to text file for sql insertion
    daily_adj_team_stats_5v5 <- rbind(team_adj_stats_5v5,
                                       daily_adj_team_stats_5v5)
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

    #bind player 5v5 to the daily 5v5 adjusted dataframe
    daily_player_stats <- rbind(home_all_sits, daily_player_stats)

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

    #bind player 5v5 to the daily 5v5 adjusted dataframe
    daily_player_stats <- rbind(away_all_sits, daily_player_stats)

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

    #add to the daily team dataframe to write to text file for sql insertion
    daily_team_stats <- rbind(team_stats_all_sits,
                                       daily_team_stats)

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

    #bind player 5v5 to the daily 5v5 adjusted dataframe
    daily_player_stats_5v5 <- rbind(home_5v5, daily_player_stats_5v5)

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

    #bind player 5v5 to the daily 5v5 adjusted dataframe
    daily_player_stats_5v5 <- rbind(away_5v5, daily_player_stats_5v5)

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

    #add to the daily team dataframe to write to text file for sql insertion
    daily_team_stats_5v5 <- rbind(team_stats_5v5,
                                      daily_team_stats_5v5)
    

    #saves all the plots to png files and folder labeled with game number
    dir.create(paste0('~/HockeyStuff/xGGameBreakdowns/2018/', as.character(game_number)))
    setwd(paste0('~/HockeyStuff/xGGameBreakdowns/2018/', as.character(game_number)))
    

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



    
}

################################################################################
##Writing Data results.                                                       ##
################################################################################

#writes the total daily pbp to a file and is delimited by | because commas from
#line change columns will mess up sql insert
# write_delim(daily_pbp, '~/HockeyStuff/CompleteNHLPbPData/dailypbp',
#             delim = '|')


#opens dailygames.txt file and updates with yesterdays game results in goals and
#xg for twitter bot posts and then closes file
# fileConn <- file('~/graphautomation/dailygames.txt')
# writeLines(daily_games, fileConn)
# close(fileConn)


