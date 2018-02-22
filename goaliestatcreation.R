games <- c(20617:21230)
seasons <- c(2018)
shots <- c("SHOT", "GOAL")

#Loops through game numbers in the daily_games vector and scrapes the data
#for the game with that game id
for (season in seasons){
    for(game_number in games){    
        pbp_df <- read_delim(paste0('~/HockeyStuff/xGGameBreakdowns/',season, 
                                    '/', game_number, '/',
                                    game_number), delim = '|')
        
        print(game_number)
        pbp_df_5v5 <- subset(pbp_df, pbp_df$home_skaters == 5 &
                                 pbp_df$away_skaters == 5)
#calculate goalie stats
        home_goalie_pbp <- subset(pbp_df, !is.na(pbp_df$home_goalie))
        away_goalie_pbp <- subset(pbp_df, !is.na(pbp_df$away_goalie))
        home_goalie_pbp_5v5 <- pbp_df_5v5
        away_goalie_pbp_5v5 <- pbp_df_5v5
        
        #calculate all situation goalie stats
        home_goalie_stats <- home_goalie_pbp %>% group_by(home_goalie) %>%
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
        
        home_goalie_stats <- rename(home_goalie_stats, goalie = home_goalie)
        away_goalie_stats <- rename(away_goalie_stats, goalie = away_goalie)
        
        
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
        
        home_goalie_stats_5v5 <- rename(home_goalie_stats_5v5, goalie = home_goalie)
        away_goalie_stats_5v5 <- rename(away_goalie_stats_5v5, goalie = away_goalie)
        
        goalie_stats_all_sits <- rbind(home_goalie_stats, away_goalie_stats)
        goalie_stats_5v5 <- rbind(home_goalie_stats_5v5, away_goalie_stats_5v5)
        
        goalie_stats_5v5 <- goalie_stats_5v5[!is.na(goalie_stats_5v5$goalie),]
        goalie_stats_all_sits <- goalie_stats_all_sits[!is.na(goalie_stats_all_sits$goalie),]
        #create unique keys for each table 
        
        goalie_stats_all_sits$db_key <- paste0(goalie_stats_all_sits$goalie,
                                               goalie_stats_all_sits$game_date,
                                               goalie_stats_all_sits$game_id)
        
        goalie_stats_5v5$db_key <- paste0(goalie_stats_5v5$goalie,
                                          goalie_stats_5v5$game_date,
                                          goalie_stats_5v5$game_id)
        
        setwd(paste0('~/HockeyStuff/xGGameBreakdowns/',
                     as.character(season), '/', as.character(game_number)))
        
        write_delim(goalie_stats_all_sits, 'goaliestats', delim = '|')
        write_delim(goalie_stats_5v5, 'goaliestats5v5', delim = '|')
        
    }
}