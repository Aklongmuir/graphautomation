import keylesstwitterpost as twitterpost
import tweepy
import sys
import pandas as pd
import matplotlib
import random
import time
import os
from sqlalchemy import create_engine, and_, or_
from sqlalchemy.sql import func, select, cast
from sqlalchemy.schema import MetaData
from sqlalchemy.types import String, Float
matplotlib.use('Agg')
import matplotlib.pyplot as plt


def text_error_check(text):

    '''
    Checks the query to make sure it meets certain parameters.  If so returns
    True if not returns False

    Inputs:
    text - person twitter query converted into a list

    Outputs:
    Boolean - True or False depending on whether the query meets the parameters
    '''

    key_list = ['playerstats', 'teamstats', 'playergraph', 'teamgraph']

    for word in key_list:
        if word not in text:
            return False

    if len(text) < 4 and len(text) > 10:
        return False
    else:
        return True


def gsaa_creation(season, player, database, conn):
    '''
    Function creates a data frame of adjusted GSAA for the stats requested
    that can then be used to graph over time or sum to get a final value for
    the summary stat function

    Inputs:
    season - string consisting of the season averages to be calculated
    database - SLQ Alchemy object to query

    Outputs:
    gsaa_df - dataframe containing player's adj.gsaa30 value
    '''

    gsaa_lg_avg_query = select([database.c.game_date, func.sum(database.c.hdga).label('hdga'),
                                func.sum(database.c.hda).label('hda'), func.sum(database.c.mdga).label('mdga'),
                                func.sum(database.c.mda).label('mda'),
                                func.sum(database.c.ldga).label('ldga'),
                                func.sum(database.c.lda).label('lda')]).\
                        where(database.c.season == season).\
                        group_by(database.c.game_date)

    gsaa_avg_df = pd.read_sql_query(gsaa_lg_avg_query, conn)
    print(gsaa_avg_df.head())
    gsaa_avg_df = gsaa_avg_df.sort_values('game_date')
    print(gsaa_avg_df.head())
    gsaa_avg_df['hdga_cum'] = gsaa_avg_df['hdga'].cumsum(axis=0)
    gsaa_avg_df['hda_cum'] = gsaa_avg_df['hda'].cumsum(axis=0)
    gsaa_avg_df['mdga_cum'] = gsaa_avg_df['mdga'].cumsum(axis=0)
    gsaa_avg_df['mda_cum'] = gsaa_avg_df['mda'].cumsum(axis=0)
    gsaa_avg_df['ldga_cum'] = gsaa_avg_df['ldga'].cumsum(axis=0)
    gsaa_avg_df['lda_cum'] = gsaa_avg_df['lda'].cumsum(axis=0)
    print(gsaa_avg_df.head())
    gsaa_avg_df['run_hdsv_per'] = 1 - (gsaa_avg_df['hdga_cum'] /
                                       gsaa_avg_df['hda_cum'])

    gsaa_avg_df['run_mdsv_per'] = 1 - (gsaa_avg_df['mdga_cum'] /
                                       gsaa_avg_df['mda_cum'])
    gsaa_avg_df['run_ldsv_per'] = 1 - (gsaa_avg_df['ldga_cum'] /
                                       gsaa_avg_df['lda_cum'])

    print(gsaa_avg_df.head())
    gsaa_avg_df.game_date = pd.to_datetime(gsaa_avg_df.game_date)
    if len(player) == 1:
        goalie_query = select([database.c.goalie, database.c.game_date, database.c.hdga,
                               database.c.hda, database.c.mdga, database.c.mda,
                               database.c.ldga, database.c.lda]).\
                            where(and_(database.c.season == season,
                                database.c.goalie == player[0]))
    else:
        goalie_query = select([database.c.goalie, database.c.game_date, database.c.hdga,
                               database.c.hda, database.c.mdga, database.c.mda,
                               database.c.ldga, database.c.lda]).\
                            where(and_(database.c.season == season,
                                or_(database.c.goalie == player[0], database.c.goalie == player[1])))

    goalie_query_df = pd.read_sql(goalie_query, conn)
    print(goalie_query_df.head())
    goalie_query_df.game_date = pd.to_datetime(goalie_query_df.game_date)

    goalie_query_df = goalie_query_df.merge(gsaa_avg_df[['game_date', 'run_hdsv_per', 'run_mdsv_per', 'run_ldsv_per']], on='game_date')
    print(goalie_query_df.head())
    goalie_query_df['adj_gsaa'] = (goalie_query_df['hda'] * (1-goalie_query_df['run_hdsv_per'])
                                - goalie_query_df['hdga']) + \
                               (goalie_query_df['mda'] * (1-goalie_query_df['run_mdsv_per'])
                                - goalie_query_df['mdga']) + \
                               (goalie_query_df['lda'] * (1-goalie_query_df['run_ldsv_per'])
                                - goalie_query_df['ldga'])

    goalie_query_df['adj_gsaa30'] = (goalie_query_df['adj_gsaa'] /
                                  (goalie_query_df['hda'] + goalie_query_df['hdga'] +
                                   goalie_query_df['mda'] + goalie_query_df['mdga'] +
                                   goalie_query_df['lda'] + goalie_query_df['ldga'])
                                  * 30)

    goalie_query_df = goalie_query_df[['goalie', 'game_date', 'adj_gsaa', 'adj_gsaa30']]
    goalie_query_df = goalie_query_df.sort_values('game_date')

    goalie_query_df['moving_avg'] = goalie_query_df.groupby(goalie_query_df.columns[0])\
            [goalie_query_df.columns[2]].rolling(5).mean().reset_index(0, drop=True)

    print(goalie_query_df.head())

    return goalie_query_df

def query_creation(query_list):
    '''
    This function creates an sql query with SQL alchemy and then parses the
    results into a list of strings so the twitter bot can post them

    Inputs:
    query_list - list parsed from the original tweet from the user online is
                 the output of the query_parse() function.

    Outputs:
    data - a list formatted so the bot can post the results to twitter.
    '''

    engine = create_engine(os.environ.get('DB_CONNECT'))
    conn = engine.connect()
    metadata = MetaData()
    metadata.reflect(bind=engine)
    print(query_list)

    if len(query_list) == 3:
        database = metadata.tables[query_list[1]]
        if query_list[1][0:6] == 'player':
            sql_query = select([database.c.player,
                               (func.sum(database.c.cf) /
                                (func.sum(database.c.ca) +
                                   func.sum(database.c.cf))*100),
                               (func.sum(database.c.xgf) /
                                (func.sum(database.c.xga) +
                                   func.sum(database.c.xgf))*100),
                               func.sum(database.c.ixg)]).\
                        where(and_(database.c.player == query_list[0],
                              database.c.season ==
                              format_season(query_list[-1])))\
                        .group_by(database.c.player)
        elif query_list[1][0:4] == 'team':
            sql_query = select([database.c.team,
                               (func.sum(database.c.cf) /
                                (func.sum(database.c.ca) +
                                   func.sum(database.c.cf))*100),
                               (func.sum(database.c.xgf) /
                                (func.sum(database.c.xga) +
                                   func.sum(database.c.xgf))*100)]).\
                               where(and_(database.c.team == query_list[0],
                                     database.c.season ==
                                     format_season(query_list[-1])))\
                               .group_by(database.c.team)
        else:
            sql_query = select([database.c.goalie,
                               (1 - cast(func.sum(database.c.ga), Float) /
                                cast(func.sum(database.c.sa), Float)),
                               (1 - cast(func.sum(database.c.ldga), Float) /
                                cast(func.sum(database.c.lda), Float)),
                               (1 - cast(func.sum(database.c.mdga), Float) /
                                cast(func.sum(database.c.mda), Float)),
                               (1 - cast(func.sum(database.c.hdga), Float) /
                                cast(func.sum(database.c.hda), Float))]).\
                              where(and_(database.c.goalie == query_list[0],
                                    database.c.season ==
                                    format_season(query_list[-1])))\
                              .group_by(database.c.goalie)
            gsaa_df = gsaa_creation(format_season(query_list[-1]), [query_list[0]],
                                    database, conn)
            gsaa_df = gsaa_df.groupby(['goalie'])['adj_gsaa'].sum()

    elif len(query_list) == 4:
        database = metadata.tables[query_list[2]]
        if query_list[2][0:6] == 'player':
            sql_query = select([database.c.player,
                               (func.sum(database.c.cf) /
                                (func.sum(database.c.ca) +
                                   func.sum(database.c.cf))*100),
                               (func.sum(database.c.xgf) /
                                (func.sum(database.c.xga) +
                                   func.sum(database.c.xgf))*100),
                               func.sum(database.c.ixg)]).\
                        where(and_(or_(database.c.player == query_list[0],
                              database.c.player == query_list[1]),
                              database.c.season ==
                              format_season(query_list[-1]))).\
                        group_by(database.c.player)
        elif query_list[2][0:4] == 'team':
            sql_query = select([database.c.team,
                               (func.sum(database.c.cf) /
                                (func.sum(database.c.ca) +
                                   func.sum(database.c.cf))*100),
                               (func.sum(database.c.xgf) /
                                (func.sum(database.c.xga) +
                                   func.sum(database.c.xgf))*100)]).\
                        where(and_(or_(database.c.team == query_list[0],
                              database.c.team == query_list[1]),
                              database.c.season ==
                              format_season(query_list[-1]))).\
                        group_by(database.c.team)
        else:
            sql_query = select([database.c.goalie,
                               (1 - cast(func.sum(database.c.ga), Float) /
                                cast(func.sum(database.c.sa), Float)),
                               (1 - cast(func.sum(database.c.ldga), Float) /
                                cast(func.sum(database.c.lda), Float)),
                               (1 - cast(func.sum(database.c.mdga), Float) /
                                cast(func.sum(database.c.mda), Float)),
                               (1 - cast(func.sum(database.c.hdga), Float) /
                                cast(func.sum(database.c.hda), Float))]).\
                        where(and_(or_(database.c.goalie == query_list[0],
                              database.c.goalie == query_list[1]),
                              database.c.season ==
                              format_season(query_list[-1]))).\
                        group_by(database.c.goalie)
            gsaa_df = gsaa_creation(format_season(query_list[-1]),
                                    [query_list[0], query_list[1]],
                                    database, conn)
            gsaa_df = gsaa_df.groupby(['goalie'])['adj_gsaa'].sum()


    results = conn.execute(sql_query)

    results = list(map(list, results))
    print(results)
    print(gsaa_df.head())
    for x in results:
        if query_list[1][:6] == 'goalie' or query_list[2][:6] == 'goalie':
            rounded = [round(y, 3) for y in x[1:]]
            x[1:] = rounded
        else:
            rounded = [round(y, 2) for y in x[1:]]
            x[1:] = rounded

    data = []
    for row in results:
        data.append('{}'.format(str(row).replace('[', '').replace(',', '')
                    .replace(']', '').replace("'", '').replace('Decimal', '')))

    if query_list[1][:6] == 'goalie':
        data[0] += ' {}'.format(str(gsaa_df.loc[query_list[0]])[:5])
    elif query_list[2][:6] == 'goalie':
        data[0] += ' {}'.format(str(gsaa_df.loc[query_list[0]])[:5])
        data[1] += ' {}'.format(str(gsaa_df.loc[query_list[1]])[:5])
    conn.close()
    print(data)
    return data


def query_parse(text_list):
    '''
    Function to parse the original tweet from the online user and convert it
    to a list that will be turned into an sql query string.

    Inputs - a list of the orignal tweet components

    Outputs - a list formatted to be passed to the query_creation() function
    '''
    new_query = []
    if 'playerstats' in text_list and 'vs.' not in text_list:
        # append player name
        new_query.append('{}.{}'.format(text_list[1], text_list[2]).upper())
        # append database
        new_query.append(text_list[3].lower())
        # append year
        new_query.append(text_list[4])
    elif 'playerstats' in text_list and 'vs.' in text_list:
        # append first player name
        new_query.append('{}.{}'.format(text_list[1], text_list[2]).upper())
        # append second player name
        new_query.append('{}.{}'.format(text_list[4], text_list[5]).upper())
        # append database
        new_query.append(text_list[6].lower())
        # append year
        new_query.append(text_list[7])
    elif 'teamstats' in text_list and 'vs.' not in text_list:
        # append team name
        new_query.append(text_list[1].upper())
        # append database
        new_query.append(text_list[2].lower())
        # append year
        new_query.append(text_list[3])
    elif 'teamstats' in text_list and 'vs.' in text_list:
        # append first team name
        new_query.append(text_list[1].upper())
        # append second team name
        new_query.append(text_list[3].upper())
        # append database
        new_query.append(text_list[4].lower())
        # append year
        new_query.append(text_list[5])
    elif 'goaliestats' in text_list:
        if 'vs.' in text_list:
            # append first player name
            new_query.append('{}.{}'.format(text_list[1], text_list[2]).upper())
            # append second player name
            new_query.append('{}.{}'.format(text_list[4], text_list[5]).upper())
            # append database
            new_query.append(text_list[6].lower())
            # append year
            new_query.append(text_list[7])
        else:
            # append player name
            new_query.append('{}.{}'.format(text_list[1], text_list[2]).upper())
            # append database
            new_query.append(text_list[3].lower())
            # append year
            new_query.append(text_list[4])

    if 'vs.' in text_list:
        if '-adj' in text_list and '-5v5' in text_list:
            new_query[2] = '{}{}{}'.format(new_query[2], 'adj', '5v5')
        elif '-adj' in text_list and '-5v5' not in text_list:
            new_query[2] = '{}{}'.format(new_query[2], 'adj')
        elif '-adj' not in text_list and '-5v5' in text_list:
            new_query[2] = '{}{}'.format(new_query[2], '5v5')

    else:
        if '-adj' in text_list and '-5v5' in text_list:
            new_query[1] = '{}{}{}'.format(new_query[1], 'adj', '5v5')
        elif '-adj' in text_list and '-5v5' not in text_list:
            new_query[1] = '{}{}'.format(new_query[1], 'adj')
        elif '-adj' not in text_list and '-5v5' in text_list:
            new_query[1] = '{}{}'.format(new_query[1], '5v5')

    return new_query


def twitter_text_parser(data_text, status_text, season):
    '''
    function to format the data into a readable format for the replying tweet
    from the bot in this form:

    Season
    Player Name
    Stat: Value
    Stat: Value
    Stat: Value

    Inputs:
    data_text - string containg the data of the query
    status_text - text from the twitter query to function can tell if its
                  formatting for player stats or team stats
    season - string representing the season of the data queried

    Outputs:

    twitter_string - string that the api will tweet in response to the original
                     tweet containing data requested
    '''

    def three_name_adjuster(data_list):
        new_list = []
        if ' EK' in data_list or '.VAN ' in data_list:
            data_list = data_list.split(' ')
            new_list.append('{} {}'.format(data_list[0], data_list[1]))
            new_list.extend(data_list[2:])
            return new_list
        else:
            return data_list.split(' ')

    stat_conditions = ''
    if '-5v5' in status_text:
        stat_conditions += '5v5 '
    if '-adj' in status_text:
        stat_conditions += 'adj'

    twitter_string = '{} {}\n'.format(season, stat_conditions)
    if 'playerstats' in status_text:
        stats = ['CF%', 'xGF%', 'ixG']
        for data in data_text:
            data_list = three_name_adjuster(data)
            for stat, x in zip(stats, range(len(data_list[0:3]))):
                data_list[x+1] = '{}: {}'.format(stat, data_list[x+1])

            twitter_string += '\n'.join(data_list)
            twitter_string += '\n'
    elif 'teamstats' in status_text:
        stats = ['CF%', 'xGF%']
        for data in data_text:
            data_list = data.split(' ')
            for stat, x in zip(stats, range(len(data_list[0:2]))):
                data_list[x+1] = '{}: {}'.format(stat, data_list[x+1])

            twitter_string += '\n'.join(data_list)
            twitter_string += '\n'
    else:
        stats = ['SV%', 'LDSV%', 'MDSV%', 'HDSV%', 'adjGSAA']
        for data in data_text:
            data_list = three_name_adjuster(data)
            for stat, x in zip(stats, range(len(data_list[0:5]))):
                data_list[x+1] = '{}: {}'.format(stat, data_list[x+1])

            twitter_string += '\n'.join(data_list)
            twitter_string += '\n'

    return twitter_string


def graph_query_parse(query_text):

    '''
    This function parses the orginal text the user passed to the bot if they
    are requesting a graph of a stat.

    Inputs:
    query_text - a list containing each word of the tweet sent to the bot split
                 into a list.

    Outputs:
    parsed_query - a list of the elements needed to pass to the sql query
                   creation function.
    '''

    parsed_query = []
    if 'team' in query_text and 'vs.' in query_text:
        '''
        appending data in this order:
        team1, team2, stat to graph, year, database
        '''
        parsed_query.append(query_text[1].upper())
        parsed_query.append(query_text[3].upper())
        parsed_query.append(query_text[6].lower())
        parsed_query.append(query_text[7])
        parsed_query.append(query_text[4])
    elif 'team' in query_text and 'vs.' not in query_text:
        '''
        appending data in this order:
        team, stat to graph, year, database
        '''
        parsed_query.append(query_text[1].upper())
        parsed_query.append(query_text[4].lower())
        parsed_query.append(query_text[5])
        parsed_query.append(query_text[2])
    elif 'player' in query_text and 'vs.' in query_text:
        # append first player name
        parsed_query.append('{}.{}'.format(query_text[1],
                                           query_text[2]).upper())
        # append second player name
        parsed_query.append('{}.{}'.format(query_text[4],
                                           query_text[5]).upper())
        # append stat
        parsed_query.append(query_text[8].lower())
        # append year
        parsed_query.append(query_text[9])
        # append database
        parsed_query.append(query_text[6])
    elif 'player' in query_text and 'vs.' not in query_text:
        # append player name
        parsed_query.append('{}.{}'.format(query_text[1],
                                           query_text[2]).upper())
        # append stat
        parsed_query.append(query_text[5].lower())
        # append year
        parsed_query.append(query_text[6])
        # append database
        parsed_query.append(query_text[3])
    elif 'goalie' in query_text:
        if 'vs.' in query_text:
            # append first player name
            parsed_query.append('{}.{}'.format(query_text[1],
                                               query_text[2]).upper())
            # append second player name
            parsed_query.append('{}.{}'.format(query_text[4],
                                               query_text[5]).upper())
            # append stat
            parsed_query.append(query_text[8].lower())
            # append year
            parsed_query.append(query_text[9])
            # append database
            parsed_query.append(query_text[6])
        else:
            # append player name
            parsed_query.append('{}.{}'.format(query_text[1],
                                               query_text[2]).upper())
            # append stat
            parsed_query.append(query_text[5].lower())
            # append year
            parsed_query.append(query_text[6])
            # append database
            parsed_query.append(query_text[3])

    '''
    This part checks for the 5v5 and adjusted flags and then adds them onto
    the database so the right database table will be queried later
    '''
    if '-adj' in query_text and '-5v5' in query_text:
        parsed_query[-1] = '{}{}{}{}'.format(parsed_query[-1],
                                             'stats', 'adj', '5v5')
    elif '-adj' in query_text and '-5v5' not in query_text:
        parsed_query[-1] = '{}{}{}'.format(parsed_query[-1], 'stats', 'adj')
    elif '-adj' not in query_text and '-5v5' in query_text:
        parsed_query[-1] = '{}{}{}'.format(parsed_query[-1], 'stats', '5v5')
    else:
        parsed_query[-1] = '{}{}'.format(parsed_query[-1], 'stats')

    print(parsed_query)
    return parsed_query


def format_season(number):
    number_less = int(number) - 1
    number = str(number_less) + str(number)
    return number


def graph_query_creation(query_list):
    '''
    Function to turn the parsed twitter query into a dataframe to plot moving
    averages that are requesting graphs

    Inputs:
    query_list - list containing keywords from parsed twitter query

    Outputs:
    query_df - dataframe created by the SQL Alchemy select statement with
               moving average calculated
    '''

    rate_stats = ['cf60', 'ff60', 'fa60', 'ca60', 'ixg', 'ixg60', 'xgf60',
                  'xga60', 'g60', 'a160', 'gf60', 'ga60']
    engine = create_engine(os.environ.get('DB_CONNECT'))
    conn = engine.connect()
    metadata = MetaData()
    metadata.reflect(bind=engine)

    if len(query_list) == 4:
        database = metadata.tables[query_list[3]]
        if query_list[-1][:6] == 'player':
            sql_query = select([database.c.player, database.c.game_date,
                               database.c[query_list[1]]]).\
                        where(and_(database.c.player == query_list[0],
                              database.c.season ==
                              format_season(query_list[-2])))
        elif query_list[-1][:4] == 'team':
            sql_query = select([database.c.team, database.c.game_date,
                               database.c[query_list[1]]]).\
                        where(and_(database.c.team == query_list[0],
                              database.c.season ==
                              format_season(query_list[-2])))
        else:
            if query_list[1] != 'gsaa':
                sql_query = select([database.c.goalie, database.c.game_date,
                                   database.c[query_list[1]]]).\
                            where(and_(database.c.goalie == query_list[0],
                                  database.c.season ==
                                  format_season(query_list[-2]),
                                  cast(database.c[query_list[1]], String) != 'NaN')
                                  )
                print(sql_query)
                average = conn.execute(select
                                       ([func.avg(database.c[query_list[1]])]).
                                       where(and_(database.c.season ==
                                             format_season(query_list[-2]),
                                             cast(database.c[query_list[1]],
                                                  String) != 'NaN')
                                             ))
                average = list(average)
                print(average)
                average = '{}'.format(str(average).
                                      replace('(', '').replace(',', '')
                                      .replace(')', '').replace("'", '')
                                      .replace('Decimal', '').replace('[', '').
                                      replace(']', ''))
                print(average)
                average = float(average)
                query_df = pd.read_sql_query(sql_query, conn)
                query_df = query_df.sort_values('game_date')

                query_df['moving_avg'] = query_df.groupby(query_df.columns[0])\
                        [query_df.columns[2]].rolling(5).mean().reset_index(0, drop=True)
            else:
                print(query_list)
                query_df =  gsaa_creation(format_season(query_list[-2]),
                                        [query_list[0]],
                                        database, conn)
                average = 0

    elif len(query_list) == 5:
        database = metadata.tables[query_list[-1]]
        if query_list[-1][:6] == 'player':
            sql_query = select([database.c.player, database.c.game_date,
                               database.c[query_list[2]]]).\
                        where(and_(or_(database.c.player == query_list[0],
                              database.c.player == query_list[1]),
                              database.c.season ==
                              format_season(query_list[-2])))
        elif query_list[-1][:4] == 'team':
            sql_query = select([database.c.team, database.c.game_date,
                               database.c[query_list[2]]]).\
                        where(and_(or_(database.c.team == query_list[0],
                              database.c.team == query_list[1]),
                              database.c.season ==
                              format_season(query_list[-2])))
        else:
            if query_list[2] != 'gsaa':
                sql_query = select([database.c.goalie, database.c.game_date,
                                   database.c[query_list[2]]]).\
                            where(and_(or_(database.c.goalie == query_list[0],
                                  database.c.goalie == query_list[1]),
                                  database.c.season ==
                                  format_season(query_list[-2]),
                                  cast(database.c[query_list[2]], String) != 'NaN')
                                  )
                print(sql_query)
                average = conn.execute(select
                                       ([func.avg(database.c[query_list[2]])]).
                                       where(and_(database.c.season ==
                                             format_season(query_list[-2]),
                                             cast(database.c[query_list[2]],
                                                  String) != 'NaN')
                                             ))
                average = list(average)
                print(average)
                average = '{}'.format(str(average)
                                      .replace('(', '')
                                      .replace(',', '')
                                      .replace(')', '')
                                      .replace("'", '')
                                      .replace('Decimal', '')
                                      .replace('[', '').
                                      replace(']', ''))
                print(average)
                average = float(average)

                query_df = pd.read_sql_query(sql_query, conn)
                query_df = query_df.sort_values('game_date')

                query_df['moving_avg'] = query_df.groupby(query_df.columns[0])\
                        [query_df.columns[2]].rolling(5).mean().reset_index(0, drop=True)
            else:
                query_df = gsaa_df = gsaa_creation(format_season(query_list[-2]),
                                        [query_list[0], query_list[1]],
                                        database, conn)
                average = 0


    if query_list[-1][:6] != 'goalie':
        if len(query_list) == 5:
            average = conn.execute(select
                                   ([func.avg(database.c[query_list[2]])]).
                                   where(and_(database.c.season ==
                                         format_season(query_list[-2]),
                                         cast(database.c[query_list[2]],
                                              String) != 'NaN')
                                         ))
        else:
            average = conn.execute(select
                                   ([func.avg(database.c[query_list[1]])]).
                                   where(and_(database.c.season ==
                                         format_season(query_list[-2]),
                                         cast(database.c[query_list[1]],
                                              String) != 'NaN')
                                         ))
        average = list(average)
        print(average)
        average = '{}'.format(str(average)
                              .replace('(', '')
                              .replace(',', '')
                              .replace(')', '')
                              .replace("'", '')
                              .replace('Decimal', '')
                              .replace('[', '').
                              replace(']', ''))
        print(average)
        average = float(average)

        query_df = pd.read_sql_query(sql_query, conn)
        query_df = query_df.sort_values('game_date')

        query_df['moving_avg'] = query_df.groupby(query_df.columns[0])\
                [query_df.columns[2]].rolling(5).mean().reset_index(0, drop=True)

    return query_df, average


def graph_creation(dataframe, graph_query, average, status_text):
    '''
    This function takes a dataframe and plots it based on the groups in either
    the player or team column depending on the orginal query from the user

    Inputs:
    dataframe - dataframe created by the graph_database_query() function will
                have columns consisting of player/team, game_date, stat
                queried, and the 5 game moving average of that statistic.

    Outputs:
    file_name - this is :a .png file of the graph created by plotting the
                moving average of the user requested statistic against the
                dates of the games played. This image file will then be
                uploaded to the API and attached to the replying tweet by
                the bot
    '''

    percent_stats = ['cf_percent', 'ff_percent', 'xgf_percent']
    # group the dataframe by player if there is only one it just
    # the same dataframe as we passed to it
    grouped = dataframe.groupby(dataframe.columns[0])
    # create the base plot
    fig, ax = plt.subplots(figsize=(16, 6))

    # plot the moving averages of each player/players
    for key, group in grouped:
        group.plot(x='game_date', y='moving_avg', label=key, ax=ax)

# add this to let people know if graph is adjusted and 5v5
    stat_conditions = ''
    if '-5v5' in status_text:
        stat_conditions += '5v5 '
    if '-adj' in status_text:
        stat_conditions += 'adj'
    # create labels for the graph
    if len(graph_query) == 5:
        plt.title('{} & {} {} {} 5 game rolling average {} by @BarloweAnalytic'
                  .format(graph_query[0], graph_query[1], graph_query[3],
                          dataframe.columns[2], stat_conditions))
    else:
        plt.title('{} {} {} 5 game rolling average {} by @BarloweAnalytic'
                  .format(graph_query[0], graph_query[2],
                          dataframe.columns[2], stat_conditions))

    # label the y axis
    plt.ylabel(dataframe.columns[2])
    # remove right and upper graph boundaries
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    # If the graph is of cf%, ff%, gf%, or xgf% add line for 50% mark for
    # everything else add line for league average
    if dataframe.columns[2] in percent_stats:
        plt.axhline(y=50, color='k', linestyle='--')
    else:
        plt.axhline(y=average, color='k', linestyle='--',
                    label='NHL avg: {}'.format(str(average)[:6]))
        plt.legend()
    ax.grid(alpha=.5)
    file_name = 'plot{}.png'.format(str(random.randint(1, 1000)))
    fig.savefig(file_name)

    return file_name


def three_name_parser(status_list):
    new_list = []
    status_list = list(map(str.lower, status_list))
    if ' ek ' in status_list or ' van ' in status_list or 'di' in status_list:
        new_list.extend(status_list[:2])
        new_list.append('{} {}'.format(status_list[2], status_list[3]))
        new_list.extend(status_list[4:])
        return new_list
    else:
        return status_list


class BotStreamer(tweepy.StreamListener):
    '''
    This class inherits from tweepy's StreamListener class with the on_status()
    function changed to create the output neccesary to answer the users data
    query.  I also pass it the bots twitter api so it can then respond to those
    queries with the appropriate data.
    '''
    def __init__(self, api):
        self.api = api

    # Called when a new status arrives which is passed down from the on_data
    # method of the StreamListener
    def on_status(self, status):
        # gets username of the person who tweeted bot
        username = status.user.screen_name
        # stores status.id of the query tweet to the bot knows
        # what tweet to reply to
        status_id = status.id
        # stores text of tweet and splits it so that the bot can further
        # process it
        status = status.text
        status = status.split(' ')
        if text_error_check(status):
            return

        if username == 'CJTDevil':
            try:
                self.api.update_status(status='@{}\n You have insulted'
                                       'my mighty creator the God Emperor'
                                       'before you can use the bot again you'
                                       'must repent for choosing me over'
                                       'him and seek supplication for your'
                                       'sins.'.format(username),
                                       in_reply_to_status_id=status_id)
            except Exception as ex:
                print(ex)
                return

        try:
            if 'graph' in status:
                status = three_name_parser(status)
                query = graph_query_parse(status)
                graph_df, average = graph_query_creation(query)
                graph_name = graph_creation(graph_df, query, average, status)
                self.api.update_with_media(graph_name,
                                           status='@{}'.format(username),
                                           in_reply_to_status_id=status_id)
                os.remove(graph_name)

            else:
                status = three_name_parser(status)
                query = query_parse(status)
                query_text = query_creation(query)
                tweet_text = twitter_text_parser(query_text, status, query[-1])
                self.api.update_status(status='@{}\n{}'.format(username,
                                                               tweet_text),
                                       in_reply_to_status_id=status_id)

        except Exception as ex:
            print(ex)
            return


def main():
    '''
    Script to run a stream listener for the @barloweanalytic tiwtter bot
    so it can catch when people tweet it requests and then respond with the
    appropriate statistics

    Input:
    sys.argv[1] - the text file containing the API keys for the twitter bot

    Outputs:
    None
    '''
    # this function gets the twitter api keys from the text file they
    # are stored in the contents of the function can be found in the
    # keylesstwitter.py file on github
    twitter_keys = twitterpost.get_twitter_keys(sys.argv[1])
    print(twitter_keys['Consumer Key'])
    auth = tweepy.OAuthHandler(twitter_keys['Consumer Key'],
                               twitter_keys['Consumer Secret Key'])
    auth.set_access_token(twitter_keys['Access Key'],
                          twitter_keys['Access Secret Key'])
    api = tweepy.API(auth)

    myStreamListener = BotStreamer(api)
# Construct the Stream instance using the BotStreamer class as the listener and
# set it to track whenever people tweet at it
    stream = tweepy.Stream(auth=api.auth, listener=myStreamListener)
    wait_time = 100
    while True:
        try:
            stream.filter(track=['@barloweanalytic'])
        except Exception as ex:
            time.sleep(wait_time)
            wait_time += 60
            if wait_time > 500:
                wait_time = 0


if __name__ == '__main__':
    main()
