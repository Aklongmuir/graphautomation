import keylesstwitterpost as twitterpost
import tweepy
import sys
import psycopg2
import pandas as pd
import matplotlib.pyplot as plt
import random
import os

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

def query_creation(query_list):
    '''
    This function creates an sql query to pass to the SQL db to collect the stats
    for teams and players.  This will be passed to the database_query() function.

    Inputs:
    query_list - list parsed from the original tweet from the user online is
                 the output of the query_parse() function.

    Outputs:
    sql_query - a string formatted to be a proper SQL query.
    '''
    if len(query_list) == 3:
        if query_list[1][0:6] == 'player':
            sql_query = 'SELECT player, ROUND((SUM(cf)/(SUM(cf)+sum(ca)))::NUMERIC, 4) * 100 as CF_Percent,'\
                        ' ROUND((SUM(xgf)/(SUM(xgf) + SUM(xga)))::NUMERIC, 4) * 100 as xGF_Percent, '\
                        'ROUND(SUM(ixg)::NUMERIC, 4)'\
                        ' FROM {} WHERE player = \'{}\' AND season = \'{}\' group by player;'.format(query_list[1],
                        query_list[0], format_season(query_list[-1]))
        else:
            sql_query = 'SELECT team, ROUND((SUM(cf)/(SUM(cf)+sum(ca)))::NUMERIC, 4) * 100 as CF_Percent,'\
                        ' ROUND((SUM(xgf)/(SUM(xgf) + SUM(xga)))::NUMERIC, 4) * 100 as xGF_Percent'\
                        ' FROM {} WHERE team = \'{}\' AND season = \'{}\' group by team;'.format(query_list[1],
                        query_list[0], format_season(query_list[-1]))
    elif len(query_list) == 4:
        if query_list[2][0:6] == 'player':
            sql_query = 'SELECT player, ROUND((SUM(cf)/(SUM(cf)+sum(ca)))::NUMERIC, 4) * 100 as CF_Percent,'\
                        ' ROUND((SUM(xgf)/(SUM(xgf) + SUM(xga)))::NUMERIC, 4) * 100 as xGF_Percent,'\
                        'ROUND(SUM(ixg)::NUMERIC, 4)'\
                        ' FROM {} WHERE (player = \'{}\' or player = \'{}\') AND (season = \'{}\') group by player;'.format(query_list[2],
                        query_list[0], query_list[1], format_season(query_list[-1]))
        else:
            sql_query = 'SELECT team, ROUND((SUM(cf)/(SUM(cf)+sum(ca)))::NUMERIC, 4) * 100 as CF_Percent,'\
                        ' ROUND((SUM(xgf)/(SUM(xgf) + SUM(xga)))::NUMERIC, 4) * 100 as xGF_Percent'\
                        ' FROM {} WHERE (team = \'{}\' or team = \'{}\') AND (season = \'{}\') group by team;'.format(query_list[2],
                        query_list[0], query_list[1], format_season(query_list[-1]))

    return sql_query


def query_parse(text_list):
    '''
    Function to parse the original tweet from the online user and convert it
    to a list that will be turned into an sql query string.

    Inputs - a list of the orignal tweet components

    Outputs - a list formatted to be passed to the query_creation() function
    '''
    new_query = []
    if 'playerstats' in text_list and 'vs.' not in text_list:
        #append player name
        new_query.append('{}.{}'.format(text_list[1], text_list[2]).upper())
        #append database
        new_query.append(text_list[3].lower())
        #append year
        new_query.append(text_list[4])
    elif 'playerstats' in text_list and 'vs.' in text_list:
        #append first player name
        new_query.append('{}.{}'.format(text_list[1], text_list[2]).upper())
        #append second player name
        new_query.append('{}.{}'.format(text_list[4], text_list[5]).upper())
        #append database
        new_query.append(text_list[6].lower())
        #append year
        new_query.append(text_list[7])
    elif 'teamstats' in text_list and 'vs.' not in text_list:
        #append team name
        new_query.append(text_list[1].upper())
        #append database
        new_query.append(text_list[2].lower())
        #append year
        new_query.append(text_list[3])
    elif 'teamstats' in text_list and 'vs.' in text_list:
        #append first team name
        new_query.append(text_list[1].upper())
        #append second team name
        new_query.append(text_list[3].upper())
        #append database
        new_query.append(text_list[4].lower())
        #append year
        new_query.append(text_list[5])

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

def database_query(query):
    '''
    This function queries the database for statistical queries only and then
    formats the data into a list for the twiiter API to post to answer
    the query of the online user.

    Inputs - a list that is the output of the query_parse() function

    Outputs - a list that is stripped of the excess characters that are the
              result of querying the sql database this will be passed to the
              twitter_text_parser() function.
    '''

    conn = psycopg2.connect(os.environ.get('DB_CONNECT'))
    cur = conn.cursor()
    cur.execute(query)
    rows = cur.fetchall()
    data = []
    for row in rows:
        data.append('{}'.format(str(row).replace('(', '').replace(',', '')\
                .replace(')', '').replace("'", '').replace('Decimal', '')))
    conn.close()
    return data

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

    twitter_string = '{}\n'.format(season)
    if 'playerstats' in status_text:
        stats = ['CF%', 'xGF%', 'ixG']
        for data in data_text:
            data_list = three_name_adjuster(data)
            for stat, x in zip(stats, range(len(data_list[0:3]))):
                data_list[x+1] = '{}: {}'.format(stat, data_list[x+1][:-2])

            twitter_string += '\n'.join(data_list)
            twitter_string += '\n'
    else:
        stats = ['CF%', 'xGF%']
        for data in data_text:
            data_list = data.split(' ')
            for stat, x in zip(stats, range(len(data_list[0:2]))):
                data_list[x+1] = '{}: {}'.format(stat, data_list[x+1][:-2])

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
    parsed_query - a list of the elements needed to pass to the sql query creation
                   function.
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
        #append first player name
        parsed_query.append('{}.{}'.format(query_text[1], query_text[2]).upper())
        #append second player name
        parsed_query.append('{}.{}'.format(query_text[4], query_text[5]).upper())
        #append stat
        parsed_query.append(query_text[8].lower())
        #append year
        parsed_query.append(query_text[9])
        #append database
        parsed_query.append(query_text[6])
    elif 'player' in query_text and 'vs.' not in query_text:
        #append player name
        parsed_query.append('{}.{}'.format(query_text[1], query_text[2]).upper())
        #append stat
        parsed_query.append(query_text[5].lower())
        #append year
        parsed_query.append(query_text[6])
        #append database
        parsed_query.append(query_text[3])

    '''
    This part checks for the 5v5 and adjusted flags and then adds them onto
    the database so the right database table will be queried later
    '''
    if '-adj' in query_text and '-5v5' in query_text:
        parsed_query[-1] = '{}{}{}{}'.format(parsed_query[-1], 'stats', 'adj', '5v5')
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
    Function to turn the parsed twitter query into an sql query for tweets
    that are requesting graphs

    Inputs:
    query_list - list containing keywords from parsed twitter query

    Outputs:
    sql_query - string representing the sql query needed to select the
                appropriate data for the requested graph
    '''

    if len(query_list) == 4:
        if query_list[-1][:6] == 'player':
            sql_query = 'SELECT player, game_date, {} from {} where ' \
                        'player = \'{}\' AND season = \'{}\';'\
                        .format(query_list[1], query_list[3],\
                        query_list[0], format_season(query_list[-2]))
        else:
            sql_query = 'SELECT team, game_date, {} from {} where ' \
                        'team = \'{}\' AND season = \'{}\';'\
                        .format(query_list[1], query_list[3],\
                        query_list[0], format_season(query_list[-2]))
    elif len(query_list) == 5:
        if query_list[-1][:6] == 'player':
            sql_query = 'SELECT player, game_date, {} from {} where ' \
                        '(player = \'{}\' or player = \'{}\') AND (season = \'{}\');'\
                        .format\
                        (query_list[2], query_list[-1], query_list[0], \
                        query_list[1], format_season(query_list[-2]))
        else:
            sql_query = 'SELECT team, game_date, {} from {} where ' \
                        '(team = \'{}\' or team = \'{}\') AND (season = \'{}\');'\
                        .format\
                        (query_list[2], query_list[-1], query_list[0], \
                        query_list[1], format_season(query_list[-2]))

    return sql_query

def graph_database_query(query_string):
    '''
    function to query the database and return the results as a pandas dataframe

    Inputs:
    query_string - string consisting of the sql query to execute this is the
                   output of the graph_query_creation() function.

    Outputs:
    query_df - SQL query results stored in a pandas dataframe this will be the
               input of the graph_creation() function
    '''
    conn = psycopg2.connect("host=localhost dbname=nhl user=matt")
    query_df = pd.read_sql_query(query_string, conn)
    query_df = query_df.sort_values('game_date')


    query_df['moving_avg'] = query_df.groupby(query_df.columns[0])\
            [query_df.columns[2]]\
            .rolling(5).mean().reset_index(0, drop = True)

    print(query_df.head())
    return query_df

def graph_creation(dataframe, graph_query):
    '''
    This function takes a dataframe and plots it based on the groups in either
    the player or team column depending on the orginal query from the user

    Inputs:
    dataframe - dataframe created by the graph_database_query() function will
                have columns consisting of player/team, game_date, stat queried,
                and the 5 game moving average of that statistic.

    Outputs:
    file_name - this is a .png file of the graph created by plotting the moving
                average of the user requested statistic against the dates of the
                games played. This image file will then be uploaded to the API
                and attached to the replying tweet by the bot
    '''

    percent_stats = ['cf_percent', 'ff_percent', 'xgf_percent']
    dataframe.head()
    grouped = dataframe.groupby(dataframe.columns[0])
    fig, ax = plt.subplots(figsize = (16,6))

    for key, group in grouped:
        group.plot(x = 'game_date', y = 'moving_avg', label = key, ax = ax)

    if len(graph_query) == 5:
        plt.title('{} & {} {} {} 5 game rolling average by @BarloweAnalytic'\
                .format(graph_query[0], graph_query[1], graph_query[3], dataframe.columns[2]))
    else:
        plt.title('{} {} {} 5 game rolling average by @BarloweAnalytic'\
                .format(graph_query[0], graph_query[2], dataframe.columns[2]))

    plt.ylabel(dataframe.columns[2])
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    if dataframe.columns[2] in percent_stats:
        plt.axhline(y = 50, color = 'k', linestyle = '--')
    ax.grid(alpha = .5)
    file_name = 'plot{}.png'.format(str(random.randint(1,1000)))
    fig.savefig(file_name)

    return file_name

def three_name_parser(status_list):
    new_list = []
    status_list = list(map(str.lower, status_list))
    if ' ek ' in status_list or ' van ' in status_list:
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

# Called when a new status arrives which is passed down from the on_data method of the StreamListener
    def on_status(self, status):
#gets username of the person who tweeted bot
        username = status.user.screen_name
#stores status.id of the query tweet to the bot knows what tweet to reply to
        status_id = status.id
#stores text of tweet and splits it so that the bot can further process it
        status = status.text
        status = status.split(' ')
        if text_error_check(status):
            return

        if username == 'CJTDevil':
            try:
                self.api.update_status(status =  '@{}\n You have insulted'\
                        'my mighty creator the God Emperor before you can use'\
                        'the bot again you must repent for choosing me over'\
                        'him and seek supplication for your sins.'.format(\
                        username,tweet_text),\
                        in_reply_to_status_id = status_id)
            except:
                return


        try:
            if 'graph' in status:
                status = three_name_parser(status)
                query = graph_query_parse(status)
                graph_query_text = graph_query_creation(query)
                graph_df = graph_database_query(graph_query_text)
                graph_name = graph_creation(graph_df, query)
                self.api.update_with_media(graph_name, status = '@{}'.format(username),\
                        in_reply_to_status_id = status_id)
                os.remove(graph_name)

            else:
                status = three_name_parser(status)
                query = query_parse(status)
                query_text = query_creation(query)
                returned_data = database_query(query_text)
                tweet_text = twitter_text_parser(returned_data, status, query[-1])
                self.api.update_status(status =  '@{}\n{}'.format(username,tweet_text),\
                       in_reply_to_status_id = status_id)

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
#this function gets the twitter api keys from the text file they are stored in
#the contents of the function can be found in the keylesstwitter.py file on
#github
    twitter_keys = twitterpost.get_twitter_keys(sys.argv[1])
    auth = tweepy.OAuthHandler(twitter_keys['Consumer Key'],
            twitter_keys['Consumer Secret Key'])
    auth.set_access_token(twitter_keys['Access Key'],
            twitter_keys['Access Secret Key'])
    api = tweepy.API(auth)


    myStreamListener = BotStreamer(api)
# Construct the Stream instance using the BotStreamer class as the listener and
#set it to track whenever people tweet at it
    stream = tweepy.Stream(auth = api.auth, listener = myStreamListener)
    stream.filter(track=['@barloweanalytic'])

if __name__ == '__main__':
    main()
