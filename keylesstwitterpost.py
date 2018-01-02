import time
import sys
import datetime
import tweepy

def get_twitter_keys(key_file):
    '''
    Funciton to get the twitter bot's api keys from a text file

    Input:
    key_file - text file holding api auth keys

    Output:
    keys_dict - dictionary with key names mapped to keys
    '''

    twitter_keys = key_file
    keys_dict = {}

    with open(twitter_keys, 'r') as keys:
        api_keys = []

        for line in keys:
            api_keys.append(line)

        api_keys = list(map(str.strip, api_keys))

        for key in api_keys:
            name_list = key.split(':')
            name_list = list(map(str.strip, name_list))
            key_name, key_value = name_list[0], name_list[1]
            keys_dict[key_name] = key_value
        return keys_dict

def top5_post(bot_api, top5_file):
    '''
    Function to post the top five players from the day before in Individual
    Expected Goals

    Input:
    bot_api - twitter api to pass updates to
    top5_file - text file holding the top five players from the day before
    by Individual Expected Goals (ixG)

    Output:
    None
    '''

    top5_players = ''
    for line in top5_file:
        top5_players += line


    bot_api.update_status(status = top5_players)

def daily_games_post(bot_api, games_file, date):
    '''
    Function to post the xG stats from the NHL games the day before

    Input:
    bot_api - twitter api to pass updates to
    games_file - txt file containing NHL game ids, game results in goals and
    xG to post on twitter

    Output:
    None
    '''
    #parse games file and seperate into individual lists for each game
    games_text = []
    for line in games_file.readlines():
        games_text.append(line)

    #Checks to see if games were played and if not ends function execution
    if games_text[0].strip() == "No games today":
        print("No games today")
        return False

    games_text = [games_text[x:x+3] for x in range(0, len(games_text), 3)]

    for game in games_text:
        # upload images and get media_ids
        filenames = ['xGAway.png', 'xGHome.png', 'xGlocations.png', 'RunningxG5v5.png']
        media_ids = []
        for filename in filenames:
            res = bot_api.media_upload('/Users/MattBarlowe/HockeyStuff/xGGameBreakdowns/2018/{}/{}'.\
                    format(game[0][:5], filename))
            media_ids.append(res.media_id)
        # tweet with multiple images
        bot_api.update_status(status= '{}{}{}'.format(game[1],
            game[2], date.strftime('%m-%d-%Y')), media_ids=media_ids)

        #to avoid overwhelming twitter api
        time.sleep(30)

    return True

def main():
    '''
    Pulls in game data from the night before and posts xg graphs for each game
    and the top5 players in ixG

    Inputs:
    sys.arg[1] - twitter api key file
    sys.arg[2] - daily game results file
    sys.arg[3] - daily top 5 players in ixG file

    Ouputs:
    None
    '''

    date = datetime.datetime.now() - datetime.timedelta(days=1)
    twitter_keys = get_twitter_keys(sys.argv[1])
    games = open(sys.argv[2], 'r')
    top5 = open(sys.argv[3], 'r')

    #set twitter API
    auth = tweepy.OAuthHandler(twitter_keys['Consumer Key'],
            twitter_keys['Consumer Secret Key'])
    auth.set_access_token(twitter_keys['Access Key'],
            twitter_keys['Access Secret Key'])
    api = tweepy.API(auth)


    games_today = daily_games_post(api, games, date)
    #checks to see if games where played yesterday if not it pass on posting
    #top 5 ixg players tweet
    if not games_today:
        print("No games today")
        pass
    else:
        top5_post(api, top5)
        print("Yesterday's gaes posted")

    games.close()
    top5.close()

if __name__ == '__main__':
    main()
