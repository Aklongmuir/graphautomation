import sys
import datetime
import tweepy

def get_twitter_keys(key_file):
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

def main():
    twitter_keys = get_twitter_keys(sys.argv[1])
    print(twitter_keys)

    auth = tweepy.OAuthHandler(twitter_keys['Consumer Key'],
            twitter_keys['Consumer Secret Key'])
    auth.set_access_token(twitter_keys['Access Key'],
            twitter_keys['Access Secret Key'])
    api = tweepy.API(auth)

if __name__ == '__main__':
    main()
