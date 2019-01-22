# file: DrinkerBot.py
# author: Kyle Campbell
# 
# description: This python code creates a twitter stream connection
#              within a location bounding box then pulls tweets with
#              certain keywords. This runs on AWS EC2 instance and 
#              writes scraped tweets to a csv file.


from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream
from tweepy import API
import json, time, string, re, csv
from credentials import *
from keywords import keywords

class DrinkListener(StreamListener):
    """Twitter data received..."""

    def on_status(self, status):
        # open csv for appending
        csvFile = open(self.filename, 'a')
        csvWriter = csv.writer(csvFile)

        # exclude retweets
        if not 'RT @' in status.text:
            # watch for keywords
            if any(word in status.text.lower() for word in keywords):
                print(status.text)
                print("\n")
                try:
                    csvWriter.writerow([status.text,
                                        status.created_at,
                                        status.geo,
                                        status.lang,
                                        status.place,
                                        status.coordinates,
                                        status.user.favourites_count,
                                        status.user.statuses_count,
                                        status.user.description,
                                        status.user.location,
                                        status.user.id,
                                        status.user.created_at,
                                        status.user.verified,
                                        status.user.following,
                                        status.user.url,
                                        status.user.listed_count,
                                        status.user.followers_count,
                                        status.user.default_profile_image,
                                        status.user.utc_offset,
                                        status.user.friends_count,
                                        status.user.default_profile,
                                        status.user.name,
                                        status.user.lang,
                                        status.user.screen_name,
                                        status.user.geo_enabled,
                                        status.user.profile_background_color,
                                        status.user.profile_image_url,
                                        status.user.time_zone,
                                        status.id,
                                        status.favorite_count,
                                        status.retweeted,
                                        status.source,
                                        status.favorited,
                                        status.retweet_count])

                except Exception as e:
                    print(e)
                    pass

        csvFile.close()
            
    def on_error(self, status):
        print('Error: %s' % status)
        return True
        
    def on_timeout(self):
        print('Timed out...')

        
class DrinkerBot(DrinkListener):
    
    def __init__(self):
        super(DrinkerBot, self).__init__()
        
        auth = OAuthHandler(consumer_key, consumer_secret)
        auth.set_access_token(access_token, access_token_secret)

        # Open handle to REST API
        self.api = API(auth)
        self.api.retry_count = 2
        self.api.retry_delay = 5

        me = self.api.me()
        self.my_id = me.id
        self.my_name = me.screen_name
        print('Name: %s id: %s' % (self.my_name, self.my_id))

        # Open handle to Streaming API
        self.stream = Stream(auth, self, retry_count=2)

        # create data file
        self.filename = 'data'+'_'+time.strftime('%Y%m%d-%H%M%S')+'.csv'
        csvFile = open(self.filename, 'w')
        csvWriter = csv.writer(csvFile)

        # initialize csv with headers
        csvWriter.writerow(['text',
                            'created_at',
                            'geo',
                            'lang',
                            'place',
                            'coordinates',
                            'user.favourites_count',
                            'user.statuses_count',
                            'user.description',
                            'user.location',
                            'user.id',
                            'user.created_at',
                            'user.verified',
                            'user.following',
                            'user.url',
                            'user.listed_count',
                            'user.followers_count',
                            'user.default_profile_image',
                            'user.utc_offset',
                            'user.friends_count',
                            'user.default_profile',
                            'user.name',
                            'user.lang',
                            'user.screen_name',
                            'user.geo_enabled',
                            'user.profile_background_color',
                            'user.profile_image_url',
                            'user.time_zone',
                            'id',
                            'favorite_count',
                            'retweeted',
                            'source',
                            'favorited',
                            'retweet_count'])
        
    def run(self):
        while True:
            print('Connecting to stream...\n')
            try:
                # San Diego bounding box coordinates
                self.stream.filter(locations=[-117.40,32.53,-116.54,33.24])
            except Exception as e:
                print('Exception from stream: ')
                print(e)
            time.sleep(5)

        
if __name__ == '__main__':
    try:
        DrinkerBot().run()
    except KeyboardInterrupt:
        pass
