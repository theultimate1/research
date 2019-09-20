import tweepy
import json

consumer_token="1EbEbqZpTKOsGo46iIlMLVZV3"
consumer_secret="MsaVvkSGVUkwkAalHfqNxSY6raOSjkn9e9uDmgf9a7e5a63ZBC"
auth=tweepy.OAuthHandler(consumer_token,consumer_secret)
auth.set_access_token("1153053655903428608-ERZK04gJccvOmrWrBXMY8MTLbvnSDC","4fwZlaNOhdLP0JA20ASwehJLsnIPJhXteOTEWeSPQ1cvW" )
class MyStreamListener(tweepy.StreamListener):
    
    ntweets=0
    def isReply(self,tweet):
        if ( tweet.retweeted_status
        or tweet.in_reply_to_status_id
        or tweet.in_reply_to_status_id_str
        or tweet.in_reply_to_user_id
        or tweet.in_reply_to_user_id_str
        or tweet.in_reply_to_screen_name ):
            return True
    def on_data(self,data):
        print(data)
        if not self.isReply(data):
            MyStreamListener.ntweets+=1
            with open("tweets_pollutionORwater.json","a",encoding='utf-8') as json_f:
                json.dump(data,json_f,indent=4)
            print("collected {} tweets".format(MyStreamListener().ntweets))
            return True
        else:
            pass
    def on_error(self, status_code):
        if status_code == 420:
            #returning False in on_data disconnects the stream
            return False

    def on_status(self, status):
        print(status.text)
Listener=MyStreamListener()
myStream = tweepy.Stream(auth =auth, listener=Listener)
myStream.filter(track="pollution OR water",is_async=True,languages=["en"])
