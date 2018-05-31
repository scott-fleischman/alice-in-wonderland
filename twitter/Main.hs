{-# LANGUAGE TypeApplications #-}

import qualified Alice.Tweets
import qualified Data.Aeson as Aeson
import qualified System.Random.Shuffle as Random.Shuffle

main :: IO ()
main = do
  eitherTweetList <- Aeson.eitherDecodeFileStrict @Alice.Tweets.TweetList "data/tweets.json"
  Alice.Tweets.TweetList tweetThreads <-
    case eitherTweetList of
      Left err -> error err
      Right result -> return result
  randomTweets <- Random.Shuffle.shuffleM tweetThreads
  mapM_ print $ take 10 randomTweets
