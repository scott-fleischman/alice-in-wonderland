{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Alice.Tweets
import qualified Control.Logging as Logging
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString.Char8
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified InlineTweets
import qualified System.Environment as Environment
import qualified System.Random.Shuffle as Random.Shuffle
import qualified Web.Twitter.Conduit as Twitter.Conduit

main :: IO ()
main = Logging.withStdoutLogging $ do
  let
    inlineTweets = InlineTweets.inlineTweets
    tweetBytes = Text.Encoding.encodeUtf8 . Text.pack $ inlineTweets
  Alice.Tweets.TweetList tweetThreads <-
    case Aeson.eitherDecodeStrict @Alice.Tweets.TweetList tweetBytes of
      Left err -> error err
      Right result -> return result
  randomTweets <- Random.Shuffle.shuffleM tweetThreads
  manager <- Twitter.Conduit.newManager Twitter.Conduit.tlsManagerSettings
  mapM_ (postThread manager) $ take 1 randomTweets

postThread :: Twitter.Conduit.Manager -> Alice.Tweets.TweetThread -> IO ()
postThread _ (Alice.Tweets.TweetThread []) = return ()
postThread manager (Alice.Tweets.TweetThread (firstTweet : _)) = postStatus manager firstTweet

postStatus :: Twitter.Conduit.Manager -> Text -> IO ()
postStatus manager status = do
  Logging.log $ "Post: " <> status
  twInfo <- getTWInfoFromEnv
  response <- Twitter.Conduit.call twInfo manager $ Twitter.Conduit.update status
  print response

getTWInfoFromEnv :: IO Twitter.Conduit.TWInfo
getTWInfoFromEnv = do
  (oauth, cred) <- getOAuthTokens
  return $ Twitter.Conduit.setCredential oauth cred Twitter.Conduit.def

getOAuthTokens :: IO (Twitter.Conduit.OAuth, Twitter.Conduit.Credential)
getOAuthTokens = do
  consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
  consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
  accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
  accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
  let
    oauth =
      Twitter.Conduit.twitterOAuth
      { Twitter.Conduit.oauthConsumerKey = consumerKey
      , Twitter.Conduit.oauthConsumerSecret = consumerSecret
      }
    cred =
      Twitter.Conduit.Credential
      [ ("oauth_token", accessToken)
      , ("oauth_token_secret", accessSecret)
      ]
  return (oauth, cred)
  where
  getEnv' = fmap ByteString.Char8.pack . Environment.getEnv
