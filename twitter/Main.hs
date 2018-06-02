{-# LANGUAGE OverloadedStrings #-} -- allows string literals to be interpreted as different types
{-# LANGUAGE TypeApplications #-} -- allows you to specify a type explicitly using the '@Type' syntax

import qualified Alice.Tweets -- contains the definition of the JSON structure
import qualified Control.Concurrent.Async.Timer as Async.Timer -- timers that fire asynchronously
import qualified Control.Exception as Exception -- exception handling
import qualified Control.Lens as Lens -- deep structural value modification
import qualified Control.Logging as Logging -- logging utility
import qualified Control.Monad as Monad -- abstractions for error handling
import qualified Data.Aeson as Aeson -- JSON library
import qualified Data.ByteString.Char8 as ByteString.Char8 -- ASCII text handling (for environment variables)
import qualified Data.Maybe as Maybe -- utilities for dealing with optional values
import           Data.Semigroup ((<>)) -- abstract string concatenation
import           Data.Text (Text) -- the preferred string type
import qualified Data.Text as Text -- utilities for the 'Text' string type
import qualified System.Environment as Environment -- environment variable access
import qualified System.Random.Shuffle as Random.Shuffle -- shuffles a list
import qualified Text.Read -- string parsing of Haskell values
import qualified TweetsData -- the embedded tweet data
import qualified Web.Twitter.Conduit as Twitter.Conduit -- core twitter api
import qualified Web.Twitter.Conduit.Parameters as Twitter.Conduit.Parameters -- extra twitter api for posting replies
import qualified Web.Twitter.Types as Twitter.Types -- core twitter api definitions

-- application starting point
main :: IO ()
main = Logging.withStdoutLogging $ do -- logs go to STDOUT

  -- Get tweet interval from environment variable.
  -- This allows us to change it without recompiling code
  tweetIntervalEnv <- Environment.lookupEnv "TWEET_INTERVAL"

  let
    defaultTweetInterval = 30 * 60 * 1000 -- 30 minutes

    -- Parse environment variable as a number. Use default value above on error
    tweetInterval = Maybe.fromMaybe defaultTweetInterval (tweetIntervalEnv >>= Text.Read.readMaybe @Int)

  -- log tweet interval for debugging purposes
  Logging.log $ "Timer interval: " <> (Text.pack . show) tweetInterval

  -- get list of tweet threads from embedded bytes
  Alice.Tweets.TweetList tweetThreads <-
    case Aeson.eitherDecodeStrict @Alice.Tweets.TweetList TweetsData.tweetsBytes of -- Decode the bytes as JSON and parse them.
      Left err -> error err -- crash the application with error message if the decoding failed
      Right result -> return result

  -- create HTTPS manager for sending data to Twitter API
  manager <- Twitter.Conduit.newManager Twitter.Conduit.tlsManagerSettings

  -- set up timer configuration
  let timerConf = Async.Timer.timerConfSetInterval tweetInterval Async.Timer.defaultTimerConf

  -- start timer
  Async.Timer.withAsyncTimer timerConf $ \timer ->

    -- repeat this loop forever
    Monad.forever $ do

      -- shuffle tweets
      randomTweets <- Random.Shuffle.shuffleM tweetThreads

      -- deal out each tweet
      Monad.forM_ randomTweets $ \thread -> do

        -- wait for the next interval
        Async.Timer.timerWait timer

        -- post the tweet and catch/ignore any exception that occurred
        tryPostThread manager thread

-- Post a thread to twitter. Do nothing upon failure.
tryPostThread :: Twitter.Conduit.Manager -> Alice.Tweets.TweetThread -> IO ()
tryPostThread manager thread = do

  -- post the thread catching any exception
  possibleResult <- Exception.try @Exception.SomeException (postThread manager thread)
  case possibleResult of

    -- log the error but continue execution
    Left err -> Logging.warn (Text.pack . show $ err)

    -- success! do nothing further
    Right () -> return ()

-- Post the thread, possibly throwing an exception
postThread :: Twitter.Conduit.Manager -> Alice.Tweets.TweetThread -> IO ()
postThread manager (Alice.Tweets.TweetThread tweets) = go Nothing tweets -- call internal loop for posting replies
  where
  -- no more tweets in thread, we are done
  go _ [] = return ()

  -- there is at least one tweet to post
  go parent (tweet : rest) = do

    -- post the tweet using the specified parent, which may be 'Nothing' meaning it is the top tweet
    status <- postStatus manager parent tweet

    -- capture the tweet identifier to be the parent for the next tweet in the thread
    let statusId = Twitter.Types.statusId status

    -- iterate over the loop using the just-posted tweet as the parent
    go (Just statusId) rest

-- Post an individual status with the given parent.
postStatus :: Twitter.Conduit.Manager -> Maybe Twitter.Types.StatusId -> Text -> IO Twitter.Types.Status
postStatus manager parent status = do

  -- log the status
  Logging.log $ "Post: " <> status

  -- get twitter settings
  twInfo <- getTWInfoFromEnv
  let
    -- create twitter api data for tweet
    baseStatus = Twitter.Conduit.update status

    -- set the parent for the twitter api request
    statusWithParent = Lens.set Twitter.Conduit.Parameters.inReplyToStatusId parent baseStatus

  -- send the post request to twitter
  response <- Twitter.Conduit.call twInfo manager statusWithParent

  -- capture the response, which includes the twitter id so we can use it to be the parent of a subsequent reply
  return response

-- Gets Twitter API information from environment, including OAuth and other credentials
getTWInfoFromEnv :: IO Twitter.Conduit.TWInfo
getTWInfoFromEnv = do
  -- get credentials from environment variables
  (oauth, cred) <- getOAuthTokens

  -- return credentials as structured data
  return $ Twitter.Conduit.setCredential oauth cred Twitter.Conduit.def

-- Extract credentials from environment variables
getOAuthTokens :: IO (Twitter.Conduit.OAuth, Twitter.Conduit.Credential)
getOAuthTokens = do
  -- these are the 4 required environment variables
  consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
  consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
  accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
  accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"

  -- build internal structure for twitter library
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

  -- return the two required values
  return (oauth, cred)

  where
  -- decode environment variables as an ASCII string
  getEnv' = fmap ByteString.Char8.pack . Environment.getEnv
