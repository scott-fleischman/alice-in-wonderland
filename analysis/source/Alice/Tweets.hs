-- | Output tweets in a JSON file to be loaded by the Twitter bot.

{-# LANGUAGE DeriveGeneric #-}

module Alice.Tweets where

import qualified Alice.Parse
import qualified Alice.Render
import qualified Alice.Sentence
import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (words)

-- Top level JSON object, indicating a list of Tweet threads.
data TweetList = TweetList { tweets :: [TweetThread] } deriving (Show, Generic)
instance Aeson.ToJSON TweetList
instance Aeson.FromJSON TweetList

-- A tweet thread contains a list of tweets indended to be posted as a single thread.
data TweetThread = TweetThread { thread :: [Text] } deriving (Show, Generic)
instance Aeson.ToJSON TweetThread
instance Aeson.FromJSON TweetThread

-- Create the JSON file at the given path.
createTweetsFile :: FilePath -> IO ()
createTweetsFile outputPath = do
  text <- Alice.TextFile.loadText Alice.TextFile.textFilePath
  body <-
    case Alice.Parse.parseBody text of
      Left err -> error (show err)
      Right result -> return result

  let tweetList = makeTweetList body
  putStrLn $ "Writing " ++ outputPath
  Aeson.encodeFile outputPath tweetList

-- Create a list of tweets from the structural (parsed) text
makeTweetList :: Alice.Structure.Body -> TweetList
makeTweetList = TweetList . concatMap makeChapterTweets . Alice.Structure.bodyChapters

-- Make a list of tweets for a chapter, one tweet thread per sentence.
-- Breaks up sentences into a thread of multiple tweets when the sentence is longer than 280 characters.
makeChapterTweets :: Alice.Structure.Chapter -> [TweetThread]
makeChapterTweets chapter =
  let
    chapterWords = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition $ Alice.Structure.chapterParagraphs chapter
    sentences = Alice.Sentence.parseAllSentences chapterWords
    sentenceTexts = fmap (\(Alice.Structure.Sentence words) -> Alice.Render.renderAllWords words) sentences
    chunks = fmap (Alice.Render.chunkRendering 280) sentenceTexts
    tweetThreads = fmap (TweetThread . Foldable.toList) chunks
  in Foldable.toList tweetThreads
