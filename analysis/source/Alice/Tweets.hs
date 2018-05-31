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

data TweetList = TweetList { tweets :: [TweetThread] } deriving (Show, Generic)
instance Aeson.ToJSON TweetList
instance Aeson.FromJSON TweetList

data TweetThread = TweetThread { thread :: [Text] } deriving (Show, Generic)
instance Aeson.ToJSON TweetThread
instance Aeson.FromJSON TweetThread

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

makeTweetList :: Alice.Structure.Body -> TweetList
makeTweetList = TweetList . concatMap makeChapterTweets . Alice.Structure.bodyChapters

makeChapterTweets :: Alice.Structure.Chapter -> [TweetThread]
makeChapterTweets chapter =
  let
    chapterWords = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition $ Alice.Structure.chapterParagraphs chapter
    sentences = Alice.Sentence.parseAllSentences chapterWords
    sentenceTexts = fmap (\(Alice.Structure.Sentence words) -> Alice.Render.renderAllWords words) sentences
    chunks = fmap (Alice.Render.chunkRendering 280) sentenceTexts
    tweetThreads = fmap (TweetThread . Foldable.toList) chunks
  in Foldable.toList tweetThreads
