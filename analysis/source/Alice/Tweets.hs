-- | Output tweets in a JSON file to be loaded by the Twitter bot.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Alice.Tweets where

import qualified Alice.Parse
import qualified Alice.Render
import qualified Alice.Sentence
import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
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
createTweetsFile _outputPath = do
  fileText <- Alice.TextFile.loadText Alice.TextFile.textFilePath
  body <-
    case Alice.Parse.parseBody fileText of
      Left err -> error (show err)
      Right result -> return result

  let
    chapters = Alice.Structure.bodyChapters body
    paraLengths
      = fmap
        (\ch ->
          ( Alice.Structure.chapterNumber ch
          , Seq.length
            . Seq.filter isContentPara
            $ Alice.Structure.chapterParagraphs ch)
          )
        chapters
    isContentPara (Alice.Structure.ParagraphFormatPlain _) = True
    isContentPara (Alice.Structure.ParagraphFormatIndented _) = True
    isContentPara (Alice.Structure.ParagraphFormatLaterEdition _) = True
    isContentPara Alice.Structure.ParagraphFormatChorusMarker = False
    isContentPara Alice.Structure.ParagraphFormatStarDivision = False

    paraText (Alice.Structure.ParagraphFormatPlain text) = text
    paraText (Alice.Structure.ParagraphFormatIndented seqText) = Text.intercalate "\n" $ Foldable.toList seqText
    paraText (Alice.Structure.ParagraphFormatLaterEdition seqText) = Text.intercalate "\n" $ Foldable.toList seqText
    paraText Alice.Structure.ParagraphFormatChorusMarker = ""
    paraText Alice.Structure.ParagraphFormatStarDivision = ""

    _charCount = Text.length . paraText

  putStrLn "Paragraph counts"
  mapM_ print paraLengths
  let totalParas = sum $ fmap snd paraLengths
  putStrLn $ "Total: " ++ show totalParas
  putStrLn ""

  putStrLn $ "Long paragraphs"
  let
    printChapter ch = do
      putStrLn "\n"
      print $ Alice.Structure.chapterNumber ch
      printParas $ Alice.Structure.chapterParagraphs ch
    printParas paras =
      mapM_ print
        $ Seq.filter (\(x, _) -> not $ Seq.null x)
        $ fmap (\para -> ((Seq.filter (Text.null . Alice.Structure.wordText) . Alice.Sentence.textWords . paraText) para, para))
        $ paras

  mapM_ printChapter chapters

  -- let tweetList = makeTweetList body
  -- putStrLn $ "Writing " ++ outputPath
  -- Aeson.encodeFile outputPath tweetList

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
