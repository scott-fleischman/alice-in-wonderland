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
import           Data.Semigroup ((<>))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           GHC.Generics (Generic)
import           Prelude hiding (words)
import qualified Text.Numeral.Roman as Numeral.Roman

data Tweet = Tweet
  { author :: Text
  , text :: Text
  } deriving Generic
instance Aeson.ToJSON Tweet
instance Aeson.FromJSON Tweet

data Thread = Thread { tweets :: [Tweet] } deriving Generic
instance Aeson.ToJSON Thread
instance Aeson.FromJSON Thread

data Book = Book { threads :: [Thread] } deriving Generic
instance Aeson.ToJSON Book
instance Aeson.FromJSON Book

-- Create the JSON file at the given path.
createTweetsFile :: FilePath -> IO ()
createTweetsFile outputPath = do
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

    paraText (Alice.Structure.ParagraphFormatPlain t) = t
    paraText (Alice.Structure.ParagraphFormatIndented seqText) =
      Alice.Render.normalizeIndent . Text.intercalate "\n" $ Foldable.toList seqText
    paraText (Alice.Structure.ParagraphFormatLaterEdition seqText) =
      Alice.Render.normalizeIndent . Text.intercalate "\n" $ Foldable.toList seqText
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
    makeParaInfo para =
      let
        paraWords = Alice.Sentence.textWords . paraText $ para
        renderedPara = Alice.Render.renderAllWords paraWords
      in (renderedPara, paraWords)
    _printPara (r, _) = do
      putStrLn ""
      Text.IO.putStrLn r
    printBreaks breaks = do
      putStrLn ""
      mapM_ (\b -> do { Text.IO.putStr "* "; Text.IO.putStrLn b }) breaks
    printParas paras =
      mapM_ printBreaks
        -- $ Seq.filter ((> 1) . Seq.length . Seq.filter ((== ".") . Alice.Structure.wordSuffix) . snd)
        $ fmap (Alice.Render.chunkRendering 280 . fst)
        $ Seq.filter ((>280) . Text.length . fst)
        $ fmap makeParaInfo
        $ paras

  mapM_ printChapter chapters

  let tweetList = makeTweetList body
  putStrLn $ "Writing " ++ outputPath
  Aeson.encodeFile outputPath tweetList

-- Create a list of tweets from the structural (parsed) text
makeTweetList :: Alice.Structure.Body -> Book
makeTweetList = Book . concatMap makeChapterTweets . Alice.Structure.bodyChapters

aliceName :: Text
aliceName = "alice"

-- Make a list of tweets for a chapter, one tweet thread per sentence.
-- Breaks up sentences into a thread of multiple tweets when the sentence is longer than 280 characters.
makeChapterTweets :: Alice.Structure.Chapter -> [Thread]
makeChapterTweets chapter =
  let
    chapterWords = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition $ Alice.Structure.chapterParagraphs chapter
    sentences = Alice.Sentence.parseAllSentences chapterWords
    sentenceTexts = fmap (\(Alice.Structure.Sentence words) -> Alice.Render.renderAllWords words) sentences
    chunks = fmap (Alice.Render.chunkRendering 280) sentenceTexts
    tweetThreads = fmap (Thread . fmap (Tweet aliceName) . Foldable.toList) chunks
    titleText
      = "#AliceInWonderland\nChapter "
      <> Numeral.Roman.toRoman (((\(Alice.Structure.ChapterNumber x) -> x) . Alice.Structure.chapterNumber) chapter)
      <> ". "
      <> ((\(Alice.Structure.ChapterTitle x) -> x) . Alice.Structure.chapterTitle) chapter
    titleTweet = Tweet aliceName titleText
    titleThread = Thread [titleTweet]
  in titleThread : Foldable.toList tweetThreads
