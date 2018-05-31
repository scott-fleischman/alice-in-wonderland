{-# LANGUAGE DeriveGeneric #-}

module Alice.Tweets where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)

data TweetList = TweetList { tweets :: [TweetThread] } deriving (Generic)
instance Aeson.ToJSON TweetList

data TweetThread = TweetThread { thread :: [Text] } deriving (Generic)
instance Aeson.ToJSON TweetThread

-- createTweetsFile :: FilePath -> IO ()
-- createTweetsFile outputPath = do
--   text <- Alice.TextFile.loadText Alice.TextFile.textFilePath
--   body <-
--     case Alice.Parse.parseBody text of
--       Left err -> error (show err)
--       Right result -> return result

--   let chapters = Alice.Structure.bodyChapters body
--   putStrLn $ "\nChapter Count: " ++  (show . length) chapters
--   mapM_ printChapterSentences chapters

-- printChapterSentences :: Alice.Structure.Chapter -> IO ()
-- printChapterSentences (Alice.Structure.Chapter number (Alice.Structure.ChapterTitle titleText) _contents paragraphs) = do
--   putStr $ "Chapter " ++ show number ++ ". "
--   Text.IO.putStrLn titleText
--   putStrLn ""
--   let
--     chapterWords = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition paragraphs
--     sentences = Alice.Sentence.parseAllSentences chapterWords
--     sentenceTexts = fmap (\(Alice.Structure.Sentence words) -> Alice.Render.renderAllWords words) sentences
--     longSentenceTexts = filter ((>280) . Text.length) $ Foldable.toList sentenceTexts
--     chunks = fmap (Alice.Render.chunkRendering 280) longSentenceTexts
--   mapM_ (Text.IO.putStrLn . Text.append "\n" . Text.intercalate "\n" . Foldable.toList) chunks
--   putStrLn "\n\n\n"
