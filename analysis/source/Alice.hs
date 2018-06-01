{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Alice where

import qualified Alice.Parse
import qualified Alice.Render
import qualified Alice.Sentence
import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Alice.Tweets
import qualified Alice.Words
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           Prelude hiding (words)

run :: FilePath -> IO ()
run path = do
  text <- Alice.TextFile.loadText path
  putStrLn $ "Text length = " ++ show (Text.length text)
  body <-
    case Alice.Parse.parseBody text of
      Left err -> error (show err)
      Right result -> return result

  let chapters = Alice.Structure.bodyChapters body
  putStrLn $ "\nChapter Count: " ++  (show . length) chapters
  mapM_ printChapterSentences chapters
--  mapM_ printChapter chapters

printSuffixSet :: Alice.Structure.Body -> IO ()
printSuffixSet body = do
  let
    chapters = Alice.Structure.bodyChapters body
    allWords :: Seq Alice.Structure.Word
    allWords
      = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition
      $ Foldable.foldMap Alice.Structure.chapterParagraphs chapters
    suffixSet
      = Set.fromList
      . fmap Alice.Structure.wordSuffix
      . filter
        (\x
          -> (not . Text.null . Alice.Structure.wordSuffix) x
          && Alice.Structure.wordLast x == Alice.Structure.NotLastWordInParagraph
        )
      . Foldable.toList
      $ allWords
  mapM_ Text.IO.putStrLn suffixSet

printChapterSentences :: Alice.Structure.Chapter -> IO ()
printChapterSentences (Alice.Structure.Chapter number (Alice.Structure.ChapterTitle titleText) _contents paragraphs) = do
  putStr $ "Chapter " ++ show number ++ ". "
  Text.IO.putStrLn titleText
  putStrLn ""
  let
    chapterWords = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition paragraphs
    sentences = Alice.Sentence.parseAllSentences chapterWords
    sentenceTexts = fmap (\(Alice.Structure.Sentence words) -> Alice.Render.renderAllWords words) sentences
    longSentenceTexts = filter ((>280) . Text.length) $ Foldable.toList sentenceTexts
    chunks = fmap (Alice.Render.chunkRendering 280) longSentenceTexts
    invalidChunks = filter (\x -> length (Seq.findIndicesL ((> 280) . Text.length) x) > 0) chunks
  mapM_ (Text.IO.putStrLn . Text.append "\n" . Text.intercalate "\n" . Foldable.toList) invalidChunks
  putStrLn "\n\n\n"

printChapter :: Alice.Structure.Chapter -> IO ()
printChapter (Alice.Structure.Chapter number title _contents paragraphs) = do
  putStrLn $ show number ++ ". " ++ show title
  putStrLn ""
  let
    chapterWords = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition paragraphs
    getSuffix = Alice.Structure.wordSuffix . Alice.Structure.wordContextWord
    getWordLast = Alice.Structure.wordLast . Alice.Structure.wordContextWord
  mapM_ printWordContextLn
    . filter
      (\wordContext
        -> (Maybe.isJust . Text.find (\c -> elem c ['.','!','?']) . getSuffix) wordContext
        && getWordLast wordContext == Alice.Structure.NotLastWordInParagraph
      )
    . Foldable.toList
    . Alice.Sentence.contextualizeWords (Alice.Structure.BeforeCount 3) (Alice.Structure.AfterCount 5)
    $ chapterWords
  putStrLn "\n\n\n"

printWord :: Alice.Structure.Word -> IO ()
printWord = Text.IO.putStr . Alice.Render.renderWord

printWordLn :: Alice.Structure.Word -> IO ()
printWordLn word = do
  printWord word
  Text.IO.putStr "\n"

printWordContextLn :: Alice.Structure.WordContext -> IO ()
printWordContextLn (Alice.Structure.WordContext before word after) =
  Text.IO.putStrLn $
    Text.intercalate "   "
      [ Alice.Render.renderAllWords before
      , "   [[ "
      , Alice.Render.renderWord word
      , " ]]   "
      , Alice.Render.renderAllWords after
      ]

printPair :: Show a => (Text, a) -> IO ()
printPair (t, a) = do
  Text.IO.putStr t
  putStr "\t"
  print a

printIfContainsNonLetter :: Text -> IO ()
printIfContainsNonLetter input =
  if Text.any (not . Char.isLetter) input
    then Text.IO.putStrLn input
    else return ()

printParagraphFlat :: Alice.Structure.ParagraphFormat -> IO ()
printParagraphFlat para = do
  let optionalText = Alice.Sentence.flattenParagraphFormat Alice.Structure.LaterEdition para
  case optionalText of
    Just text -> do
      Text.IO.putStrLn text
      putStrLn ""
    Nothing -> return ()

printParagraphFormat :: Alice.Structure.ParagraphFormat -> IO ()
printParagraphFormat (Alice.Structure.ParagraphFormatPlain text) = do
  Text.IO.putStrLn text
  putStrLn ""
printParagraphFormat (Alice.Structure.ParagraphFormatIndented texts) = do
  mapM_ Text.IO.putStrLn texts
  putStrLn ""
printParagraphFormat (Alice.Structure.ParagraphFormatLaterEdition texts) = do
  putStrLn "  [later edition:]"
  mapM_ Text.IO.putStrLn texts
  putStrLn ""
printParagraphFormat Alice.Structure.ParagraphFormatStarDivision = do
  putStrLn " * * * (star divison) * * * "
  putStrLn ""
printParagraphFormat Alice.Structure.ParagraphFormatChorusMarker = do
  putStrLn "   --CHORUS.-- "
  putStrLn ""

main :: IO ()
main = do
  Alice.Tweets.createTweetsFile "data/tweets.json"
  Alice.Words.createTopWordsFile "output/top-words.txt"
  -- run Alice.TextFile.textFilePath
