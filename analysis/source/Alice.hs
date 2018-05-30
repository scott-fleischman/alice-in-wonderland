{-# LANGUAGE OverloadedStrings #-}

module Alice where

import qualified Alice.Parse
import qualified Alice.Sentence
import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.HashSet as HashSet
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

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
  mapM_ printChapter chapters

printChapter :: Alice.Structure.Chapter -> IO ()
printChapter (Alice.Structure.Chapter number title _contents paragraphs) = do
  putStrLn $ show number ++ ". " ++ show title
  putStrLn ""
  let chapterWords = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition paragraphs
  mapM_ Text.IO.putStrLn . HashSet.fromList . Foldable.toList . fmap Alice.Structure.wordSuffix $ chapterWords
  -- mapM_ printIfContainsNonLetter $ fmap Alice.Structure.wordText $ chapterWords
  putStrLn "\n\n\n"

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
main = run Alice.TextFile.textFilePath
