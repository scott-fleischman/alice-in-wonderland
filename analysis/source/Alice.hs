{-# LANGUAGE OverloadedStrings #-}

module Alice where

import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Data.Text
import qualified Data.Text.IO

run :: FilePath -> IO ()
run path = do
  text <- Alice.TextFile.loadText path
  putStrLn $ "Text length = " ++ show (Data.Text.length text)
  body <-
    case Alice.Structure.parseBody text of
      Left err -> error (show err)
      Right result -> return result

  let chapters = Alice.Structure.bodyChapters body
  putStrLn $ "\nChapter Count: " ++  (show . length) chapters
  mapM_ printChapter chapters
  where
  printChapter (Alice.Structure.Chapter number title _contents paragraphs) = do
    putStrLn $ show number ++ ". " ++ show title
    putStrLn ""
    mapM_ printParagraphFormat paragraphs
    putStrLn "\n\n\n"
  printParagraphFormat (Alice.Structure.ParagraphFormatPlain text) = do
    Data.Text.IO.putStrLn text
    putStrLn ""
  printParagraphFormat (Alice.Structure.ParagraphFormatIndented texts) = do
    mapM_ Data.Text.IO.putStrLn texts
    putStrLn ""
  printParagraphFormat (Alice.Structure.ParagraphFormatLaterEdition texts) = do
    putStrLn "  [later edition:]"
    mapM_ Data.Text.IO.putStrLn texts
    putStrLn ""
  printParagraphFormat Alice.Structure.ParagraphFormatStarDivision = do
    putStrLn " * * * (star divison) * * * "
    putStrLn ""

main :: IO ()
main = run Alice.TextFile.textFilePath
