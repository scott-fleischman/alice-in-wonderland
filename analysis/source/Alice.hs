{-# LANGUAGE OverloadedStrings #-}

module Alice where

import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Data.Text

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
    mapM_ print paragraphs
    putStrLn "\n"

main :: IO ()
main = run Alice.TextFile.textFilePath
