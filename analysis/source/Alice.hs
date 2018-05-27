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

  Data.Text.IO.putStrLn "\nChapter Count:"
  mapM_ print . fmap (\x -> (Alice.Structure.chapterNumber x, Alice.Structure.chapterTitle x)) . Alice.Structure.bodyChapters $ body

main :: IO ()
main = run Alice.TextFile.textFilePath
