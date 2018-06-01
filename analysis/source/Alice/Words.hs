{-# LANGUAGE OverloadedStrings #-}

module Alice.Words where

import qualified Alice.Parse
import qualified Alice.Sentence
import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           Prelude hiding (words)

createTopWordsFile :: FilePath -> IO ()
createTopWordsFile outputPath = do
  text <- Alice.TextFile.loadText Alice.TextFile.textFilePath
  body <-
    case Alice.Parse.parseBody text of
      Left err -> error (show err)
      Right result -> return result

  let
    chapters = Alice.Structure.bodyChapters body
    words = Foldable.foldMap (Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition . Alice.Structure.chapterParagraphs) chapters
    output
      = Text.intercalate "\n"
      . fmap (\(k, v) -> Text.concat [k, " ", (Text.pack . show) v])
      . List.sortOn (Ord.Down . snd)
      . Map.toList
      . Foldable.foldr (\(k, v) m -> Map.insertWith (+) k v m) Map.empty
      . fmap (\x -> ((Text.toLower . Alice.Structure.wordText) x, 1 :: Int))
      $ words
  Text.IO.writeFile outputPath output
