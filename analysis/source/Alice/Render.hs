{-# LANGUAGE OverloadedStrings #-}

module Alice.Render where

import qualified Alice.Structure
import           Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text

wordToText :: Alice.Structure.Word -> Text
wordToText word = Text.concat
  [ Alice.Structure.wordPrefix word
  , Alice.Structure.wordText word
  , Alice.Structure.wordSuffix word
  ]

concatTextWords :: Seq Text -> Text
concatTextWords Seq.Empty = Text.empty
concatTextWords (word1 :<| word2 :<| rest) | Text.isSuffixOf "--" word1 = Text.append word1 $ concatTextWords (word2 :<| rest)
concatTextWords (word1 :<| word2 :<| rest) = Text.concat [word1, " ", concatTextWords (word2 :<| rest)]
concatTextWords (word1 :<| Seq.Empty) = word1
