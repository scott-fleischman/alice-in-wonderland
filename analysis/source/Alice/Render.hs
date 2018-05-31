{-# LANGUAGE OverloadedStrings #-}

module Alice.Render where

import qualified Alice.Structure
import           Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text

wordToText :: Alice.Structure.Word -> Text
wordToText word = Text.concat
  [ (useUnicodeEmdash . Alice.Structure.wordPrefix) word
  , Alice.Structure.wordText word
  , (useUnicodeEmdash . Alice.Structure.wordSuffix) word
  ]

concatTextWords :: Seq Text -> Text
concatTextWords Seq.Empty = Text.empty
concatTextWords (word :<| Seq.Empty) = word
concatTextWords (word1 :<| word2 :<| rest) | isEmdashSuffix word1 = Text.append word1 $ concatTextWords (word2 :<| rest)
concatTextWords (word1 :<| word2 :<| rest) = Text.concat [word1, " ", concatTextWords (word2 :<| rest)]

asciiEmdash :: Text
asciiEmdash = "--"

unicodeEmdash :: Text
unicodeEmdash = "\x2014"

useUnicodeEmdash :: Text -> Text
useUnicodeEmdash = Text.replace asciiEmdash unicodeEmdash

isEmdashSuffix :: Text -> Bool
isEmdashSuffix text = Text.isSuffixOf asciiEmdash text || Text.isSuffixOf unicodeEmdash text
