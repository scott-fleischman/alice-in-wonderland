{-# LANGUAGE OverloadedStrings #-}

module Alice.Render where

import qualified Alice.Structure
import           Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text

renderAllWords :: Seq Alice.Structure.Word -> Text
renderAllWords Seq.Empty = Text.empty
renderAllWords (word :<| Seq.Empty) = renderWord word
renderAllWords (word1 :<| word2 :<| rest)
  | hasIndent word2 = Text.concat [renderWord word1, "\n", renderAllWords (word2 :<| rest)]
  | renderedWord1 <- renderWord word1
  , isEmdashSuffix renderedWord1 = Text.append renderedWord1 $ renderAllWords (word2 :<| rest)
  | otherwise = Text.concat [renderWord word1, " ", renderAllWords (word2 :<| rest)]

hasIndent :: Alice.Structure.Word -> Bool
hasIndent = (> 0) . getIndentValue

getIndentValue :: Alice.Structure.Word -> Int
getIndentValue = (\(Alice.Structure.Indent indent) -> indent) . Alice.Structure.wordIndent

renderWord :: Alice.Structure.Word -> Text
renderWord word = Text.concat
  [ Text.replicate (getIndentValue word) " "
  , (useUnicodeEmdash . Alice.Structure.wordPrefix) word
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
