{-# LANGUAGE OverloadedStrings #-}

module Alice.Render where

import qualified Alice.Structure
import qualified Data.Char as Char
import           Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text

renderAllWords :: Seq Alice.Structure.Word -> Text
renderAllWords = normalizeIndent . renderAllWordsUnnormalizedIndent

chunkRendering :: Int -> Text -> Seq Text
chunkRendering maxLength input =
  case chunkRenderingOn "; " maxLength input of
    Just result -> result
    Nothing ->
      case chunkRenderingOn ", " maxLength input of
        Just result -> result
        Nothing -> Seq.singleton input

chunkRenderingOn :: Text -> Int -> Text -> Maybe (Seq Text)
chunkRenderingOn _ maxLength input | Text.length input <= maxLength = Just $ Seq.singleton input
chunkRenderingOn separator maxLength input =
  let
    (firstPass, remainder) = Text.splitAt maxLength input
    (beforeIncludingSep, afterSep) = Text.breakOnEnd separator firstPass
  in if Text.null beforeIncludingSep
    then Nothing
    else do
      rest <- chunkRenderingOn separator maxLength (Text.append afterSep remainder)
      let
        stripped =
          if Text.stripEnd separator /= separator
            then Text.stripEnd beforeIncludingSep
            else beforeIncludingSep
      Just $ stripped :<| rest

normalizeIndent :: Text -> Text
normalizeIndent input | Text.null input = input
normalizeIndent input =
  let
    inputLines = Text.lines input
    indent = Text.length . Text.takeWhile Char.isSpace
    minimumIndent = minimum $ fmap indent inputLines
  in if minimumIndent <= 0
    then input
    else Text.intercalate "\n" $ fmap (Text.drop minimumIndent) inputLines

renderAllWordsUnnormalizedIndent :: Seq Alice.Structure.Word -> Text
renderAllWordsUnnormalizedIndent Seq.Empty = Text.empty
renderAllWordsUnnormalizedIndent (word :<| Seq.Empty) = renderWord word
renderAllWordsUnnormalizedIndent (word1 :<| word2 :<| rest)
  | hasIndent word2 = Text.concat [renderWord word1, "\n", renderAllWordsUnnormalizedIndent (word2 :<| rest)]
  | renderedWord1 <- renderWord word1
  , isEmdashSuffix renderedWord1 = Text.append renderedWord1 $ renderAllWordsUnnormalizedIndent (word2 :<| rest)
  | otherwise = Text.concat [renderWord word1, " ", renderAllWordsUnnormalizedIndent (word2 :<| rest)]

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
