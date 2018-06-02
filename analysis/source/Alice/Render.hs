-- | Render our data structure into readable text.
-- Basically reverses the parsing process, although we can use a real emdash character.

{-# LANGUAGE OverloadedStrings #-}

module Alice.Render where

import qualified Alice.Structure
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import           Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text

-- Render all of the words separated by spaces and normalize the indent for poetry.
renderAllWords :: Seq Alice.Structure.Word -> Text
renderAllWords = normalizeIndent . renderAllWordsUnnormalizedIndent

-- Break the text into chunks of a given size so they can become a Tweet thread if
-- the sentence is too long for a single tweet.
chunkRendering :: Int -> Text -> Seq Text
chunkRendering maxLength input
  = Maybe.fromMaybe (Seq.singleton input) $ attempt
    [ "; " -- try breaking up the text using these characters (try the first, then the next one, etc.)
    , "’ "
    , ") "
    , ": "
    , "—"
    , ", "
    ]
  where
  attempt [] = Nothing
  attempt (x : xs) =
    case chunkRenderingOn x maxLength input of
      Just result -> Just result
      Nothing -> attempt xs

-- Attempt to break the text by the separator into chunks of a certain size or less.
-- This is needed so that tweets can fit into 280 characters.
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

-- | Normalize the indent.
-- If the paragraph contains 4 lines, and
-- 2 lines have a prefix of 5 spaces
-- and the other 2 lines of a prefix of 6 spaces,
-- the resulting text will have lines prefixed by 0 spaces and 1 space.
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

-- Render words with full indentation as in the Gutenberg text
renderAllWordsUnnormalizedIndent :: Seq Alice.Structure.Word -> Text
renderAllWordsUnnormalizedIndent Seq.Empty = Text.empty
renderAllWordsUnnormalizedIndent (word :<| Seq.Empty) = renderWord word

-- in the case of two adjacent words, don't output a space if the first word ends with an emdash
renderAllWordsUnnormalizedIndent (word1 :<| word2 :<| rest)
  | hasIndent word2 = Text.concat [renderWord word1, "\n", renderAllWordsUnnormalizedIndent (word2 :<| rest)]
  | renderedWord1 <- renderWord word1
  , isEmdashSuffix renderedWord1 = Text.append renderedWord1 $ renderAllWordsUnnormalizedIndent (word2 :<| rest)

  | otherwise = Text.concat [renderWord word1, " ", renderAllWordsUnnormalizedIndent (word2 :<| rest)]

-- does this word have significant indentation?
hasIndent :: Alice.Structure.Word -> Bool
hasIndent = (> 0) . getIndentValue

-- how many spaces of indentation does this word have?
getIndentValue :: Alice.Structure.Word -> Int
getIndentValue = (\(Alice.Structure.Indent indent) -> indent) . Alice.Structure.wordIndent

-- render word with surrounding punctuation
renderWord :: Alice.Structure.Word -> Text
renderWord word = Text.concat
  [ Text.replicate (getIndentValue word) " "
  , (useUnicodeEmdash . Alice.Structure.wordPrefix) word
  , Alice.Structure.wordText word
  , (useUnicodeEmdash . Alice.Structure.wordSuffix) word
  ]

-- space each word and combine into a single string
concatTextWords :: Seq Text -> Text
concatTextWords Seq.Empty = Text.empty
concatTextWords (word :<| Seq.Empty) = word
concatTextWords (word1 :<| word2 :<| rest) | isEmdashSuffix word1 = Text.append word1 $ concatTextWords (word2 :<| rest)
concatTextWords (word1 :<| word2 :<| rest) = Text.concat [word1, " ", concatTextWords (word2 :<| rest)]

-- ascii emdash approximation
asciiEmdash :: Text
asciiEmdash = "--"

-- unicode emdash charater
unicodeEmdash :: Text
unicodeEmdash = "\x2014"

useUnicodeEmdash :: Text -> Text
useUnicodeEmdash = Text.replace asciiEmdash unicodeEmdash

-- does this word end in an ascii or unicode emdash?
isEmdashSuffix :: Text -> Bool
isEmdashSuffix text = Text.isSuffixOf asciiEmdash text || Text.isSuffixOf unicodeEmdash text
