{-# LANGUAGE OverloadedStrings #-}

module Alice.Sentence where

import           Alice.Structure
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (Word)

allParagraphWords :: EditionOption -> Seq ParagraphFormat -> [Word]
allParagraphWords editionOption = concatMap (paragraphWords editionOption)

paragraphWords :: EditionOption -> ParagraphFormat -> [Word]
paragraphWords editionOption = textWords . maybe "" id . flattenParagraphFormat editionOption

textWords :: Text -> [Word]
textWords = concatMap buildWord . Text.words

buildWord :: Text -> [Word]
buildWord input =
  let
    (prefix, afterPrefix) = stripPunctuationPrefix input
    emdash = "--"
    (beforeEmdash, emdashAndAfter) = Text.breakOn emdash afterPrefix
    (text, suffixBeforeEmdash) = stripPunctuationSuffix beforeEmdash
  in case Text.stripPrefix emdash emdashAndAfter of
    Nothing ->
      [ Word
        { wordPrefix = prefix
        , wordText = text
        , wordSuffix = suffixBeforeEmdash
        }
      ]
    Just afterEmdash ->
      Word
        { wordPrefix = prefix
        , wordText = text
        , wordSuffix = Text.concat [suffixBeforeEmdash, emdash]
        }
      : buildWord afterEmdash

stripPunctuationPrefix :: Text -> (Text, Text)
stripPunctuationPrefix = Text.break Char.isLetter

stripPunctuationSuffix :: Text -> (Text, Text)
stripPunctuationSuffix input =
  let
    (suffixRev, textRev) = Text.break Char.isLetter . Text.reverse $ input
    suffix = Text.reverse suffixRev
    text = Text.reverse textRev
  in (text, suffix)

flattenParagraphFormat :: EditionOption -> ParagraphFormat -> Maybe Text
flattenParagraphFormat _ (ParagraphFormatPlain text) = Just text
flattenParagraphFormat _ (ParagraphFormatIndented paraLines) = Just $ flattenParagraphLines paraLines
flattenParagraphFormat editionOption (ParagraphFormatLaterEdition paraLines) =
  case editionOption of
    EarlyEdition -> Nothing
    LaterEdition -> Just . flattenParagraphLines $ paraLines
flattenParagraphFormat _ ParagraphFormatStarDivision = Nothing
flattenParagraphFormat _ ParagraphFormatChorusMarker = Nothing

flattenParagraphLines :: Seq Text -> Text
flattenParagraphLines = Text.intercalate " " . fmap Text.strip . Foldable.toList
