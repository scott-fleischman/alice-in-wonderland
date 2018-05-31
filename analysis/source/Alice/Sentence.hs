{-# LANGUAGE OverloadedStrings #-}

module Alice.Sentence where

import           Alice.Structure
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import           Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (Word, words)

parseAllSentences :: Seq Word -> Seq Sentence
parseAllSentences words = go Seq.empty wordContexts
  where
  wordContexts = contextualizeWords (BeforeCount 0) (AfterCount 1) words
  go sentences context =
    case parseSentence context of
      (Nothing, _) -> sentences
      (Just sentence, rest) -> go (sentences :|> sentence) rest

parseSentence :: Seq WordContext -> (Maybe Sentence, Seq WordContext)
parseSentence = go Seq.empty
  where
  go Seq.Empty Seq.Empty = (Nothing, Seq.empty)
  go wordsSoFar@(_ :<| _) Seq.Empty = (Just (Sentence wordsSoFar), Seq.empty)
  go wordsSoFar (nextWordContext :<| rest)
    | IsEndOfSentence <- isEndOfSentence nextWordContext = (Just (Sentence wordsNext), rest)
    | otherwise = go wordsNext rest
    where
    wordsNext = wordsSoFar :|> wordContextWord nextWordContext

data SentenceEnd = IsEndOfSentence | NotEndOfSentence

isEndOfSentence :: WordContext -> SentenceEnd
isEndOfSentence wordContext
  | word <- wordContextWord wordContext
  , IsLastWordInParagraph <- wordLast word = IsEndOfSentence

  | word <- wordContextWord wordContext
  , suffix <- wordSuffix word
  , Just _ <- Text.find (\c -> c == '.' || c == '!' || c == '?') suffix
  , (nextWord :<| _) <- wordContextAfter wordContext
  , nextWordText <- wordText nextWord
  , Just (firstChar, _) <- Text.uncons nextWordText
  , Char.isUpper firstChar = IsEndOfSentence

  | otherwise = NotEndOfSentence

allParagraphWords :: EditionOption -> Seq ParagraphFormat -> Seq Word
allParagraphWords editionOption = Foldable.foldMap (paragraphWords editionOption)

paragraphWords :: EditionOption -> ParagraphFormat -> Seq Word
paragraphWords editionOption paragraphs =
  let
    initialResult = textWords . maybe "" id . flattenParagraphFormat editionOption $ paragraphs
  in case initialResult of
    Seq.Empty -> Seq.empty
    rest :|> lastWord -> rest :|> lastWord { wordLast = IsLastWordInParagraph }

textWords :: Text -> Seq Word
textWords = Foldable.foldMap buildWord . Seq.fromList . Text.words

buildWord :: Text -> Seq Word
buildWord input | Text.null input = Seq.empty
buildWord input =
  let
    (prefix, afterPrefix) = stripPunctuationPrefix input
    emdash = "--"
    (beforeEmdash, emdashAndAfter) = Text.breakOn emdash afterPrefix
    (text, suffixBeforeEmdash) = stripPunctuationSuffix beforeEmdash
  in case Text.stripPrefix emdash emdashAndAfter of
    Nothing ->
      Seq.singleton $ Word
        { wordPrefix = prefix
        , wordText = text
        , wordSuffix = suffixBeforeEmdash
        , wordLast = NotLastWordInParagraph
        }

    Just afterEmdash ->
      Word
        { wordPrefix = prefix
        , wordText = text
        , wordSuffix = Text.concat [suffixBeforeEmdash, emdash]
        , wordLast = NotLastWordInParagraph
        }
      :<| buildWord afterEmdash

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

contextualizeWords :: BeforeCount -> AfterCount -> Seq Word -> Seq WordContext
contextualizeWords (BeforeCount beforeCount) (AfterCount afterCount) allItems = go Seq.empty (Seq.drop 1 allItems) allItems
  where
  go _ _ Seq.Empty = Seq.empty
  go beforeItems afterItems (word :<| rest) =
    let
      trimmedBefore = reduceToSizeFromLeft beforeCount beforeItems
      trimmedAfter = reduceToSizeFromRight afterCount afterItems
    in WordContext
      { wordContextBefore = trimmedBefore
      , wordContextWord = word
      , wordContextAfter = trimmedAfter
      }
      :<| go (trimmedBefore :|> word) (Seq.drop 1 rest) rest

reduceToSizeFromLeft :: Int -> Seq a -> Seq a
reduceToSizeFromLeft _ Seq.Empty = Seq.empty
reduceToSizeFromLeft size items@(_ :<| rest)
  | Seq.length items <= size = items
  | otherwise = reduceToSizeFromLeft size rest

reduceToSizeFromRight :: Int -> Seq a -> Seq a
reduceToSizeFromRight _ Seq.Empty = Seq.empty
reduceToSizeFromRight size items@(rest :|> _)
  | Seq.length items <= size = items
  | otherwise = reduceToSizeFromRight size rest
