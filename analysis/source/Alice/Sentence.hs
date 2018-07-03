-- | Break words up into sentences.
-- This algorithm was developed iteratively by attempting one parsing,
-- checking the output, and repeating until a parsing approach yielded
-- high-quality results.

{-# LANGUAGE OverloadedStrings #-}

module Alice.Sentence where

import qualified Alice.Render
import           Alice.Structure
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import           Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (Word, words)

-- Parse the words (in a paragraph, say) into sentences.
parseAllSentences :: Seq Word -> Seq Sentence
parseAllSentences words = go Seq.empty wordContexts
  where
  wordContexts = contextualizeWords (BeforeCount 0) (AfterCount 1) words
  go sentences context =
    case parseSentence context of
      (Nothing, _) -> sentences
      (Just sentence, rest) -> go (sentences :|> sentence) rest

-- Parse a single sentence and preserve the remaining words
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

-- Instead of a boolean, use our own type to represent whether the word is at the end of a sentence or not
data SentenceEnd = IsEndOfSentence | NotEndOfSentence

-- Given the word and surrounding context, is the the last word in a sentence?
isEndOfSentence :: WordContext -> SentenceEnd
isEndOfSentence wordContext
  -- all words at the end of a paragraph are at the end of a sentence
  | word <- wordContextWord wordContext
  , IsLastWordInParagraph <- wordLast word = IsEndOfSentence

  -- IF the word contains trailing punctuation containing a
  --   * period
  --   * exclamation mark
  --   * question mark
  -- AND if the next word is capitalized
  -- THEN it is the end of a sentence.
  --
  -- Note this will fail for cases like "'Oh!' Alice exclaimed."
  -- I'm not sure it's possible to know that is a single sentence (this algorithm parses it as two sentences)
  -- without access to a much deeper textual analysis.
  | word <- wordContextWord wordContext
  , suffix <- wordSuffix word
  , Just _ <- Text.find (\c -> c == '.' || c == '!' || c == '?') suffix
  , (nextWord :<| _) <- wordContextAfter wordContext
  , nextWordText <- wordText nextWord
  , Just (firstChar, _) <- Text.uncons nextWordText
  , Char.isUpper firstChar = IsEndOfSentence

  -- If the above two checks are false, then it's not the end of a sentence
  | otherwise = NotEndOfSentence

-- Get a flat list of words for all paragraphs
allParagraphWords :: EditionOption -> Seq ParagraphFormat -> Seq Word
allParagraphWords editionOption = Foldable.foldMap (paragraphWords editionOption)

-- Get a flat list of words for a paragraph (ignores paragraph structure like indentation)
paragraphWords :: EditionOption -> ParagraphFormat -> Seq Word
paragraphWords _ (ParagraphFormatPlain text) = (setLastWordInParagraph . textWords) text
paragraphWords _ (ParagraphFormatIndented paraLines) = (setLastWordInParagraph . Foldable.foldMap textWords) paraLines
paragraphWords editionOption (ParagraphFormatLaterEdition paraLines) =
  case editionOption of
    EarlyEdition -> Seq.empty
    LaterEdition -> (setLastWordInParagraph . Foldable.foldMap textWords) paraLines
paragraphWords _ ParagraphFormatStarDivision = Seq.empty
paragraphWords _ ParagraphFormatChorusMarker = Seq.empty

-- set the flag whether this is the last word in the paragraph or not
setLastWordInParagraph :: Seq Word -> Seq Word
setLastWordInParagraph Seq.Empty = Seq.empty
setLastWordInParagraph (rest :|> lastWord) = rest :|> lastWord { wordLast = IsLastWordInParagraph }

-- break a text into words preserving significant line indentation in poetry
textWords :: Text -> Seq Word
textWords text =
  let
    initialSpaces = Text.takeWhile Char.isSpace text
    firstIndent = Indent (Text.length initialSpaces)
    strippedText = Text.stripStart text
    initialWords = Foldable.foldMap buildWord . Seq.fromList . Text.words $ strippedText
  in case initialWords of
    Seq.Empty -> Seq.empty
    firstWord :<| rest -> firstWord { wordIndent = firstIndent } :<| rest

-- check if a "word" has emdashes in the middle and keep splitting until all emdashed words are divided
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
        { wordIndent = Indent 0
        , wordPrefix = prefix
        , wordText = text
        , wordSuffix = suffixBeforeEmdash
        , wordLast = NotLastWordInParagraph
        }

    Just afterEmdash ->
      -- consume any additional punctuation after the emdash
      let
        punctuationAfterEmdash = Text.takeWhile Char.isPunctuation afterEmdash
        nextWords = Text.drop (Text.length punctuationAfterEmdash) afterEmdash
      in Word
        { wordIndent = Indent 0
        , wordPrefix = prefix
        , wordText = text
        , wordSuffix = Text.concat [suffixBeforeEmdash, emdash, punctuationAfterEmdash]
        , wordLast = NotLastWordInParagraph
        }
      :<| buildWord nextWords

-- extract the initial punctuation from a word
stripPunctuationPrefix :: Text -> (Text, Text)
stripPunctuationPrefix = Text.break Char.isLetter

-- extract the trailing punctuation from a word
stripPunctuationSuffix :: Text -> (Text, Text)
stripPunctuationSuffix input =
  let
    (suffixRev, textRev) = Text.break Char.isLetter . Text.reverse $ input
    suffix = Text.reverse suffixRev
    text = Text.reverse textRev
  in (text, suffix)

-- render the paragraph text as a flat string of words separated by spaces, ignoring all extra paragraph markers
-- such as star divisions, chorus and poetic indentation
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
flattenParagraphLines = Alice.Render.concatTextWords . fmap Text.strip

-- Create a sequence of words with surrounding context from an initial sequence of plain words.
-- You express how many words before and after you want for context.
contextualizeWords :: BeforeCount -> AfterCount -> Seq Word -> Seq WordContext
contextualizeWords = contextualizeItems WordContext

-- An abstraction so you can create reuse the word contextualization using different word data structures
contextualizeItems :: (Seq a -> a -> Seq a -> b) -> BeforeCount -> AfterCount -> Seq a -> Seq b
contextualizeItems f (BeforeCount beforeCount) (AfterCount afterCount) allItems = go Seq.empty (Seq.drop 1 allItems) allItems
  where
  go _ _ Seq.Empty = Seq.empty
  go beforeItems afterItems (word :<| rest) =
    let
      trimmedBefore = reduceToSizeFromLeft beforeCount beforeItems
      trimmedAfter = reduceToSizeFromRight afterCount afterItems
    in f trimmedBefore word trimmedAfter
      :<| go (trimmedBefore :|> word) (Seq.drop 1 rest) rest

-- Limit a sequence to a certain size by removing items from the left (front).
-- Useful to limit preceding word context to a certain size.
reduceToSizeFromLeft :: Int -> Seq a -> Seq a
reduceToSizeFromLeft _ Seq.Empty = Seq.empty
reduceToSizeFromLeft size items@(_ :<| rest)
  | Seq.length items <= size = items
  | otherwise = reduceToSizeFromLeft size rest

-- Limit a sequence to a certain size by removing items from the right (end).
-- Useful to limit trailing word context to a certain size.
reduceToSizeFromRight :: Int -> Seq a -> Seq a
reduceToSizeFromRight _ Seq.Empty = Seq.empty
reduceToSizeFromRight size items@(rest :|> _)
  | Seq.length items <= size = items
  | otherwise = reduceToSizeFromRight size rest
