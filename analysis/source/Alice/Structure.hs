-- | Definitions of Body, Chapter and Paragraph structure in the Alice in Wonderland text.

module Alice.Structure where

import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Prelude hiding (Word)

newtype GutenbergPreamble = GutenbergPreamble (Seq Text) deriving (Eq, Ord, Show)
newtype GutenbergPostlude = GutenbergPostlude (Seq Text) deriving (Eq, Ord, Show)
newtype BookTitle = BookTitle (Seq Text) deriving (Eq, Ord, Show)
newtype ChapterNumber = ChapterNumber Int deriving (Eq, Ord, Show)
newtype ChapterTitle = ChapterTitle Text deriving (Eq, Ord, Show)
newtype ChapterContents = ChapterContents (Seq Text) deriving (Eq, Ord, Show)
newtype TheEnd = TheEnd Text deriving (Eq, Ord, Show)
newtype ParagraphSeq = ParagraphSeq (Seq Text) deriving (Eq, Ord, Show)
newtype BeforeCount = BeforeCount Int deriving (Eq, Ord, Show)
newtype AfterCount = AfterCount Int deriving (Eq, Ord, Show)
newtype Sentence = Sentence (Seq Word) deriving (Eq, Ord, Show)
newtype Indent = Indent Int deriving (Eq, Ord, Show)

-- Represents the entire text including Gutenberg pre/postludes.
data Body = Body
  { bodyPreamble :: GutenbergPreamble
  , bodyTitle :: BookTitle
  , bodyChapters :: Seq Chapter
  , bodyTheEnd :: TheEnd
  , bodyPostlude :: GutenbergPostlude
  } deriving (Eq, Ord, Show)

-- Chapter with structural paragraphs
data Chapter = Chapter
  { chapterNumber :: ChapterNumber
  , chapterTitle :: ChapterTitle
  , chapterContents :: ChapterContents
  , chapterParagraphs :: Seq ParagraphFormat
  } deriving (Eq, Ord, Show)

-- A paragraph with different formatting options
data ParagraphFormat
  = ParagraphFormatPlain Text -- plain text paragraph (just list of words)
  | ParagraphFormatIndented (Seq Text) -- poetic text paragraph with significant indent per line
  | ParagraphFormatLaterEdition (Seq Text) -- text that has been marked as belonging to a later edition (with the edition marker removed)
  | ParagraphFormatChorusMarker -- the "CHORUS" marker (always the same so we don't need to preserve it)
  | ParagraphFormatStarDivision -- the division indicated by stars which is used when Alice grows or shrinks
  deriving (Eq, Ord, Show)

-- Which edition: early or later
data EditionOption = EarlyEdition | LaterEdition deriving (Eq, Ord, Show)

-- Whether a word is the last word in the paragraph or not
data LastWordInParagraph = NotLastWordInParagraph | IsLastWordInParagraph deriving (Eq, Ord, Show)

-- Word structural information
data Word = Word
  { wordIndent :: Indent -- How much indent (in spaces) this word has
  , wordPrefix :: Text -- punctuation as prefix of word
  , wordText :: Text -- the core word text itself, including inline apostrophe
  , wordSuffix :: Text -- punctuation at end of word
  , wordLast :: LastWordInParagraph -- whether it's the last word in the paragraph
  } deriving (Eq, Ord, Show)

-- Context that has zero or more preceding words and zero or more following words
data WordContext = WordContext
  { wordContextBefore :: Seq Word
  , wordContextWord :: Word
  , wordContextAfter :: Seq Word
  } deriving (Eq, Ord, Show)
