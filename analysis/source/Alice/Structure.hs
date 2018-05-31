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

data Body = Body
  { bodyPreamble :: GutenbergPreamble
  , bodyTitle :: BookTitle
  , bodyChapters :: Seq Chapter
  , bodyTheEnd :: TheEnd
  , bodyPostlude :: GutenbergPostlude
  } deriving (Eq, Ord, Show)

data Chapter = Chapter
  { chapterNumber :: ChapterNumber
  , chapterTitle :: ChapterTitle
  , chapterContents :: ChapterContents
  , chapterParagraphs :: Seq ParagraphFormat
  } deriving (Eq, Ord, Show)

data ParagraphFormat
  = ParagraphFormatPlain Text
  | ParagraphFormatIndented (Seq Text)
  | ParagraphFormatLaterEdition (Seq Text)
  | ParagraphFormatChorusMarker
  | ParagraphFormatStarDivision
  deriving (Eq, Ord, Show)

data EditionOption = EarlyEdition | LaterEdition deriving (Eq, Ord, Show)
data LastWordInParagraph = NotLastWordInParagraph | IsLastWordInParagraph deriving (Eq, Ord, Show)
data Word = Word
  { wordPrefix :: Text
  , wordText :: Text
  , wordSuffix :: Text
  , wordLast :: LastWordInParagraph
  } deriving (Eq, Ord, Show)

data WordContext = WordContext
  { wordContextBefore :: Seq Word
  , wordContextWord :: Word
  , wordContextAfter :: Seq Word
  } deriving (Eq, Ord, Show)
