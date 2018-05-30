module Alice.Structure where

import           Data.Sequence (Seq)
import           Data.Text (Text)

newtype GutenbergPreamble = GutenbergPreamble (Seq Text) deriving (Eq, Show)
newtype GutenbergPostlude = GutenbergPostlude (Seq Text) deriving (Eq, Show)
newtype BookTitle = BookTitle (Seq Text) deriving (Eq, Show)
newtype ChapterNumber = ChapterNumber Int deriving (Eq, Show)
newtype ChapterTitle = ChapterTitle Text deriving (Eq, Show)
newtype ChapterContents = ChapterContents (Seq Text) deriving (Eq, Show)
newtype TheEnd = TheEnd Text deriving (Eq, Show)
newtype ParagraphSeq = ParagraphSeq (Seq Text) deriving (Eq, Show)

data Body = Body
  { bodyPreamble :: GutenbergPreamble
  , bodyTitle :: BookTitle
  , bodyChapters :: Seq Chapter
  , bodyTheEnd :: TheEnd
  , bodyPostlude :: GutenbergPostlude
  } deriving (Eq, Show)

data Chapter = Chapter
  { chapterNumber :: ChapterNumber
  , chapterTitle :: ChapterTitle
  , chapterContents :: ChapterContents
  , chapterParagraphs :: Seq ParagraphFormat
  } deriving (Eq, Show)

data ParagraphFormat
  = ParagraphFormatPlain Text
  | ParagraphFormatIndented (Seq Text)
  | ParagraphFormatLaterEdition (Seq Text)
  | ParagraphFormatChorusMarker
  | ParagraphFormatStarDivision
  deriving (Eq, Show)

data EditionOption = EarlyEdition | LaterEdition deriving (Eq, Show)
data LastWordInParagraph = NotLastWordInParagraph | IsLastWordInParagraph deriving (Eq, Show)
data Word = Word
  { wordPrefix :: Text
  , wordText :: Text
  , wordSuffix :: Text
  } deriving (Eq, Show)
