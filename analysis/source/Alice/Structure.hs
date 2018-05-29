module Alice.Structure where

import           Data.Sequence (Seq)
import           Data.Text (Text)

newtype GutenbergPreamble = GutenbergPreamble (Seq Text) deriving Show
newtype GutenbergPostlude = GutenbergPostlude (Seq Text) deriving Show
newtype BookTitle = BookTitle (Seq Text) deriving Show
newtype ChapterNumber = ChapterNumber Int deriving Show
newtype ChapterTitle = ChapterTitle Text deriving Show
newtype ChapterContents = ChapterContents (Seq Text) deriving Show
newtype TheEnd = TheEnd Text deriving Show
newtype ParagraphSeq = ParagraphSeq (Seq Text) deriving Show

data Body = Body
  { bodyPreamble :: GutenbergPreamble
  , bodyTitle :: BookTitle
  , bodyChapters :: Seq Chapter
  , bodyTheEnd :: TheEnd
  , bodyPostlude :: GutenbergPostlude
  } deriving Show

data Chapter = Chapter
  { chapterNumber :: ChapterNumber
  , chapterTitle :: ChapterTitle
  , chapterContents :: ChapterContents
  , chapterParagraphs :: Seq ParagraphFormat
  } deriving Show

data ParagraphFormat
  = ParagraphFormatPlain Text
  | ParagraphFormatIndented (Seq Text)
  | ParagraphFormatLaterEdition (Seq Text)
  | ParagraphFormatStarDivision
  deriving Show

data EditionOption = EarlyEdition | LaterEdition
