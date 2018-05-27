{-# LANGUAGE OverloadedStrings #-}

module Alice.Structure where

import           Data.Sequence (Seq, ViewL((:<)), (<|), (|>), ViewR((:>)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Numeral.Roman as Numeral.Roman

newtype GutenbergPreamble = GutenbergPreamble (Seq Text) deriving Show
newtype GutenbergPostlude = GutenbergPostlude (Seq Text) deriving Show
newtype BookTitle = BookTitle (Seq Text) deriving Show
newtype ChapterNumber = ChapterNumber Int deriving Show
newtype ChapterTitle = ChapterTitle Text deriving Show
newtype ChapterContents = ChapterContents (Seq Text) deriving Show
newtype TheEnd = TheEnd Text deriving Show

data Error
  = NoGutenbergStartFound
  | NoGutenbergEndFound
  | NoTheEndFound
  | NonTheEndTextFoundAtEnd
  | NoChapterFound
  | InvalidChapterHeading Text
  | ChapterHeadingDotMismatch Text
  | InvalidRomanNumeral Text
  | EmptyChapterTitle
  deriving Show

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
  } deriving Show

parseBody :: Text -> Either Error Body
parseBody input = do
  let lineSequence = makeLineSequence input
  (preamble, afterPreamble) <- parsePreambleFromLeft lineSequence
  (beforePostlude, postlude) <- parsePostludeFromRight afterPreamble
  (beforeTheEnd, theEnd) <- parseTheEndFromRight beforePostlude
  (title, afterTitle) <- parseTitleFromLeft beforeTheEnd
  chapters <- parseAllChaptersFromLeft afterTitle
  return
    Body
    { bodyPreamble = preamble
    , bodyTitle = title
    , bodyChapters = chapters
    , bodyTheEnd = theEnd
    , bodyPostlude = postlude
    }

makeLineSequence :: Text -> Seq Text
makeLineSequence = Seq.fromList . Text.splitOn "\r\n"

parsePreambleFromLeft :: Seq Text -> Either Error (GutenbergPreamble, Seq Text)
parsePreambleFromLeft = go (GutenbergPreamble Seq.empty)
  where
  go (GutenbergPreamble soFar) input =
    case Seq.viewl input of
      Seq.EmptyL -> Left NoGutenbergStartFound
      line :< rest ->
        let newPreamble = GutenbergPreamble (line <| soFar)
        in if line == "*** START OF THIS PROJECT GUTENBERG EBOOK ALICE’S ADVENTURES IN WONDERLAND ***"
          then Right (newPreamble, rest)
          else go newPreamble rest

parsePostludeFromRight :: Seq Text -> Either Error (Seq Text, GutenbergPostlude)
parsePostludeFromRight = go (GutenbergPostlude Seq.empty)
  where
  go (GutenbergPostlude soFar) input =
    case Seq.viewr input of
      Seq.EmptyR -> Left NoGutenbergEndFound
      rest :> line ->
        let newPostlude = GutenbergPostlude (line <| soFar)
        in if line == "End of Project Gutenberg’s Alice’s Adventures in Wonderland, by Lewis Carroll"
          then Right (rest, newPostlude)
          else go newPostlude rest

parseTheEndFromRight :: Seq Text -> Either Error (Seq Text, TheEnd)
parseTheEndFromRight input =
  case Seq.viewr input of
    Seq.EmptyR -> Left NoTheEndFound
    rest :> line | (Text.null . Text.strip) line -> parseTheEndFromRight rest
    rest :> line | Text.strip line == "THE END" -> Right (rest, TheEnd line)
    _ :> _ -> Left NonTheEndTextFoundAtEnd

parseTitleFromLeft :: Seq Text -> Either Error (BookTitle, Seq Text)
parseTitleFromLeft = go (BookTitle Seq.empty)
  where
  go (BookTitle soFar) input =
    case Seq.viewl input of
      Seq.EmptyL -> Left NoChapterFound
      line :< rest ->
        if matchesChapter line
          then Right (BookTitle soFar, line <| rest)
          else go (BookTitle (line <| soFar)) rest

parseAllChaptersFromLeft :: Seq Text -> Either Error (Seq Chapter)
parseAllChaptersFromLeft = go Seq.empty
  where
  go chapters input = do
    result <- parseChapterFromLeft input
    case result of
      Left () -> return chapters
      Right (chapter, rest) -> go (chapters |> chapter) rest

parseChapterFromLeft :: Seq Text -> Either Error (Either () (Chapter, Seq Text))
parseChapterFromLeft fullInput =
  case Seq.viewl fullInput of
    Seq.EmptyL -> Right (Left ())
    line :< rest | (Text.null . Text.strip) line -> parseChapterFromLeft rest
    line :< rest | matchesChapter line -> do
      (number, title) <- parseChapterHeading line
      let (contents, moreChapters) = buildContents (ChapterContents Seq.empty) rest
      Right (Right (Chapter number title contents, moreChapters))
    line :< _ -> Left $ InvalidChapterHeading line
  where
  buildContents :: ChapterContents -> Seq Text -> (ChapterContents, Seq Text)
  buildContents (ChapterContents soFar) input =
    case Seq.viewl input of
      Seq.EmptyL -> (ChapterContents soFar, Seq.empty)
      line :< rest | matchesChapter line -> (ChapterContents soFar, line <| rest)
      line :< rest -> buildContents (ChapterContents (line <| soFar)) rest

matchesChapter :: Text -> Bool
matchesChapter = Text.isPrefixOf chapterPrefix

chapterPrefix :: Text
chapterPrefix = "CHAPTER "

parseChapterHeading :: Text -> Either Error (ChapterNumber, ChapterTitle)
parseChapterHeading input = do
  numberDotTitle <-
    case Text.stripPrefix chapterPrefix input of
      Nothing -> Left $ InvalidChapterHeading input
      Just rest -> Right rest
  let
    dotInfix = ". "
    (beforeDot, dotAndAfter) = Text.breakOn dotInfix numberDotTitle
  number <-
    case Numeral.Roman.fromRoman beforeDot of
      Nothing -> Left $ InvalidRomanNumeral beforeDot
      Just number -> Right number
  title <-
    case Text.stripPrefix dotInfix dotAndAfter of
      Nothing -> Left $ ChapterHeadingDotMismatch dotAndAfter
      Just title -> Right title
  Right (ChapterNumber number, ChapterTitle title)
