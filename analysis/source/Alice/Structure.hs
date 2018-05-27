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
newtype Paragraph = Paragraph (Seq Text) deriving Show

data Error
  = NoGutenbergStartFound
  | NoGutenbergEndFound
  | NoTheEndFound
  | NonTheEndTextFoundAtEnd Text
  | NoChapterFound
  | InvalidChapterHeading Text
  | ChapterHeadingDotMismatch Text
  | InvalidRomanNumeral Text
  | EmptyChapterTitle
  | EmptyParagraph
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
  , chapterParagraphs :: Seq Paragraph
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
    _ :> line -> Left $ NonTheEndTextFoundAtEnd line

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
      let (ChapterContents contents, moreChapters) = buildContents (ChapterContents Seq.empty) rest
      paragraphs <- parseParagraphs contents
      Right (Right (Chapter number title (ChapterContents contents) paragraphs, moreChapters))
    line :< _ -> Left $ InvalidChapterHeading line
  where
  buildContents :: ChapterContents -> Seq Text -> (ChapterContents, Seq Text)
  buildContents (ChapterContents soFar) input =
    case Seq.viewl input of
      Seq.EmptyL -> (ChapterContents soFar, Seq.empty)
      line :< rest | matchesChapter line -> (ChapterContents soFar, line <| rest)
      line :< rest -> buildContents (ChapterContents (soFar |> line)) rest

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

parseParagraphFromLeft :: Seq Text -> Either Error (Paragraph, Seq Text)
parseParagraphFromLeft input =
  case Seq.viewl input of
    Seq.EmptyL -> Left EmptyParagraph
    line :< rest | Text.null line -> parseParagraphFromLeft rest
    line :< rest ->
      let (Paragraph soFar, finalRest) = build (Paragraph Seq.empty) rest
      in Right (Paragraph (line <| soFar), finalRest)
  where
  build (Paragraph soFar) inputBuild =
    case Seq.viewl inputBuild of
      Seq.EmptyL -> (Paragraph soFar, Seq.empty)
      line :< _ | Text.null line -> (Paragraph soFar, inputBuild)
      line :< rest -> build (Paragraph (soFar |> line)) rest

parseParagraphs :: Seq Text -> Either Error (Seq Paragraph)
parseParagraphs = go Seq.empty
  where
  go paragraphs input =
    case parseParagraphFromLeft input of
      Left EmptyParagraph -> Right paragraphs
      Left err -> Left err
      Right (p, more) -> go (paragraphs |> p) more
