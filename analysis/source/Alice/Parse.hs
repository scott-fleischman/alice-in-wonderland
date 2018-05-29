{-# LANGUAGE OverloadedStrings #-}

module Alice.Parse where

import           Alice.Structure
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import           Data.Sequence (Seq((:<|), (:|>)), ViewL((:<)), (<|), (|>), ViewR((:>)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Numeral.Roman as Numeral.Roman

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
  | InvalidStarParagraphCount Int
  | LaterEditionEmpty
  | LaterEditionMissingCloseBracket Text
  deriving Show

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
  go (GutenbergPreamble _) Seq.Empty = Left NoGutenbergStartFound
  go (GutenbergPreamble soFar) (line :<| rest) =
    let newPreamble = GutenbergPreamble (line <| soFar)
    in if line == "*** START OF THIS PROJECT GUTENBERG EBOOK ALICE’S ADVENTURES IN WONDERLAND ***"
      then Right (newPreamble, rest)
      else go newPreamble rest

parsePostludeFromRight :: Seq Text -> Either Error (Seq Text, GutenbergPostlude)
parsePostludeFromRight = go (GutenbergPostlude Seq.empty)
  where
  go (GutenbergPostlude _) Seq.Empty = Left NoGutenbergEndFound
  go (GutenbergPostlude soFar) (rest :|> line) =
    let newPostlude = GutenbergPostlude (line <| soFar)
    in if line == "End of Project Gutenberg’s Alice’s Adventures in Wonderland, by Lewis Carroll"
      then Right (rest, newPostlude)
      else go newPostlude rest

parseTheEndFromRight :: Seq Text -> Either Error (Seq Text, TheEnd)
parseTheEndFromRight Seq.Empty = Left NoTheEndFound
parseTheEndFromRight (rest :|> line) | (Text.null . Text.strip) line = parseTheEndFromRight rest
parseTheEndFromRight (rest :|> line) | Text.strip line == "THE END" = Right (rest, TheEnd line)
parseTheEndFromRight (_ :|> line) = Left $ NonTheEndTextFoundAtEnd line

parseTitleFromLeft :: Seq Text -> Either Error (BookTitle, Seq Text)
parseTitleFromLeft = go (BookTitle Seq.empty)
  where
  go (BookTitle _) Seq.Empty = Left NoChapterFound
  go (BookTitle soFar) (line :<| rest) =
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
parseChapterFromLeft Seq.Empty = Right (Left ())
parseChapterFromLeft (line :<| rest) | (Text.null . Text.strip) line = parseChapterFromLeft rest
parseChapterFromLeft (line :<| rest) | matchesChapter line =
  do
    (number, title) <- parseChapterHeading line
    let (ChapterContents contents, moreChapters) = buildContents (ChapterContents Seq.empty) rest
    paragraphSeqs <- parseParagraphSeqs contents
    paragraphFormats <- parseParagraphFormats paragraphSeqs
    Right (Right (Chapter number title (ChapterContents contents) paragraphFormats, moreChapters))
parseChapterFromLeft (line :<| _) = Left $ InvalidChapterHeading line

buildContents :: ChapterContents -> Seq Text -> (ChapterContents, Seq Text)
buildContents (ChapterContents soFar) Seq.Empty = (ChapterContents soFar, Seq.empty)
buildContents (ChapterContents soFar) (line :<| rest) | matchesChapter line = (ChapterContents soFar, line <| rest)
buildContents (ChapterContents soFar) (line :<| rest) = buildContents (ChapterContents (soFar |> line)) rest

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

parseParagraphFromLeft :: Seq Text -> Either Error (ParagraphSeq, Seq Text)
parseParagraphFromLeft Seq.Empty = Left EmptyParagraph
parseParagraphFromLeft (line :<| rest) | Text.null line = parseParagraphFromLeft rest
parseParagraphFromLeft (line :<| rest) =
  let (ParagraphSeq soFar, finalRest) = buildParagraph (ParagraphSeq Seq.empty) rest
  in Right (ParagraphSeq (line <| soFar), finalRest)

buildParagraph :: ParagraphSeq -> Seq Text -> (ParagraphSeq, Seq Text)
buildParagraph (ParagraphSeq soFar) Seq.Empty = (ParagraphSeq soFar, Seq.empty)
buildParagraph (ParagraphSeq soFar) (line :<| rest) | Text.null line = (ParagraphSeq soFar, line :<| rest)
buildParagraph (ParagraphSeq soFar) (line :<| rest) = buildParagraph (ParagraphSeq (soFar |> line)) rest

parseParagraphSeqs :: Seq Text -> Either Error (Seq ParagraphSeq)
parseParagraphSeqs = go Seq.empty
  where
  go paragraphs input =
    case parseParagraphFromLeft input of
      Left EmptyParagraph -> Right paragraphs
      Left err -> Left err
      Right (p, more) -> go (paragraphs |> p) more

parseParagraphFormats :: Seq ParagraphSeq -> Either Error (Seq ParagraphFormat)
parseParagraphFormats = go Seq.empty
  where
  go paras Seq.Empty = Right paras
  go paras input@(ParagraphSeq paraLines :<| restParas) =
    let (starParaCount, afterStars) = getStarParaCount 0 input
    in if starParaCount == 0
      then
        case Seq.viewl paraLines of
          Seq.EmptyL -> Left EmptyParagraph
          firstLine :< restLines ->
            let isLaterEdition = ((== "[later editions continued as follows") . Text.strip) firstLine
            in if isLaterEdition
              then
                case Seq.viewr restLines of
                  Seq.EmptyR -> Left LaterEditionEmpty
                  beforeLastLine :> lastLine ->
                    case Text.stripSuffix "]" lastLine of
                      Nothing -> Left $ LaterEditionMissingCloseBracket lastLine
                      Just strippedLastLine -> go (paras |> ParagraphFormatLaterEdition (beforeLastLine |> strippedLastLine)) restParas
              else
                let startsWithSpace = (not . Text.null) firstLine && (Char.isSpace . Text.head) firstLine
                in case startsWithSpace of
                  True | ((== "CHORUS.") . Text.strip . Text.intercalate " " . Foldable.toList) paraLines ->
                    go (paras |> ParagraphFormatChorusMarker) restParas
                  True ->  go (paras |> ParagraphFormatIndented paraLines) restParas
                  False ->
                    let spacedText = (Text.intercalate " " . Foldable.toList) paraLines
                    in go (paras |> ParagraphFormatPlain spacedText) restParas
      else
        if starParaCount == 3
          then go (paras |> ParagraphFormatStarDivision) afterStars
          else Left $ InvalidStarParagraphCount starParaCount

getStarParaCount :: Int -> Seq ParagraphSeq -> (Int, Seq ParagraphSeq)
getStarParaCount numberSoFar Seq.Empty = (numberSoFar, Seq.empty)
getStarParaCount numberSoFar (ParagraphSeq paraLines :<| rest) =
  let paraText = (Text.concat . Foldable.toList) paraLines
  in if Text.all (\c -> Char.isSpace c || c == '*') paraText
    then getStarParaCount (numberSoFar + 1) rest
    else (numberSoFar, rest)
