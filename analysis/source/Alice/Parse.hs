-- | Parse Gutenberg text into chapters and paragraphs with indentation differences.

{-# LANGUAGE OverloadedStrings #-}

module Alice.Parse where

import qualified Alice.Render
import           Alice.Structure
import qualified Data.Char as Char
import           Data.Sequence (Seq((:<|), (:|>)), (<|), (|>))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Numeral.Roman as Numeral.Roman

-- all of the possible parsing errors
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

-- main parsing function
parseBody :: Text -> Either Error Body
parseBody input = do
  let lineSequence = makeLineSequence input

  -- parse out the Gutenberg preamble and postlude
  (preamble, afterPreamble) <- parsePreambleFromLeft lineSequence
  (beforePostlude, postlude) <- parsePostludeFromRight afterPreamble
  (beforeTheEnd, theEnd) <- parseTheEndFromRight beforePostlude

  -- parse out the title of the document
  (title, afterTitle) <- parseTitleFromLeft beforeTheEnd

  -- parse each chapter (which parses paragraphs too)
  chapters <- parseAllChaptersFromLeft afterTitle

  -- return all of the above structure
  return
    Body
    { bodyPreamble = preamble
    , bodyTitle = title
    , bodyChapters = chapters
    , bodyTheEnd = theEnd
    , bodyPostlude = postlude
    }

-- parse Gutenberg text into lines using the Windows line ending pattern
makeLineSequence :: Text -> Seq Text
makeLineSequence = Seq.fromList . Text.splitOn "\r\n"

-- parse the text from the beginning up to and including the Gutenberg preamble
parsePreambleFromLeft :: Seq Text -> Either Error (GutenbergPreamble, Seq Text)
parsePreambleFromLeft = go (GutenbergPreamble Seq.empty)
  where
  go (GutenbergPreamble _) Seq.Empty = Left NoGutenbergStartFound
  go (GutenbergPreamble soFar) (line :<| rest) =
    let newPreamble = GutenbergPreamble (line <| soFar)
    in if line == "*** START OF THIS PROJECT GUTENBERG EBOOK ALICE’S ADVENTURES IN WONDERLAND ***"
      then Right (newPreamble, rest)
      else go newPreamble rest

-- parse the text from the "right" (end of the document) to extract the Gutenberg postlude
parsePostludeFromRight :: Seq Text -> Either Error (Seq Text, GutenbergPostlude)
parsePostludeFromRight = go (GutenbergPostlude Seq.empty)
  where
  go (GutenbergPostlude _) Seq.Empty = Left NoGutenbergEndFound
  go (GutenbergPostlude soFar) (rest :|> line) =
    let newPostlude = GutenbergPostlude (line <| soFar)
    in if line == "End of Project Gutenberg’s Alice’s Adventures in Wonderland, by Lewis Carroll"
      then Right (rest, newPostlude)
      else go newPostlude rest

-- parse the "THE END" text from the end of the document
parseTheEndFromRight :: Seq Text -> Either Error (Seq Text, TheEnd)
parseTheEndFromRight Seq.Empty = Left NoTheEndFound
parseTheEndFromRight (rest :|> line) | (Text.null . Text.strip) line = parseTheEndFromRight rest
parseTheEndFromRight (rest :|> line) | Text.strip line == "THE END" = Right (rest, TheEnd line)
parseTheEndFromRight (_ :|> line) = Left $ NonTheEndTextFoundAtEnd line

-- parse the title text from the start of the document
parseTitleFromLeft :: Seq Text -> Either Error (BookTitle, Seq Text)
parseTitleFromLeft = go (BookTitle Seq.empty)
  where
  go (BookTitle _) Seq.Empty = Left NoChapterFound
  go (BookTitle soFar) (line :<| rest) =
    if matchesChapter line
      then Right (BookTitle soFar, line <| rest)
      else go (BookTitle (line <| soFar)) rest

-- parse the chapters starting from the beginning
parseAllChaptersFromLeft :: Seq Text -> Either Error (Seq Chapter)
parseAllChaptersFromLeft = go Seq.empty
  where
  go chapters input = do
    result <- parseChapterFromLeft input
    case result of
      Nothing -> return chapters
      Just (chapter, rest) -> go (chapters |> chapter) rest

-- parse a single chapter and preserve remaining text for more parsing (more chapters or end of document)
parseChapterFromLeft :: Seq Text -> Either Error (Maybe (Chapter, Seq Text))
parseChapterFromLeft Seq.Empty = Right Nothing
parseChapterFromLeft (line :<| rest) | (Text.null . Text.strip) line = parseChapterFromLeft rest
parseChapterFromLeft (line :<| rest) | matchesChapter line =
  do
    (number, title) <- parseChapterHeading line
    let (ChapterContents contents, moreChapters) = buildContents (ChapterContents Seq.empty) rest
    paragraphSeqs <- parseParagraphSeqs contents
    paragraphFormats <- parseParagraphFormats paragraphSeqs
    Right (Just (Chapter number title (ChapterContents contents) paragraphFormats, moreChapters))
parseChapterFromLeft (line :<| _) = Left $ InvalidChapterHeading line

-- break text on chapter headings, and match chapter heading with its contents
buildContents :: ChapterContents -> Seq Text -> (ChapterContents, Seq Text)
buildContents (ChapterContents soFar) Seq.Empty = (ChapterContents soFar, Seq.empty)
buildContents (ChapterContents soFar) (line :<| rest) | matchesChapter line = (ChapterContents soFar, line <| rest)
buildContents (ChapterContents soFar) (line :<| rest) = buildContents (ChapterContents (soFar |> line)) rest

-- does this line match the chapter heading marker
matchesChapter :: Text -> Bool
matchesChapter = Text.isPrefixOf chapterPrefix

-- the text that each chapter line starts with
chapterPrefix :: Text
chapterPrefix = "CHAPTER "

-- parse a full chapter heading including parsing the roman numerals and chapter title
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

-- parse a paragraph. paragraphs end on a blank line
parseParagraphFromLeft :: Seq Text -> Either Error (ParagraphSeq, Seq Text)
parseParagraphFromLeft Seq.Empty = Left EmptyParagraph
parseParagraphFromLeft (line :<| rest) | Text.null line = parseParagraphFromLeft rest
parseParagraphFromLeft (line :<| rest) =
  let (ParagraphSeq soFar, finalRest) = buildParagraph (ParagraphSeq Seq.empty) rest
  in Right (ParagraphSeq (line <| soFar), finalRest)

-- build the paragraph text stopping on a blank line
buildParagraph :: ParagraphSeq -> Seq Text -> (ParagraphSeq, Seq Text)
buildParagraph (ParagraphSeq soFar) Seq.Empty = (ParagraphSeq soFar, Seq.empty)
buildParagraph (ParagraphSeq soFar) (line :<| rest) | Text.null line = (ParagraphSeq soFar, line :<| rest)
buildParagraph (ParagraphSeq soFar) (line :<| rest) = buildParagraph (ParagraphSeq (soFar |> line)) rest

-- parse a sequence of paragraphs that are the content of a chapter
parseParagraphSeqs :: Seq Text -> Either Error (Seq ParagraphSeq)
parseParagraphSeqs = go Seq.empty
  where
  go paragraphs input =
    case parseParagraphFromLeft input of
      Left EmptyParagraph -> Right paragraphs
      Left err -> Left err
      Right (p, more) -> go (paragraphs |> p) more

-- parse a seequence of paragraphs into significant structure, including:
-- * star divisions (Alice grows or shrinks)
-- * later edition marker (this is removed from the text and preserved structurally in our data format)
-- * chorus marker (this is removed from the raw text and preserved in our data format)
-- * paragraphs with significant indentation (poetry or the "tail" effect)
parseParagraphFormats :: Seq ParagraphSeq -> Either Error (Seq ParagraphFormat)
parseParagraphFormats = go Seq.empty
  where
  go paras Seq.Empty = Right paras
  go paras input@(_ :<| _)
    | (starParaCount, afterStars) <- getStarParaCount input, starParaCount > 0
    = if starParaCount == 3
        then go (paras |> ParagraphFormatStarDivision) afterStars
        else Left $ InvalidStarParagraphCount starParaCount
  go _ (ParagraphSeq Seq.Empty :<| _) = Left EmptyParagraph
  go paras (ParagraphSeq (firstLine :<| restLines) :<| restParas)
    | ((== "[later editions continued as follows") . Text.strip) firstLine
    = case restLines of
        Seq.Empty -> Left LaterEditionEmpty
        beforeLastLine :|> lastLine ->
          case Text.stripSuffix "]" lastLine of
            Nothing -> Left $ LaterEditionMissingCloseBracket lastLine
            Just strippedLastLine -> go (paras |> ParagraphFormatLaterEdition (beforeLastLine |> strippedLastLine)) restParas
  go paras (ParagraphSeq paraLines@(firstLine :<| _) :<| restParas)
    | (not . Text.null) firstLine
    , (Char.isSpace . Text.head) firstLine
    = if ((== "CHORUS.") . Text.strip . Alice.Render.concatTextWords) paraLines
      then go (paras |> ParagraphFormatChorusMarker) restParas
      else go (paras |> ParagraphFormatIndented paraLines) restParas
  go paras (ParagraphSeq paraLines :<| restParas) =
    let spacedText = Alice.Render.concatTextWords paraLines
    in go (paras |> ParagraphFormatPlain spacedText) restParas

-- count the number of lines composed only of stars and spaces (indicating the "star division" described above)
getStarParaCount :: Seq ParagraphSeq -> (Int, Seq ParagraphSeq)
getStarParaCount = go 0
  where
  go numberSoFar Seq.Empty = (numberSoFar, Seq.empty)
  go numberSoFar (ParagraphSeq paraLines :<| rest) =
    let paraText = Alice.Render.concatTextWords paraLines
    in if Text.all (\c -> Char.isSpace c || c == '*') paraText
      then go (numberSoFar + 1) rest
      else (numberSoFar, rest)
