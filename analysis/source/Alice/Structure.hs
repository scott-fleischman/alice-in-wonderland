{-# LANGUAGE OverloadedStrings #-}

module Alice.Structure where

import           Data.Sequence (Seq, ViewL((:<)), (<|), ViewR((:>)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text

newtype GutenbergPreamble = GutenbergPreamble (Seq Text) deriving Show
newtype GutenbergPostlude = GutenbergPostlude (Seq Text) deriving Show
newtype Title = Title (Seq Text) deriving Show
newtype TheEnd = TheEnd Text deriving Show

data Error
  = NoGutenbergStartFound
  | NoGutenbergEndFound
  | NoTheEndFound
  | NonTheEndTextFoundAtEnd
  | NoChapterFound
  deriving Show

data Body = Body
  { bodyPreamble :: GutenbergPreamble
  , bodyTitle :: Title
  , bodyChapters :: Seq Text
  , bodyTheEnd :: TheEnd
  , bodyPostlude :: GutenbergPostlude
  } deriving Show

parseBody :: Text -> Either Error Body
parseBody input = do
  let lineSequence = makeLineSequence input
  (preamble, afterPreamble) <- parsePreambleFromLeft lineSequence
  (beforePostlude, postlude) <- parsePostludeFromRight afterPreamble
  (beforeTheEnd, theEnd) <- parseTheEndFromRight beforePostlude
  (title, chapters) <- parseTitleFromLeft beforeTheEnd
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

parseTitleFromLeft :: Seq Text -> Either Error (Title, Seq Text)
parseTitleFromLeft = go (Title Seq.empty)
  where
  go (Title soFar) input =
    case Seq.viewl input of
      Seq.EmptyL -> Left NoChapterFound
      line :< rest ->
        if matchesChapter line
          then Right (Title soFar, line <| rest)
          else go (Title (line <| soFar)) rest

matchesChapter :: Text -> Bool
matchesChapter = Text.isPrefixOf "CHAPTER "
