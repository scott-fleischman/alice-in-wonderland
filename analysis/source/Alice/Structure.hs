{-# LANGUAGE OverloadedStrings #-}

module Alice.Structure where

import qualified Control.Lens as Lens
import           Data.Text (Text)
import qualified Data.Text as Text

data Book body = Book
  { bookBefore :: [Text]
  , bookBody :: body
  , bookAfter :: [Text]
  }

data Error = MarkerNotFound Text
  deriving Show

parseBook :: Text -> Either Error (Book [Text])
parseBook input =
  pure Book
  <*> fmap firstPart firstSplit
  <*> fmap firstPart secondSplit
  <*> fmap secondPart secondSplit
  where
  inputLines = Text.splitOn "\r\n" input
  firstSplit = takeMarker TakeThrough startMarker inputLines
  secondSplit = firstSplit >>= (takeMarker TakeUntil endMarker . secondPart)
  startMarker = "*** START OF THIS PROJECT GUTENBERG EBOOK ALICE’S ADVENTURES IN WONDERLAND ***"
  endMarker = "End of Project Gutenberg’s Alice’s Adventures in Wonderland, by Lewis Carroll"

data Partition = Partition
  { firstPart :: [Text]
  , secondPart :: [Text]
  }
firstPartLens :: Applicative f => ([Text] -> f [Text]) -> Partition -> f Partition
firstPartLens f (Partition x y) = pure Partition <*> f x <*> pure y

data TakeOption = TakeThrough | TakeUntil

takeMarker :: TakeOption -> Text -> [Text] -> Either Error Partition
takeMarker _ marker [] = Left $ MarkerNotFound marker
takeMarker TakeThrough marker (line : rest) | line == marker = Right $ Partition { firstPart = [line], secondPart = rest }
takeMarker TakeUntil   marker (line : rest) | line == marker = Right $ Partition { firstPart = [],     secondPart = line : rest }
takeMarker takeOption marker (line : rest)
  = Lens.over (Lens._Right . firstPartLens) (line :)
  $ takeMarker takeOption marker rest
