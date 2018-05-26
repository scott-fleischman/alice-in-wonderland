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
  <*> fmap firstPart initialParts
  <*> fmap firstPart remainderPart
  <*> fmap secondPart remainderPart
  where
  inputLines = Text.splitOn "\r\n" input
  initialParts = takeThroughMarker startMarker inputLines
  remainderPart = initialParts >>= (takeThroughMarker endMarker . secondPart)
  startMarker = "*** START OF THIS PROJECT GUTENBERG EBOOK ALICE’S ADVENTURES IN WONDERLAND ***"
  endMarker = "End of Project Gutenberg’s Alice’s Adventures in Wonderland, by Lewis Carroll"

data Partition = Partition
  { firstPart :: [Text]
  , secondPart :: [Text]
  }
firstPartLens :: Applicative f => ([Text] -> f [Text]) -> Partition -> f Partition
firstPartLens f (Partition x y) = pure Partition <*> f x <*> pure y

takeThroughMarker :: Text -> [Text] -> Either Error Partition
takeThroughMarker marker [] = Left $ MarkerNotFound marker
takeThroughMarker marker (line : rest) | line == marker = Right $ Partition { firstPart = [line], secondPart = rest }
takeThroughMarker marker (line : rest)
  = Lens.over (Lens._Right . firstPartLens) (line :)
  $ takeThroughMarker marker rest
