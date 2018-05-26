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
  <*> fmap splitFirst split1
  <*> fmap splitFirst split2
  <*> fmap splitSecond split2
  where
  inputLines = Text.splitOn "\r\n" input
  split1 = takeCheck (MarkerNotFound startMarker) TakeThrough (== startMarker) inputLines
  split2 = split1 >>= (takeCheck (MarkerNotFound endMarker) TakeUntil (== endMarker) . splitSecond)
  startMarker = "*** START OF THIS PROJECT GUTENBERG EBOOK ALICE’S ADVENTURES IN WONDERLAND ***"
  endMarker = "End of Project Gutenberg’s Alice’s Adventures in Wonderland, by Lewis Carroll"

data Split item = Split
  { splitFirst :: [item]
  , splitSecond :: [item]
  }
splitFirstLens :: Applicative f => ([item] -> f [item]) -> (Split item -> f (Split item))
splitFirstLens f (Split x y) = pure Split <*> f x <*> pure y

data TakeOption = TakeThrough | TakeUntil

takeCheck :: Eq item => err -> TakeOption -> (item -> Bool) -> [item] -> Either err (Split item)
takeCheck err _ _ [] = Left err
takeCheck _ TakeThrough check (item : rest) | check item = Right $ Split { splitFirst = [item], splitSecond = rest }
takeCheck _ TakeUntil   check (item : rest) | check item = Right $ Split { splitFirst = [],     splitSecond = item : rest }
takeCheck err takeOption check (item : rest)
  = Lens.over (Lens._Right . splitFirstLens) (item :)
  $ takeCheck err takeOption check rest

data Body chapter = Body
  { bodyTitle :: [Text]
  , bodyChapters :: [chapter]
  }

data Chapter content = Chapter
  { chapterNumber :: Text
  , chapterTitle :: Text
  , chapterContent :: content
  }
