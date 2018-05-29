{-# LANGUAGE OverloadedStrings #-}

module Alice.Sentence where

import           Alice.Structure
import qualified Data.Foldable as Foldable
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as Text

flattenParagraphFormat :: EditionOption -> ParagraphFormat -> Maybe Text
flattenParagraphFormat _ (ParagraphFormatPlain text) = Just text
flattenParagraphFormat _ (ParagraphFormatIndented paraLines) = Just $ flattenParagraphLines paraLines
flattenParagraphFormat editionOption (ParagraphFormatLaterEdition paraLines) =
  case editionOption of
    EarlyEdition -> Nothing
    LaterEdition -> Just . flattenParagraphLines $ paraLines
flattenParagraphFormat _ (ParagraphFormatStarDivision) = Nothing

flattenParagraphLines :: Seq Text -> Text
flattenParagraphLines = Text.intercalate " " . fmap Text.strip . Foldable.toList
