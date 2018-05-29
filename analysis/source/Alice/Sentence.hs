{-# LANGUAGE OverloadedStrings #-}

module Alice.Sentence where

import           Alice.Structure
import qualified Data.Foldable as Foldable
import           Data.Sequence (Seq((:<|)), (<|))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text

flattenParagraphs :: EditionOption -> Seq ParagraphFormat -> Seq Text

flattenParagraphs _ Seq.Empty = Seq.empty

flattenParagraphs editionOption (ParagraphFormatPlain text :<| restParas)
  = text <| flattenParagraphs editionOption restParas

flattenParagraphs editionOption (ParagraphFormatIndented paraLines :<| restParas)
  = flattenParagraphLines paraLines <| flattenParagraphs editionOption restParas

flattenParagraphs editionOption (ParagraphFormatLaterEdition paraLines :<| restParas) =
  case editionOption of
    EarlyEdition -> flattenParagraphs editionOption restParas
    LaterEdition -> flattenParagraphLines paraLines <| flattenParagraphs editionOption restParas

flattenParagraphs editionOption (ParagraphFormatStarDivision :<| restParas) = flattenParagraphs editionOption restParas


flattenParagraphLines :: Seq Text -> Text
flattenParagraphLines = Text.intercalate " " . Foldable.toList
