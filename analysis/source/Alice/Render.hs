module Alice.Render where

import qualified Alice.Structure
import           Data.Text (Text)
import qualified Data.Text as Text

wordToText :: Alice.Structure.Word -> Text
wordToText word = Text.concat
  [ Alice.Structure.wordPrefix word
  , Alice.Structure.wordText word
  , Alice.Structure.wordSuffix word
  ]
