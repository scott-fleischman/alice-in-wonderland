-- | Gutenberg text file loading and path

module Alice.TextFile where

import qualified Data.ByteString
import           Data.Text (Text)
import qualified Data.Text.Encoding

-- Gutenberg text file name
textFilePath :: FilePath
textFilePath = "AliceInWonderland.txt"

-- Load the file into memory
loadText :: FilePath -> IO Text
loadText path = do
  bytes <- Data.ByteString.readFile path
  return $ Data.Text.Encoding.decodeUtf8 bytes
