module Alice.TextFile where

import qualified Data.ByteString
import           Data.Text (Text)
import qualified Data.Text.Encoding

textFilePath :: FilePath
textFilePath = "AliceInWonderland.txt"

loadText :: FilePath -> IO Text
loadText path = do
  bytes <- Data.ByteString.readFile path
  return $ Data.Text.Encoding.decodeUtf8 bytes
