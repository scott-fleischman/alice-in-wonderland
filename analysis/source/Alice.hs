module Alice where

import qualified Alice.TextFile
import qualified Data.Text

run :: FilePath -> IO ()
run path = do
  text <- Alice.TextFile.loadText path
  putStrLn $ "Text length = " ++ show (Data.Text.length text)

main :: IO ()
main = run Alice.TextFile.textFilePath
