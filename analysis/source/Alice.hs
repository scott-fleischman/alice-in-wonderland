module Alice where

import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Data.Text

run :: FilePath -> IO ()
run path = do
  text <- Alice.TextFile.loadText path
  putStrLn $ "Text length = " ++ show (Data.Text.length text)
  result <-
    case Alice.Structure.parseBook text of
      Left err -> error (show err)
      Right result -> return result

  putStrLn "Before:"
  print $ Alice.Structure.bookBefore result

  putStrLn "\n\n\nBody:"
  print $ Alice.Structure.bookBody result

  putStrLn "After:"
  print $ Alice.Structure.bookBefore result

main :: IO ()
main = run Alice.TextFile.textFilePath
