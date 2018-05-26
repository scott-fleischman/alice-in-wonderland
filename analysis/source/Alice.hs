module Alice where

import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Data.Text
import qualified Data.Text.IO

run :: FilePath -> IO ()
run path = do
  text <- Alice.TextFile.loadText path
  putStrLn $ "Text length = " ++ show (Data.Text.length text)
  result <-
    case Alice.Structure.parseBook text of
      Left err -> error (show err)
      Right result -> return result

  putStrLn "\nContent:\n"
  mapM_ Data.Text.IO.putStrLn $ Alice.Structure.bookBody result

main :: IO ()
main = run Alice.TextFile.textFilePath
