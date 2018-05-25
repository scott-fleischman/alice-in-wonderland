{-# LANGUAGE TemplateHaskell #-}

import qualified Alice
import qualified Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO
import qualified Hedgehog
import qualified System.Exit
import qualified System.IO

testPdfPath :: FilePath
testPdfPath = "../" ++ Alice.pdfPath

prop_pageCount :: Hedgehog.Property
prop_pageCount =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    liftIO $ Data.Text.IO.putStrLn =<< Alice.run testPdfPath

tests :: IO Bool
tests =
  Hedgehog.checkSequential $$(Hedgehog.discover)

main :: IO ()
main = do
  System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
  System.IO.hSetBuffering System.IO.stderr System.IO.LineBuffering

  isSuccess <- tests
  Control.Monad.unless isSuccess System.Exit.exitFailure
