{-# LANGUAGE TemplateHaskell #-}

import qualified Alice.Pdf
import qualified Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text
import qualified Hedgehog
import qualified System.Exit
import qualified System.IO

testPdfPath :: FilePath
testPdfPath = "../" ++ Alice.Pdf.pdfPath

prop_getPdfText :: Hedgehog.Property
prop_getPdfText =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    text <- liftIO $ Alice.Pdf.getPdfText testPdfPath
    Hedgehog.assert $ Data.Text.length text > 0

tests :: IO Bool
tests =
  Hedgehog.checkSequential $$(Hedgehog.discover)

main :: IO ()
main = do
  System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
  System.IO.hSetBuffering System.IO.stderr System.IO.LineBuffering

  isSuccess <- tests
  Control.Monad.unless isSuccess System.Exit.exitFailure
