{-# LANGUAGE TemplateHaskell #-}

import qualified Alice
import qualified Alice.Pdf
import qualified Alice.TextFile
import qualified Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text
import qualified Hedgehog
import qualified System.Exit
import           System.FilePath ((</>))
-- import qualified System.IO

testPathPrefix :: FilePath
testPathPrefix = ".."

testPdfPath :: FilePath
testPdfPath = testPathPrefix </> Alice.Pdf.pdfPath

testTextFilePath :: FilePath
testTextFilePath = testPathPrefix </> Alice.TextFile.textFilePath

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
  isSuccess <- tests
  Control.Monad.unless isSuccess System.Exit.exitFailure
