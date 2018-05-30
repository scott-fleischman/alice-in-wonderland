{-# LANGUAGE TemplateHaskell #-}

import qualified Alice.Pdf
import qualified Alice.Sentence
import qualified Alice.TextFile
import qualified Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text
import           Hedgehog ((===))
import qualified Hedgehog
import qualified System.Exit
import           System.FilePath ((</>))

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

prop_reduceToSizeFromRight :: Hedgehog.Property
prop_reduceToSizeFromRight =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    let five = Seq.fromList [1,2,3,4,5] :: Seq Int
    Seq.fromList [1,2,3,4,5] === Alice.Sentence.reduceToSizeFromRight 6 five
    Seq.fromList [1,2,3,4,5] === Alice.Sentence.reduceToSizeFromRight 5 five
    Seq.fromList [1,2,3,4] === Alice.Sentence.reduceToSizeFromRight 4 five
    Seq.fromList [1,2,3] === Alice.Sentence.reduceToSizeFromRight 3 five
    Seq.fromList [1,2] === Alice.Sentence.reduceToSizeFromRight 2 five
    Seq.singleton 1 === Alice.Sentence.reduceToSizeFromRight 1 five
    Seq.empty === Alice.Sentence.reduceToSizeFromRight 0 five

prop_reduceToSizeFromLeft :: Hedgehog.Property
prop_reduceToSizeFromLeft =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    let five = Seq.fromList [1,2,3,4,5] :: Seq Int
    Seq.fromList [1,2,3,4,5] === Alice.Sentence.reduceToSizeFromLeft 6 five
    Seq.fromList [1,2,3,4,5] === Alice.Sentence.reduceToSizeFromLeft 5 five
    Seq.fromList [2,3,4,5] === Alice.Sentence.reduceToSizeFromLeft 4 five
    Seq.fromList [3,4,5] === Alice.Sentence.reduceToSizeFromLeft 3 five
    Seq.fromList [4,5] === Alice.Sentence.reduceToSizeFromLeft 2 five
    Seq.singleton 5 === Alice.Sentence.reduceToSizeFromLeft 1 five
    Seq.empty === Alice.Sentence.reduceToSizeFromLeft 0 five

tests :: IO Bool
tests =
  Hedgehog.checkSequential $$(Hedgehog.discover)

main :: IO ()
main = do
  isSuccess <- tests
  Control.Monad.unless isSuccess System.Exit.exitFailure
