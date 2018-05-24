module Alice where

import qualified Control.Lens as Lens
import qualified Pdf.Toolbox.Document as Pdf.Document
import qualified System.IO as IO

pdfPath :: FilePath
pdfPath = "AliceInWonderland.pdf"

getPageCount :: FilePath -> IO (Either String Int)
getPageCount path = do
  ePageCount <- IO.withBinaryFile path IO.ReadMode $ \handle ->
    Pdf.Document.runPdfWithHandle handle Pdf.Document.knownFilters $ do
      pdf <- Pdf.Document.document
      catalog <- Pdf.Document.documentCatalog pdf
      rootNode <- Pdf.Document.catalogPageNode catalog
      Pdf.Document.pageNodeNKids rootNode
  return (Lens.over Lens._Left show ePageCount)

main :: IO ()
main = do
  ePageCount <- getPageCount pdfPath
  case ePageCount of
    Left err -> putStrLn err
    Right pageCount -> putStrLn $ show pageCount ++ " pages in " ++ pdfPath
