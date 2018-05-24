module Alice where

import qualified Pdf.Toolbox.Document as Pdf.Document
import qualified System.IO as IO

main :: IO ()
main = do
  let documentPath = "AliceInWonderland.pdf"
  ePageCount <- IO.withBinaryFile documentPath IO.ReadMode $ \handle ->
    Pdf.Document.runPdfWithHandle handle Pdf.Document.knownFilters $ do
      pdf <- Pdf.Document.document
      catalog <- Pdf.Document.documentCatalog pdf
      rootNode <- Pdf.Document.catalogPageNode catalog
      Pdf.Document.pageNodeNKids rootNode
  case ePageCount of
    Left err -> print err
    Right pageCount -> putStrLn $ show pageCount ++ " pages in " ++ documentPath
