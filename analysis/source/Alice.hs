module Alice where

import qualified Control.Monad.IO.Class as Monad.IO
import qualified Pdf.Toolbox.Document as Pdf.Document
import qualified System.IO as IO

pdfPath :: FilePath
pdfPath = "AliceInWonderland.pdf"

run :: FilePath -> IO ()
run path = do
  result <- IO.withBinaryFile path IO.ReadMode $ \handle ->
    Pdf.Document.runPdfWithHandle handle Pdf.Document.knownFilters $ do
      pdf <- Pdf.Document.document
      catalog <- Pdf.Document.documentCatalog pdf
      rootNode <- Pdf.Document.catalogPageNode catalog
      childNodes <- Pdf.Document.pageNodeKids rootNode
      Monad.IO.liftIO $ mapM_ print childNodes
      return ()
  case result of
    Left err -> error $ show err
    Right x -> return x

main :: IO ()
main = run pdfPath
