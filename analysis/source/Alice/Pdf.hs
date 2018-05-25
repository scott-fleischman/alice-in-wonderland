module Alice.Pdf where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Pdf.Toolbox.Document as Pdf.Document
import qualified Pdf.Toolbox.Document.Page as Pdf.Page
import qualified Pdf.Toolbox.Document.PageNode as Pdf.PageNode
import qualified System.IO as IO

pdfPath :: FilePath
pdfPath = "AliceInWonderland.pdf"

getPdfText :: FilePath -> IO Text
getPdfText path = do
  result <- IO.withBinaryFile path IO.ReadMode $ \handle ->
    Pdf.Document.runPdfWithHandle handle Pdf.Document.knownFilters $ do
      pdf <- Pdf.Document.document
      catalog <- Pdf.Document.documentCatalog pdf
      rootNode <- Pdf.Document.catalogPageNode catalog
      childNodes <- Pdf.Document.pageNodeKids rootNode
      fmap Text.concat $ mapM loadPageText childNodes
  case result of
    Left err -> error $ show err
    Right x -> return x
  where
  loadPageText pageNodeRef = do
    pageNode <- Pdf.PageNode.loadPageNode pageNodeRef
    case pageNode of
      Pdf.PageNode.PageTreeNode _ -> return Text.empty
      Pdf.PageNode.PageTreeLeaf page -> Pdf.Page.pageExtractText page
