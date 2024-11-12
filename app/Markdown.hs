module Markdown (module Markdown) where 
import PDF (PDFTree (PDFTree), documentCatalog)

data MDTree = MDTree String

parseMarkdown :: String -> MDTree
parseMarkdown contents = MDTree contents

-- markdownToPDF :: MDTree -> PDFTree
-- markdownToPDF (MDTree text) = documentCatalog 
