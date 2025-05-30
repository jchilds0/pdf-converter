{-# LANGUAGE GADTs #-}

module Main(main) where
import Markdown
import Test.HUnit
import qualified System.Exit as Exit
import PDF
import Text (markdownToPDF, resources, paragraphFontAttr, emphasisFontAttr)
import GHC.IO (unsafePerformIO)
import Text.Pretty.Simple (pShow)

data Wrapper a where 
    Wrap :: (Show a, Eq a) => a -> Wrapper a

instance Show (Wrapper a) where 
    show (Wrap a) = show (pShow a)

instance Eq (Wrapper a) where 
    Wrap a == Wrap b = a == b

main :: IO ()
main = do 
    result <- runTestTT tests
    if failures result > 0 || errors result > 0 then Exit.exitFailure else Exit.exitSuccess

testMD = "> Lorem ipsum dolor\nsit amet.\n> - Qui *quodsi iracundia*\n> - aliquando id"
testMDTree1 = Document [
        Quote [
                Paragraph [Inlines Plain "Lorem ipsum dolor ", Inlines Plain "sit amet. "],
                Markdown.ListItem (Bullet '-') (Paragraph [Inlines Plain "Qui ", Inlines Emphasis "quodsi iracundia", Inlines Plain " "]),
                Markdown.ListItem (Bullet '-') (Paragraph [Inlines Plain "aliquando id "])
            ]
    ]

test1 = TestCase (assertEqual "mdtree" testMDTree1 (parseMarkdown testMD))

testMDtoPDF = "Lorem ipsum *hello world*"
testMDTree2 = Document [
        Paragraph [Inlines Plain "Lorem ipsum ", Inlines Emphasis "hello world", Inlines Plain " "]
    ]
testPDFTree = pdfCreateCatalog (pdfCreatePageTree [
            pdfCreatePage [
                pdfCreateTextObject (PDF.Text "Lorem ipsum" paragraphFontAttr 1.0 (Point 72 730)),
                pdfCreateTextObject (PDF.Text "hello world" emphasisFontAttr 1.0 (Point 298 730))
            ] resources
        ]
    )

test2 = TestCase (assertEqual "mdtree" testMDTree2 (parseMarkdown testMDtoPDF))
test3 = TestCase (assertEqual "pdftree" testPDFTree (unsafePerformIO (markdownToPDF testMDTree2)))

tests = TestList [
        TestLabel "parseMarkdown" test1,
        TestLabel "parseMarkdown" test2,
        TestLabel "markdownToPDF" test3
    ]
