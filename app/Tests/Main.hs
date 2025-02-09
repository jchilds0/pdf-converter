module Main(main) where
import Markdown
import Test.HUnit
import qualified System.Exit as Exit

main :: IO ()
main = do 
    result <- runTestTT tests
    if failures result > 0 || errors result > 0 then Exit.exitFailure else Exit.exitSuccess

testMD = "> Lorem ipsum dolor\nsit amet.\n> - Qui *quodsi iracundia*\n> - aliquando id"
testMDTree = Document [
        Quote [
                Paragraph [Plain "Lorem ipsum dolor ", Plain "sit amet. "],
                Markdown.ListItem (Bullet '-') (Paragraph [Plain "Qui ", Emphasis "quodsi iracundia", Plain " "]),
                Markdown.ListItem (Bullet '-') (Paragraph [Plain "aliquando id "])
            ]
    ]

test1 = TestCase (assertEqual "mdtree" testMDTree (parseMarkdown testMD))

tests = TestList [
        TestLabel "test1" test1
    ]
