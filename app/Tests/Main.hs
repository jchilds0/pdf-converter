module Main(main) where
import Markdown
import Test.HUnit
import qualified System.Exit as Exit

main :: IO ()
main = do 
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

testMD = "> Lorem ipsum dolor\nsit amet.\n> - Qui *quodsi iracundia*\n> - aliquando id"
testMDTree = Document [
        Quote [
                Paragraph ["Lorem ipsum dolor", "sit amet."],
                Markdown.ListItem (Bullet '-') (Paragraph ["Qui *quodsi iracundia*"]),
                Markdown.ListItem (Bullet '-') (Paragraph ["aliquando id"])
            ]
    ]

test1 = TestCase (assertEqual "mdtree" testMDTree (parseMarkdown testMD))

tests = TestList [
        TestLabel "test1" test1
    ]
