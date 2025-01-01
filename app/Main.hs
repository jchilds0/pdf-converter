module Main (module Main) where
import System.Environment (getArgs)
import Data.Text (stripSuffix, pack, unpack)
import Markdown (parseMarkdown)
import Text (markdownToPDF)
import PDF (generatePDF)

main :: IO ()
main = do 
    args <- getArgs
    let groupArg = groupArgs args
    parseArgs groupArg

flag :: String -> Bool
flag "" = False
flag ('-':_) = True 
flag _ = False

collectArgs :: [String] -> [String] -> ([String], [String])
collectArgs acc [] = (acc, [])
collectArgs acc (a:as) = if flag a then (acc, a:as) else collectArgs (a:acc) as

groupArgs :: [String] -> [[String]]
groupArgs [] = []
groupArgs (x:xs) = (x:as):groupArgs rs
    where
        (as, rs) = collectArgs [] xs 

getArg :: String -> [[String]] -> [String]
getArg _ [] = []
getArg name ([]:args) = getArg name args
getArg name ((a:as):args) = if a == "-" ++ name || a == "--" ++ name then as else getArg name args

formatArgs :: [String] -> [String] -> [(String, String)]
formatArgs [] _ = []
formatArgs (i:is) [] = (i, o):formatArgs is []
    where
        format = pack ".md"
        inputName = pack i
        o = case stripSuffix format inputName of 
            Just fileName -> unpack fileName ++ ".pdf"
            Nothing -> i ++ ".pdf"
formatArgs (i:is) (o:os) = (i, o):formatArgs is os

parseArgs :: [[String]] -> IO ()
parseArgs [] = putStrLn "No input files"
parseArgs args = parseFiles (formatArgs input output)
    where
        inputArgs = getArg "input" args
        input = if null inputArgs then head args else inputArgs
        output = getArg "output" args

parseFiles :: [(String, String)] -> IO ()
parseFiles [] = putStrLn "Parsed all files"
parseFiles ((input, output):items) = do
    parseFile input output
    parseFiles items

parseFile :: FilePath -> FilePath -> IO ()
parseFile input output = do
    putStrLn $ "Converting " ++ input ++ " to " ++ output
    contents <- readFile input
    pdfTree <- markdownToPDF (parseMarkdown contents)
    let pdfContents = generatePDF pdfTree
    writeFile output pdfContents

printArray :: Show a => [a] -> String
printArray ls = "[\n" ++ strs ++ "]"
    where 
        strs = foldr (\b s -> "\t" ++ show b ++ ",\n" ++ s) "" ls
