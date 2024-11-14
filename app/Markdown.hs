module Markdown (module Markdown) where 
import Data.Text (strip, pack, unpack, stripStart)

data Leaf = Heading Int String | HorizontalRule | BlankLine
    deriving (Show, Eq)

data Marker = Bullet Char | Number Int
    deriving (Show, Eq)

data Block = Leaf Leaf | Paragraph [String] | Code Bool [String] | Quote Blocks | ListItem Marker Block
    deriving (Show, Eq)

type Blocks = [Block]
newtype MDTree = Document Blocks
    deriving (Show, Eq)

parseMarkdown :: String -> MDTree
parseMarkdown content = tree
    where 
        ls = lines content
        ls2 = map parseLine ls
        tree = parseBlocks (Document []) ls2

parseBlocks :: MDTree -> [Block] -> MDTree
parseBlocks tree [] = tree
parseBlocks tree (l:ls) = parseBlocks newTree ls
    where 
        newTree = addLine tree l

addLine :: MDTree -> Block -> MDTree
addLine (Document []) l = Document [l]
addLine (Document blocks) block = Document newBlocks
    where 
        lastBlock = last blocks
        newBlocks = init blocks ++ mergeBlocks lastBlock block

mergeBlocks :: Block -> Block -> Blocks
mergeBlocks (Leaf l) next = [Leaf l, next]
mergeBlocks (ListItem m b) next = [ListItem m b, next]
mergeBlocks (Paragraph ls1) next = case next of 
    Paragraph ls2 -> [Paragraph (ls1 ++ ls2)]
    _ -> [Paragraph ls1, next]
mergeBlocks (Code False ls1) next = [Code False ls1, next]
mergeBlocks (Code True ls1) next = case next of 
    Paragraph ls2 -> [Code True (ls1 ++ ls2)]
    Code _ ls2 -> [Code False (ls1 ++ ls2)]
    _ -> [Code False ls1, next]
mergeBlocks (Quote bs1) next = case next of 
    Quote bs2 -> [Quote blocks]
        where 
            newBlocks = mergeBlocks (last bs1) (last bs2)
            blocks = init bs1 ++ newBlocks
    Leaf _ -> [Quote bs1, next]
    _ -> [Quote blocks]
        where 
            newBlocks = mergeBlocks (last bs1) next
            blocks = init bs1 ++ newBlocks

parseLine :: String -> Block
parseLine s
    | null line = Leaf BlankLine
    | isHeading line = parseHeading line
    | take 3 line == "```" = Code True [drop 3 line]
    | take 3 line == "---" = Leaf HorizontalRule
    | head line == '>' = Quote [parseLine (tail line)]
    | head line == '-' = ListItem (Bullet '-') (parseLine (tail line))
    | head line == '*' = ListItem (Bullet '*') (parseLine (tail line))
    | head line == '+' = ListItem (Bullet '+') (parseLine (tail line))
    | otherwise = Paragraph [line]
    where 
        line = unpack (stripStart (pack s))

isHeading :: String -> Bool
isHeading s = (head s == '#') && 0 < n && n <= 6
    where 
        n = countHash s 

countHash :: String -> Int
countHash ('#':s) = countHash s + 1
countHash _ = 0 

parseHeading :: String -> Block 
parseHeading s = Leaf (Heading n str)
    where 
        n = countHash s
        text = pack (drop n s)
        str = unpack (strip text)
