module Markdown (module Markdown) where 
import Data.Text (strip, pack, unpack, stripStart)

data Leaf = Heading Int String | HorizontalRule | BlankLine
    deriving (Show, Eq)

data Marker = Bullet Char | Number Int
    deriving (Show, Eq)

data Block = Leaf Leaf | Paragraph [String] | Code [String] | Quote Blocks | ListItem Marker Block
    deriving (Show, Eq)

type Blocks = [Block]
newtype MDTree = Document Blocks
    deriving (Show, Eq)

parseMarkdown :: String -> MDTree
parseMarkdown content = parseLines (Document []) ls2
    where 
        ls = lines content
        ls2 = map parseLine ls

parseLines :: MDTree -> [Block] -> MDTree
parseLines tree [] = tree
parseLines tree (l:ls) = parseLines newTree ls
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
mergeBlocks (Code ls1) next = case next of 
    Code ls2 -> [Code (ls1 ++ ls2)]
    _ -> [Code ls1, next]
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
    | take 3 line == "```" = Code [drop 3 line]
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
        text = pack (take n s)
        str = unpack (strip text)
