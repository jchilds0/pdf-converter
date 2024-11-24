module Markdown (module Markdown) where 
import Data.Text (pack, unpack, stripStart, strip)
import Debug.Trace (trace)

data Prefix = Tab | BackTick | LeftArrow | Star | Minus | Plus | Hash | Space
    deriving (Show, Eq) 

data Line = Line Prefix Line String | Blank String
    deriving (Show, Eq) 

data Leaf = Heading Int String | HorizontalRule | BlankLine
    deriving (Show, Eq)

data Marker = Bullet Char | Number Int
    deriving (Show, Eq)

data Block = Leaf Leaf | Paragraph String | IndentCode Bool [String] | FencedCode Bool [String] | Quote Blocks | ListItem Marker Block
    deriving (Show, Eq)

type Blocks = [Block]
newtype MDTree = Document Blocks
    deriving (Show, Eq)

parseMarkdown :: String -> MDTree
parseMarkdown content = Document blocks
    where 
        ls = map parseLine (lines content)
        blocks = parseBlocks [] ls

printArray :: Show a => [a] -> String
printArray ls = "[\n" ++ strs ++ "]"
    where 
        strs = foldr (\b s -> "\t" ++ show b ++ ",\n" ++ s) "" ls

parseLine :: String -> Line
parseLine s
    | null s = Blank s
    | head s == '\t' = Line Tab (parseLine (tail s)) s
    | length line <= length s - 4 = Line Tab (parseLine (drop 4 s)) s
    | length line /= length s = Line Space (parseLine line) s
    | head line == '#' = Line Hash (parseLine (tail line)) s
    | head line == '`' = Line BackTick (parseLine (tail line)) s
    | head line == '>' = Line LeftArrow (parseLine removeSpace) s
    | head line == '-' = Line Minus (parseLine (tail line)) s
    | head line == '+' = Line Plus (parseLine (tail line)) s
    | head line == '*' = Line Star (parseLine (tail line)) s
    | otherwise = Blank s
    where 
        line = unpack (stripStart (pack s))
        removeSpace 
            | null (tail line) = "" 
            | head (tail line) == ' ' = tail (tail line)
            | otherwise = tail line

parseBlocks :: Blocks -> [Line] -> Blocks 
parseBlocks blocks [] = blocks
parseBlocks blocks (l:ls) = parseBlocks newBlocks ls
    where 
        newBlocks = addLine blocks l

addLine :: Blocks -> Line -> Blocks 
addLine [] l = [lineToBlock l]
addLine blocks block = init blocks ++ mergeBlocks lastBlock block
    where 
        lastBlock = last blocks

countPrefix :: Prefix -> Line -> Int
countPrefix p1 (Line p2 line _) 
    | p1 == p2 = countPrefix p1 line + 1
    | otherwise = countPrefix p1 line
countPrefix _ _ = 0 

lineText :: Line -> String
lineText (Blank text) = text
lineText (Line _ _ text) = text

headingText :: Line -> String
headingText (Line Hash line _) = headingText line
headingText line = unpack (strip (pack text))
    where 
        text = lineText line

lineToBlock :: Line -> Block
lineToBlock (Blank s) = if null s then Leaf BlankLine else Paragraph s
lineToBlock (Line Space line _) = lineToBlock line
lineToBlock (Line Star line _) = ListItem (Bullet '*') (lineToBlock line)
lineToBlock (Line Plus line _) = ListItem (Bullet '+') (lineToBlock line)
lineToBlock (Line Minus line _) = ListItem (Bullet '-') (lineToBlock line)
lineToBlock (Line LeftArrow line _) = Quote [lineToBlock line]
lineToBlock (Line Tab line _) = IndentCode True [lineText line]
lineToBlock (Line BackTick (Line BackTick (Line BackTick _ _) _) _) = FencedCode True []
lineToBlock (Line Hash line s)
    | hashCount <= 6 = Leaf (Heading hashCount (headingText line))
    | otherwise = Paragraph s
    where 
        hashCount = countPrefix Hash (Line Hash line s)
lineToBlock (Line _ _ s) = Paragraph s 

mergeBlocks :: Block -> Line -> Blocks
mergeBlocks (Leaf l) line = [Leaf l, lineToBlock line]
mergeBlocks (ListItem m b) line = [ListItem m b, lineToBlock line]
mergeBlocks (Paragraph ls1) line = mergeParagraph ls1 line
mergeBlocks (IndentCode False ls1) line = [IndentCode False ls1, lineToBlock line]
mergeBlocks (FencedCode False ls1) line = [FencedCode False ls1, lineToBlock line]
mergeBlocks (IndentCode True ls1) line = case line of 
    Line Tab (Line _ _ text) _ -> [IndentCode True (ls1 ++ [text])]
    _ -> [IndentCode False ls1, lineToBlock line]
mergeBlocks (FencedCode True ls1) line = case lineToBlock line of 
    FencedCode _ _ -> [FencedCode False ls1]
    _ -> [FencedCode True (ls1 ++ [lineText line])]
mergeBlocks (Quote bs) line = mergeQuote bs line

mergeQuote :: Blocks -> Line -> Blocks
mergeQuote blocks (Line LeftArrow nextLine _) = [mergeQuoteBlock blocks nextLine]
mergeQuote blocks (Line Space (Line LeftArrow nextLine _) _) = [mergeQuoteBlock blocks nextLine]
mergeQuote blocks line = case lineToBlock line of 
    Paragraph _ -> case last blocks of 
        Paragraph _ -> [mergeQuoteBlock blocks line]
        _ -> [Quote blocks, lineToBlock line]
    _ -> [Quote blocks, lineToBlock line]

mergeQuoteBlock :: Blocks -> Line -> Block
mergeQuoteBlock blocks nextLine = Quote (init blocks ++ newBlocks)
    where
        block = last blocks
        newBlocks = mergeBlocks block nextLine

mergeParagraph :: String -> Line -> Blocks
mergeParagraph para (Blank s) 
    | null s = [Paragraph para, Leaf BlankLine]
    | otherwise = [Paragraph (para ++ " " ++ s)]
mergeParagraph para line = [Paragraph para, lineToBlock line]

