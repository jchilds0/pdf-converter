module Markdown (module Markdown) where 
import Data.Text (pack, unpack, stripStart, strip)

data Prefix = Tab | BackTick | LeftArrow | Star | Minus | Plus | Hash | Space
    deriving (Show, Eq) 

data Inline = Text String | Code String | Strong String | Emphasis String
    deriving (Show, Eq) 

data Line = Line Prefix Line [Inline] | Blank [Inline]
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
        ls = map parseLinePrefix (lines content)
        blocks = parseBlocks [] ls

printArray :: Show a => [a] -> String
printArray ls = "[\n" ++ strs ++ "]"
    where 
        strs = foldr (\b s -> "\t" ++ show b ++ ",\n" ++ s) "" ls

parseLinePrefix :: String -> Line
parseLinePrefix s
    | null s = Blank inline
    | head s == '\t' = Line Tab (parseLinePrefix (tail s)) inline
    | length line <= length s - 4 = Line Tab (parseLinePrefix (drop 4 s)) inline
    | length line /= length s = Line Space (parseLinePrefix line) inline 
    | head line == '#' = Line Hash (parseLinePrefix (tail line)) inline
    | head line == '`' = Line BackTick (parseLinePrefix (tail line)) inline
    | head line == '>' = Line LeftArrow (parseLinePrefix removeSpace) inline 
    | head line == '-' = Line Minus (parseLinePrefix (tail line)) inline
    | head line == '+' = Line Plus (parseLinePrefix (tail line)) inline
    | head line == '*' = Line Star (parseLinePrefix (tail line)) inline
    | otherwise = Blank inline
    where 
        line = unpack (stripStart (pack s))
        inline = parseLineInline s
        removeSpace 
            | null (tail line) = "" 
            | head (tail line) == ' ' = tail (tail line)
            | otherwise = tail line

parseLineInline :: String -> [Inline]
parseLineInline s = [Text s]

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

headingInfo :: Line -> ([Inline], Int)
headingInfo (Line Hash line _) = (heading, count + 1)
    where 
        (heading, count) = headingInfo line
headingInfo (Line _ _ inline) = (inline, 0) 
headingInfo (Blank inline) = (inline, 0) 

lineToBlock :: Line -> Block
lineToBlock (Blank s) = if null s then Leaf BlankLine else Paragraph s
lineToBlock (Line Space line _) = lineToBlock line
lineToBlock (Line Minus (Line Minus (Line Minus _ _) _) _) = Leaf HorizontalRule
lineToBlock (Line Star line _) = ListItem (Bullet '*') (lineToBlock line)
lineToBlock (Line Plus line _) = ListItem (Bullet '+') (lineToBlock line)
lineToBlock (Line Minus line _) = ListItem (Bullet '-') (lineToBlock line)
lineToBlock (Line LeftArrow line _) = Quote [lineToBlock line]
lineToBlock (Line Tab line _) = IndentCode True [line]
lineToBlock (Line BackTick (Line BackTick (Line BackTick _ _) _) _) = FencedCode True []
lineToBlock (Line Hash line inline)
    | hashCount <= 6 = Leaf (Heading hashCount (headingInfo line))
    | otherwise = Paragraph inline
    where 
        (inline, hashCount) = headingInfo (Line Hash line inline)
lineToBlock (Line _ _ inline) = Paragraph inline 

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

