module Markdown (module Markdown) where 
import Data.Text (strip, pack, unpack, stripStart)

data Prefix = Tab | BackTick | LeftArrow | Star | Minus | Plus | Hash | Space
    deriving (Show, Eq) 

data Line = Line [Prefix] String String
    deriving (Show, Eq) 

data Leaf = Heading Int String | HorizontalRule | BlankLine
    deriving (Show, Eq)

data Marker = Bullet Char | Number Int
    deriving (Show, Eq)

data Block = Leaf Leaf | Paragraph String | Code Bool [String] | Quote Blocks | ListItem Marker Block
    deriving (Show, Eq)

type Blocks = [Block]
newtype MDTree = Document Blocks
    deriving (Show, Eq)

parseMarkdown :: String -> MDTree
parseMarkdown content = Document blocks
    where 
        ls = map parseLine (lines content)
        blocks = parseBlocks [] ls

addPrefix :: Prefix -> Line -> Line
addPrefix prefix (Line ps s line) = Line (prefix:ps) s line

parseLine :: String -> Line
parseLine s
    | null s = Line [] [] []
    | head s == '\t' = addPrefix Tab (parseLine (tail s))
    | length line < length s - 4 = addPrefix Tab (parseLine (drop 4 s))
    | length line /= length s = addPrefix Space (parseLine line)
    | head line == '#' = addPrefix Hash (parseLine (tail line))
    | head line == '`' = addPrefix BackTick (parseLine (tail line))
    | head line == '>' = addPrefix LeftArrow (parseLine (tail line))
    | head line == '-' = addPrefix Minus (parseLine (tail line))
    | head line == '+' = addPrefix Plus (parseLine (tail line))
    | head line == '*' = addPrefix Star (parseLine (tail line))
    | otherwise = Line [] line s
    where 
        line = unpack (stripStart (pack s))

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

dropPrefix :: [Prefix] -> Prefix -> [Prefix]
dropPrefix [] _ = []
dropPrefix (p1:ps) p2 
    | p1 == p2 = dropPrefix ps p2
    | otherwise = p1:dropPrefix ps p2

lineToBlock :: Line -> Block
lineToBlock (Line _ [] _) = Leaf BlankLine
lineToBlock (Line (Space:ps) s line) = lineToBlock (Line ps s line)
lineToBlock (Line (Tab:_) _ line) = Code False [line]
lineToBlock (Line (Star:ps) s line) = ListItem (Bullet '*') (lineToBlock (Line ps s line))
lineToBlock (Line (Plus:ps) s line) = ListItem (Bullet '+') (lineToBlock (Line ps s line))
lineToBlock (Line (Minus:ps) s line) = ListItem (Bullet '-') (lineToBlock (Line ps s line))
lineToBlock (Line (LeftArrow:ps) s line) = Quote [lineToBlock (Line ps s line)]
lineToBlock (Line (BackTick:BackTick:BackTick:_) _ _) = Code True []
lineToBlock (Line (Hash:ps) s line)
    | isHeading (Hash:ps) = parseHeading (Hash:ps) s 
    | otherwise = lineToBlock (Line (dropPrefix ps Hash) s line)
lineToBlock (Line _ _ line) = Paragraph s 
    where
        text = pack line
        s = unpack (stripStart text)

countPrefix :: [Prefix] -> Prefix -> Int
countPrefix [] _ = 0 
countPrefix (p1:ps) p2 
    | p1 == p2 = countPrefix ps p2 + 1
    | otherwise = countPrefix ps p2

isHeading :: [Prefix] -> Bool 
isHeading ps = hashCount > 0 && hashCount <= 6
    where 
        hashCount = countPrefix ps Hash

parseHeading :: [Prefix] -> String -> Block
parseHeading ps s = Leaf (Heading n s)
    where 
        n = countPrefix ps Hash

mergeBlocks :: Block -> Line -> Blocks
mergeBlocks (Leaf l) line = [Leaf l, lineToBlock line]
mergeBlocks (ListItem m b) line = [ListItem m b, lineToBlock line]
mergeBlocks (Paragraph ls1) line = mergeParagraph ls1 line
mergeBlocks (Code False ls1) line = [Code False ls1, lineToBlock line]
mergeBlocks (Code True ls1) line = mergeCode ls1 line
mergeBlocks (Quote bs) line = mergeBlocks (last bs) line

mergeQuote :: Blocks -> Line -> Blocks
mergeQuote blocks (Line [] _ _) = blocks
mergeQuote blocks (Line (LeftArrow:ps) s line) = init blocks ++ newBlocks
    where 
        block = last blocks
        newBlocks = mergeBlocks block (Line ps s line)
mergeQuote blocks line = init blocks ++ newBlocks
    where 
        block = last blocks
        newBlocks = mergeBlocks block line 

mergeParagraph :: String -> Line -> Blocks
mergeParagraph para (Line [] s _) 
    | null s = [Paragraph para, Leaf BlankLine]
    | otherwise = [Paragraph (para ++ " " ++ s)]
mergeParagraph para line = [Paragraph para, lineToBlock line]

mergeCode :: [String] -> Line -> Blocks
mergeCode ls (Line [] s _) = [Code True (ls ++ [s])]
mergeCode ls (Line ps s line) 
    | take 3 ps == [BackTick, BackTick, BackTick] = [Code False ls, lineToBlock (Line (drop 3 ps) s line)]
    | otherwise = [Code True (ls ++ [line])]
