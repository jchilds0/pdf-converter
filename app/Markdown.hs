module Markdown (module Markdown) where 
import Debug.Trace (traceShow)

-- | markdown lexer
data Token = Tab | BackTick | LeftArrow | Star | UnderLine | Minus | Plus | Hash | Space | NewLine | Text String
    deriving (Show, Eq) 

tokeniseInput :: String -> [[Token]]
tokeniseInput contents = map tokeniseLine (lines contents)

tokeniseLine :: String -> [Token]
tokeniseLine "" = [Space]
tokeniseLine contents = tok:ts
    where
        (tok, rs) = nextToken contents
        ts = tokeniseLine rs 

nextToken :: String -> (Token, String)
nextToken "" = (Text "", "")
nextToken ('\t':str) = (Tab, str)
nextToken (' ':' ':' ':' ':str) = (Tab, str)
nextToken ('`':str) = (BackTick, str)
nextToken ('>':str) = (LeftArrow, str)
nextToken ('*':str) = (Star, str)
nextToken ('_':str) = (UnderLine, str)
nextToken ('-':str) = (Minus, str)
nextToken ('+':str) = (Plus, str)
nextToken ('#':str) = (Hash, str)
nextToken (' ':str) = (Space, str)
nextToken ('\n':str) = (NewLine, str)
nextToken (c:str) = case nextToken str of 
    (Text ts, "") -> (Text (c:ts), "")
    (Text ts, rs) -> (Text (c:ts), rs)
    (_, _) -> (Text (c:""), str)

tokenToStr :: Token -> String
tokenToStr Tab = "\t"
tokenToStr BackTick = "`"
tokenToStr LeftArrow = ">"
tokenToStr Star = "*"
tokenToStr UnderLine = "_"
tokenToStr Minus = "-"
tokenToStr Plus = "+"
tokenToStr Hash = "#"
tokenToStr Space = " "
tokenToStr NewLine = "\n"
tokenToStr (Text text) = text

-- | markdown parser
data Inline = Plain String | Code String | Strong String | Emphasis String
    deriving (Show, Eq) 

data Leaf = Heading Int [Inline] | HorizontalRule | BlankLine
    deriving (Show, Eq)

data Marker = Bullet Char | Number Int
    deriving (Show, Eq)

data Block = Leaf Leaf | Paragraph [Inline] | IndentCode Bool [String] | FencedCode Bool [String] | Quote [Block] | ListItem Marker Block
    deriving (Show, Eq)

newtype MDTree = Document [Block]
    deriving (Show, Eq)

parseMarkdown :: String -> MDTree
parseMarkdown contents = Document (parseBlocks [] (tokeniseInput contents)) 

printArray :: Show a => [a] -> String
printArray ls = "[\n" ++ strs ++ "]"
    where 
        strs = foldr (\b s -> "\t" ++ show b ++ ",\n" ++ s) "" ls

dropTokens :: Token -> [Token] -> [Token]
dropTokens _ [] = []
dropTokens tok (t:ts)
    | tok == t = dropTokens tok ts
    | otherwise = t:ts

rightFlankingDelimiter :: Token -> Int -> [Token] -> Bool
rightFlankingDelimiter _ 0 _ = True 
rightFlankingDelimiter _ _ [] = False
rightFlankingDelimiter delim count (Space:ts) = rightFlankingDelimiter delim count tokens
    where 
        tokens = dropTokens delim ts
rightFlankingDelimiter delim count (tok:ts) = delimited || rightFlankingDelimiter delim count ts
    where 
        delimited = (tok == delim) && rightFlankingDelimiter delim (count - 1) ts

data Delimiter = LeftFlank | RightFlank | Both 
    deriving (Show, Eq)

data Text = Delim Delimiter Int Token | Str Token
    deriving (Show, Eq)

-- | tokensToText parses a string of tokens into an intermediate form 
--   which contains information about delimiters, length, character and 
--   flanking
tokensToText :: Token -> [Token] -> [Text]
tokensToText _ [] = []
tokensToText prev (t:ts) = case t of 
    Star -> delim:text
    UnderLine -> delim:text
    BackTick -> delim:text
    _ -> Str t:tokensToText t ts
    where 
        (delim, newTokens) = delimiter prev (t:ts)
        text = tokensToText t newTokens

countDelim :: Token -> [Token] -> Int
countDelim _ [] = 0
countDelim t1 (t2:ts) 
    | t1 == t2 = countDelim t1 ts + 1
    | otherwise = 0

delimiter :: Token -> [Token] -> (Text, [Token])
delimiter _ [] = (Str (Text ""), [])
delimiter prev (t:ts) = case prev of 
    Space -> case newTokens of 
        (Space:_) -> (Str t, ts)
        _ -> (Delim LeftFlank count t, newTokens)
    _ -> case newTokens of 
        (Space:_) -> (Delim RightFlank count t, newTokens)
        _ -> (Delim Both count t, newTokens)
    where 
        count = countDelim t (t : ts)
        newTokens = drop count (t : ts)

-- | parseInline transforms the intermediate representation
--   from tokensToText into Inline's
parseInline :: [Token] -> [Inline]
parseInline tokens = collapseInline (parseText text)
    where 
        text = tokensToText NewLine tokens

parseText :: [Text] -> [Inline]
parseText [] = []
parseText (t:text) = case t of 
    Delim {} -> processDelim t text
    Str tok -> Plain (tokenToStr tok) : parseText text

processDelim :: Text -> [Text] -> [Inline]
processDelim delim text = case delim of 
    Str tok -> Plain (tokenToStr tok) : parseText text
    Delim RightFlank _ _ -> parseText (textDelim : text)
        where
            textDelim = Str (Text (textToString delim))
    Delim _ count char -> parseMatchingDelim delim delimText remText
        where 
            (delimText, remText) = findMatchingDelim char count text

textToString :: Text -> String
textToString (Delim _ count char) = concat (replicate count (tokenToStr char))
textToString (Str text) = tokenToStr text

findMatchingDelim :: Token -> Int -> [Text] -> ([Text], [Text])
findMatchingDelim _ _ [] = ([], [])
findMatchingDelim c1 count (delim : text) = case delim of 
    Str _ -> retval
    Delim LeftFlank _ _ -> retval
    Delim _ _ c2 -> if c1 == c2 then ([delim], text) else retval
    where 
        (delimText, remText) = findMatchingDelim c1 count text
        retval = if null delimText then ([], delim : text) else (delim : delimText, remText)

parseMatchingDelim :: Text -> [Text] -> [Text] -> [Inline]
parseMatchingDelim delimStart [] text = parseText text
parseMatchingDelim delimStart delimText text = case prev of 
    Just i2 -> i2 : i1 : inlines
    Nothing -> i1 : inlines
    where 
        inlineText = init delimText
        delimEnd = last delimText 

        (prev, i1, next) = formatInline delimStart inlineText delimEnd
        inlines = parseText (next ++ text)

formatInline :: Text -> [Text] -> Text -> (Maybe Inline, Inline, [Text])
formatInline (Delim type1 count1 char1) text (Delim type2 count2 char2) 
    | char1 == char2 = case compare count1 count2 of 
        LT -> (Nothing, inlineBlock char1 count1 text, [Delim type2 (count2 - count1) char2])
        EQ -> (Nothing, inlineBlock char1 count1 text, [])
        GT -> (Just (Plain (textToString newDelim)), inlineBlock char1 count2 text, [])
            where 
                newDelim = Delim type1 (count1 - count2) char1
    | otherwise = (Nothing, Plain (concatMap textToString text), [])
formatInline _ text _ = (Nothing, Plain (concatMap textToString text), [])

inlineBlock :: Token -> Int -> [Text] -> Inline
inlineBlock char 1 text = case char of 
    UnderLine -> Emphasis string
    Star -> Emphasis string
    BackTick -> Code string
    _ -> Plain string
    where 
        string = concatMap textToString text
inlineBlock char _ text = case char of 
    UnderLine -> Strong string
    Star -> Strong string
    BackTick -> Code string
    _ -> Plain string
    where 
        string = concatMap textToString text

collapseInline :: [Inline] -> [Inline]
collapseInline [] = []
collapseInline [i] = [i]
collapseInline (Plain t1 : Plain t2 : inlines) = collapseInline (Plain (t1 ++ t2) : inlines)
collapseInline (i1 : inlines) = i1 : collapseInline inlines

-- | parseBlocks takes the current blocks in the document and 
--   the remaining tokens, split by newline, and either adds 
--   the next line to the last block or creates a new block.
parseBlocks :: [Block] -> [[Token]] -> [Block] 
parseBlocks blocks [] = blocks
parseBlocks blocks (l:ls) = parseBlocks newBlocks ls
    where 
        newBlocks = addLine blocks l

addLine :: [Block] -> [Token] -> [Block] 
addLine [] l = [lineToBlock l]
addLine blocks l = init blocks ++ mergeBlocks (last blocks) l

countPrefix :: Token -> [Token] -> Int
countPrefix t1 (t2:tokens) 
    | t1 == t2 = countPrefix t1 tokens + 1
    | otherwise = countPrefix t1 tokens
countPrefix _ _ = 0 

headingInfo :: [Token] -> Int
headingInfo (Hash:tokens) = count + 1
    where 
        count = headingInfo tokens
headingInfo _ = 0 

lineToBlock :: [Token] -> Block
lineToBlock (Space:ts) = lineToBlock ts
lineToBlock (Star:ts) = ListItem (Bullet '*') (lineToBlock ts)
lineToBlock (Plus:ts) = ListItem (Bullet '+') (lineToBlock ts)
lineToBlock (Minus:ts) = ListItem (Bullet '-') (lineToBlock ts)
lineToBlock (LeftArrow:ts) = Quote [lineToBlock ts]
lineToBlock (Tab:ts) = IndentCode True [concatMap tokenToStr ts]
lineToBlock (BackTick:BackTick:BackTick:_) = FencedCode True []
lineToBlock (Hash:ts)
    | hashCount <= 6 = Leaf (Heading hashCount (parseInline (Hash:ts)))
    | otherwise = Paragraph (parseInline (Hash:ts))
    where 
        hashCount = headingInfo (Hash:ts)
lineToBlock tokens = Paragraph (parseInline tokens) 

mergeBlocks :: Block -> [Token] -> [Block]
mergeBlocks (Leaf l) line = [Leaf l, lineToBlock line]
mergeBlocks (ListItem m b) line = [ListItem m b, lineToBlock line]
mergeBlocks (Paragraph ls1) line = mergeParagraph ls1 (lineToBlock line)
mergeBlocks (Quote bs) line = mergeQuote bs line
mergeBlocks (IndentCode False ls1) line = [IndentCode False ls1, lineToBlock line]
mergeBlocks (IndentCode True ls1) line = case line of 
    (Tab:ts) -> [IndentCode True (ls1 ++ [concatMap tokenToStr ts])]
    _ -> [IndentCode False ls1, lineToBlock line]
mergeBlocks (FencedCode False ls1) line = [FencedCode False ls1, lineToBlock line]
mergeBlocks (FencedCode True ls1) line = case lineToBlock line of 
    FencedCode _ _ -> [FencedCode False ls1]
    _ -> [FencedCode True (ls1 ++ [concatMap tokenToStr line])]

mergeQuote :: [Block] -> [Token] -> [Block]
mergeQuote blocks (LeftArrow:ts) = [mergeQuoteBlock blocks ts]
mergeQuote blocks (Space:LeftArrow:ts) = [mergeQuoteBlock blocks ts]
mergeQuote blocks line = case lineToBlock line of 
    Paragraph _ -> case last blocks of 
        Paragraph _ -> [mergeQuoteBlock blocks line]
        _ -> [Quote blocks, lineToBlock line]
    _ -> [Quote blocks, lineToBlock line]

mergeQuoteBlock :: [Block] -> [Token] -> Block
mergeQuoteBlock blocks nextLine = Quote (init blocks ++ newBlocks)
    where
        block = last blocks
        newBlocks = mergeBlocks block nextLine

mergeParagraph :: [Inline] -> Block -> [Block] 
mergeParagraph p1 (Paragraph p2) = [Paragraph (p1 ++ p2)]
mergeParagraph para block = [Paragraph para, block]

