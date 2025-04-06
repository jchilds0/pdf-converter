module Text (module Text) where
import Markdown (MDTree (Document), Leaf (Heading, HorizontalRule), Block (Leaf, Paragraph, Quote, IndentCode, FencedCode, ListItem), Inline (Plain, Emphasis, Code, Strong))
import PDF (PDFTree, pdfCreateCatalog, pdfCreatePageTree, pdfCreatePage, Page, Object (Indirect, Inline), Position (Point), pdfCreateTextObject, Text (Text), Dictionary, IndirectType (Dictionary), InlineType (Name), Rectangle (Rectangle), pdfCreateRectangleObject, Color (Color))
import FreeType (ft_With_FreeType, ft_Load_Char, FT_FaceRec (frGlyph), ft_With_Face, FT_GlyphSlotRec (gsrAdvance), FT_Vector (FT_Vector), ft_Set_Char_Size)
import Foreign (Storable(peek), Int64)
import Data.Char (ord)

margin :: Int
margin = 72 

width :: Int
width = 595

height :: Int
height = 842

paragraphFontAttr :: (Int64, String)
paragraphFontAttr = (12, "Plain")

strongFontAttr :: (Int64, String)
strongFontAttr = (12, "Strong")

emphasisFontAttr :: (Int64, String)
emphasisFontAttr = (12, "Emphasis")

codeFontAttr :: (Int64, String)
codeFontAttr = (12, "Code")

resources :: Dictionary
resources = [
        ("Font", Indirect (Dictionary [
            ("Plain", Indirect (Dictionary paragraphFont)),
            ("Strong", Indirect (Dictionary strongFont)),
            ("Emphasis", Indirect (Dictionary emphasisFont)),
            ("Code", Indirect (Dictionary codeFont))
        ]))
    ]

paragraphFont :: Dictionary 
paragraphFont = [
        ("Type", Inline (Name "Font")),
        ("Subtype", Inline (Name "Type1")),
        ("BaseFont", Inline (Name "Courier"))
    ]

strongFont :: Dictionary 
strongFont = [
        ("Type", Inline (Name "Font")),
        ("Subtype", Inline (Name "Type1")),
        ("BaseFont", Inline (Name "Courier-Bold"))
    ]

emphasisFont :: Dictionary 
emphasisFont = [
        ("Type", Inline (Name "Font")),
        ("Subtype", Inline (Name "Type1")),
        ("BaseFont", Inline (Name "Courier-Italic"))
    ]

codeFont :: Dictionary 
codeFont = [
        ("Type", Inline (Name "Font")),
        ("Subtype", Inline (Name "Type1")),
        ("BaseFont", Inline (Name "Courier"))
    ]

rowPadding :: Int
rowPadding = 2

paraPadding :: Int
paraPadding = 10

headingScale :: Int64
headingScale = 2

indent :: Int
indent = 24

start :: Position
start = Point margin (height - 100)

markdownToPDF :: MDTree -> IO PDFTree
markdownToPDF (Document blocks) = do
    pages <- blocksToPages blocks
    return (pdfCreateCatalog (pdfCreatePageTree pages))

data Bound = Rect Position Position
    deriving Show
data Node = Node [Object] Block Position | Final [Object] Position | NewPage
    deriving Show

-- | blocksToPages converts a list of blocks to a list of pages, 
-- wrapping objects to a new page before they reach within 'margin'
-- of the end of the page.
blocksToPages :: [Block] -> IO [Page]
blocksToPages [] = return []
blocksToPages blocks = do 
    let rect = Rect start (Point (width - margin) margin)
    (objs, newBlocks) <- blocksToPageObjects blocks rect
    let page = pdfCreatePage objs resources
    let pages = blocksToPages newBlocks
    let skipBlock = blocksToPages (tail newBlocks)
    if null objs then skipBlock else fmap (page:) pages

-- | blocksToPageObjects takes a list of blocks and a rectangle 
-- boundary, and pulls out blocks from the list until the 
-- rectangle is filled, returning the objects which fill the 
-- boundary and the remaining blocks
blocksToPageObjects :: [Block] -> Bound -> IO ([Object], [Block])
blocksToPageObjects [] _ = return ([], [])
blocksToPageObjects (block1:bs) rect = do 
    node <- blockToObject block1 rect
    case node of 
        NewPage -> return ([], block1:bs)
        Node objs block2 pos3 -> do
            (newObjs, newBlocks) <- blocksToPageObjects (block2:bs) (Rect pos3 pos2)
            return (objs ++ newObjs, newBlocks)
        Final objs pos3 -> do 
            (newObjs, newBlocks) <- blocksToPageObjects bs (Rect pos3 pos2)
            return (objs ++ newObjs, newBlocks)
    where 
        (Rect _ pos2) = rect

-- | convert a block to a pdf object
blockToObject :: Block -> Bound -> IO Node
blockToObject (Leaf l) bound = return (leafBlock l bound)
blockToObject (ListItem _ b) bound = blockToObject b bound
blockToObject (FencedCode _ ls) bound = return (codeBlock (FencedCode False) ls bound)
blockToObject (IndentCode _ ls) bound = return (codeBlock (IndentCode False) ls bound)
blockToObject (Paragraph inlines) bound = paragraphObject inlines bound
blockToObject (Quote blocks) bound = case blocks of 
    [] -> return (Final [] pos1)
    (block:bs) -> do 
        node <- blockToObject block (Rect indentPos pos2)
        case node of 
            NewPage -> return node
            Final objs pos3 -> return (Node objs (Quote bs) pos4)
                where 
                    (Point _ yPos3) = pos3
                    pos4 = Point xPos1 yPos3
            Node objs b pos3 -> return (Node objs (Quote (b:bs)) pos4)
                where 
                    (Point _ yPos3) = pos3
                    pos4 = Point xPos1 yPos3
    where 
        (Rect pos1 pos2) = bound
        (Point xPos1 yPos1) = pos1
        indentPos = Point (xPos1 + indent) yPos1

leafBlock :: Leaf -> Bound -> Node
leafBlock (Heading n s) (Rect pos1 pos2)
    | yPos2 > yPos3 = NewPage
    | otherwise = Final objs pos4
    where 
        (Point _ yPos2) = pos2
        (objs, pos3) = headingText n s (Rect pos1 pos2) 
        (Point xPos3 yPos3) = pos3
        pos4 = Point xPos3 (yPos3 - paraPadding)
leafBlock HorizontalRule (Rect pos1 pos2) 
    | yPos1 - yPos2 < 8 = NewPage 
    | otherwise = Final [obj] (Point xPos1 (yPos1 - 8))
    where 
        (Point xPos1 yPos1) = pos1
        (Point _ yPos2) = pos2
        rectWidth = width - margin - xPos1
        bgColor = Color 200 200 200
        startPos = Point xPos1 (yPos1 - 5)
        obj = pdfCreateRectangleObject (Rectangle startPos rectWidth 1) bgColor
leafBlock _ (Rect pos1 _) = Final [] pos1

headingAttr :: (Int64, String) -> Int -> (Int64, String)
headingAttr (fontSize, fontId) n = (fromIntegral (fontSize + (7 - fromIntegral n) * headingScale), fontId)

headingText :: Int -> [Inline] -> Bound -> ([Object], Position)
headingText _ [] (Rect _ pos2) = ([], pos2)
headingText n (text : inlines) (Rect pos1 pos2) = (pdfCreateTextObject obj : objs, pos3)
    where
        (obj, pos3) = case text of 
            Plain s -> textObject s (headingAttr paragraphFontAttr n) pos1
            Strong s -> textObject s (headingAttr strongFontAttr n) pos1
            Emphasis s -> textObject s (headingAttr emphasisFontAttr n) pos1
            Code s -> textObject s (headingAttr codeFontAttr n) pos1
        (objs, _) = headingText n inlines (Rect pos3 pos2)

codeBlock :: ([String] -> Block) -> [String] -> Bound -> Node
codeBlock code ls (Rect pos1 pos2) = case codeLines code ls (Rect indentPos pos2) of 
    NewPage -> NewPage
    Final objs (Point _ yPos3) -> Final (bg:objs) pos3
        where 
            bgRect = Rectangle rectPos (width - xPos1 - margin) (yPos3 - yPos1)
            bg = pdfCreateRectangleObject bgRect bgColor
            pos3 = Point xPos1 (yPos3 - 24)
    Node objs block (Point _ yPos3) -> Node (bg:objs) block pos3
        where 
            bgRect = Rectangle rectPos (width - xPos1 - margin) (yPos3 - yPos1)
            bg = pdfCreateRectangleObject bgRect bgColor
            pos3 = Point xPos1 (yPos3 - 24)
    where
        (Point xPos1 yPos1) = pos1
        rectPos = Point xPos1 (yPos1 - 12)
        bgColor = Color 200 200 200
        indentPos = Point (xPos1 + indent) (yPos1 - 24)

codeLines :: ([String] -> Block) -> [String] -> Bound -> Node
codeLines _ [] (Rect pos1 _) = Final [] pos1
codeLines code (l:ls) (Rect pos1 pos2) 
    | fromIntegral codeFontSize > yPos1 - yPos2 = NewPage
    | otherwise = case codeLines code ls (Rect pos3 pos2) of 
        NewPage -> Node [obj] (code ls) pos3
        Final objs pos4 -> Final (obj:objs) pos4
        Node objs block pos4 -> Node (obj:objs) block pos4
    where 
        (codeFontSize, _) = codeFontAttr
        (Point _ yPos1) = pos1 
        (Point _ yPos2) = pos2 

        (text, pos3) = textObject l codeFontAttr pos1
        obj = pdfCreateTextObject text

paragraphObject :: [Inline] -> Bound -> IO Node
paragraphObject [] (Rect _ pos2) = return (Final [] pos2)
paragraphObject (inlineText : line1) bound = do 
    let (Rect pos1 pos2) = bound
    let (Point xPos1 _) = pos1
    let paraWidth = 72 * (width - margin - xPos1)

    (line2, text) <- case inlineText of 
        Plain lineText -> wrapText paragraphFontAttr paraWidth "" lineText
        Strong lineText -> wrapText strongFontAttr paraWidth "" lineText
        Emphasis lineText -> wrapText emphasisFontAttr paraWidth "" lineText
        Code lineText -> wrapText codeFontAttr paraWidth "" lineText

    let (text1, pos3) = case inlineText of 
            Plain _ -> textObject line2 paragraphFontAttr pos1
            Strong _ -> textObject line2 strongFontAttr pos1
            Emphasis _ -> textObject line2 emphasisFontAttr pos1
            Code _ -> textObject line2 codeFontAttr pos1

    let remText = case inlineText of 
            Plain _ -> Plain text
            Strong _ -> Strong text
            Emphasis _ -> Emphasis text
            Code _ -> Code text

    let newInline = if null text then line1 else remText : line1
    textObj <- if null newInline then return text1 else textJustify text1 paraWidth

    let (Point xPos3 yPos3) = pos3
    let (Point _ yPos2) = pos2
    let pos4 = Point xPos3 (yPos3 - paraPadding)
    let obj = pdfCreateTextObject textObj
    let node 
            | yPos2 > yPos3 = NewPage
            | null newInline = Final [obj] pos4
            | otherwise = Node [obj] (Paragraph newInline) pos3

    return node

fontPath :: String
fontPath = "./fonts/CourierPrime-Regular.ttf"

addLine :: String -> IO [String] -> IO [String]
addLine line ss = do 
    ls <- ss
    return (line:ls)

-- | wrapText takes a fontSize, lineWidth, a string 'line' (the current text 
-- in the line) and a string 'text' (the remaining text), and returns
-- a pair (nextLine, remainingText) where nextLine is a substring of words 
-- from the string of text which fits the line width for the given font size 
-- and remainingText is the rest of the text not included in nextLine.
wrapText :: (Int64, String) -> Int -> String -> String -> IO (String, String)
wrapText _ _ line "" = return (line, "")
wrapText (fontSize, fontId) lineWidth line text = do 
    let currentLine = if null line then line else line ++ " "
    let (word, remainingLine) = splitSpace currentLine text
    wordCharWidths <- mapM (charWidth fontSize) word
    let wordWidth = sum wordCharWidths
    let wrappedText = wrapText (fontSize, fontId) lineWidth word remainingLine
    if wordWidth > lineWidth then return (line, text) else wrappedText

splitSpace :: String -> String -> (String, String) 
splitSpace current "" = (current, "")
splitSpace current (' ':remaining) = (current, remaining)
splitSpace current (c:remaining) = splitSpace (current ++ [c]) remaining

charWidth :: Int64 -> Char -> IO Int
charWidth fontSize c = ft_With_FreeType $ \lib -> 
    ft_With_Face lib fontPath 0 $ \face -> do
        ft_Set_Char_Size face 0 (fontSize * 72) 0 0
        ft_Load_Char face (fromIntegral $ ord c) 4
        slot <- peek . frGlyph =<< peek face
        let (FT_Vector vX _) = gsrAdvance slot
        return (fromIntegral vX)

-- | textJustify calculates the horizontal stretch on spaces required for 
-- a string of text to fill the given width.
textJustify :: Text -> Int -> IO Text
textJustify (Text s fontid fontSize _ pos) textWidth = do
    cWidths <- mapM (charWidth (fromIntegral fontSize)) s
    let cWidth = sum cWidths
    let spaceCount = length (filter (' ' ==) s)
    let spaceStretch = fromIntegral (textWidth - cWidth) / fromIntegral (72 * spaceCount)
    let stretch = if spaceCount == 0 then 1.0 else spaceStretch
    return (Text s fontid fontSize stretch pos)

textObject :: String -> (Int64, String) -> Position -> (Text, Position)
textObject s (fontSize, fontId) (Point x y) = (text, newPos)
    where 
        newPos = Point x (y - fromIntegral fontSize)
        text = Text s fontId (fromIntegral fontSize) 1.0 newPos
