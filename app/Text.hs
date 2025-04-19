module Text (module Text) where
import Markdown (MDTree (Document), Leaf (Heading, HorizontalRule), Block (Leaf, Paragraph, Quote, IndentCode, FencedCode, ListItem), InlineType (Plain, Strong, Emphasis, Code), Inline (Inlines))
import PDF (PDFTree, pdfCreateCatalog, pdfCreatePageTree, pdfCreatePage, Page, Object (Indirect, Inline), Position (Point), pdfCreateTextObject, Text (Text), Dictionary, IndirectType (Dictionary), InlineType (Name), Rectangle (Rectangle), pdfCreateRectangleObject, Color (Color), positionReplaceY, positionOffsetX, positionOffsetY)
import FreeType (ft_With_FreeType, ft_Load_Char, FT_FaceRec (frGlyph), ft_With_Face, FT_GlyphSlotRec (gsrAdvance), FT_Vector (FT_Vector), ft_Set_Char_Size)
import Foreign (Storable(peek), Int64)
import Data.Char (ord)

data FontAttributes = FontAttrs Int64 String String

margin :: Int
margin = 72 

width :: Int
width = 595

height :: Int
height = 842

paragraphFontAttr :: FontAttributes
paragraphFontAttr = FontAttrs 12 "Plain" "./fonts/CourierPrime-Regular.ttf"

strongFontAttr :: FontAttributes
strongFontAttr = FontAttrs 12 "Strong" "./fonts/CourierPrime-Bold.ttf"

emphasisFontAttr :: FontAttributes
emphasisFontAttr = FontAttrs 12 "Emphasis" "./fonts/CourierPrime-Italic.ttf"

codeFontAttr :: FontAttributes
codeFontAttr = FontAttrs 12 "Code" "./fonts/CourierPrime-Regular.ttf"

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
blockToObject (Leaf l) bound = leafBlock l bound
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
            Final objs pos3 -> return (Node objs (Quote bs) (positionReplaceY pos1 pos3))
            Node objs b pos3 -> return (Node objs (Quote (b:bs)) (positionReplaceY pos1 pos3))
    where 
        (Rect pos1 pos2) = bound
        indentPos = positionOffsetX pos1 indent

leafBlock :: Leaf -> Bound -> IO Node
leafBlock (Heading n s) (Rect pos1 pos2) = do
        (objs, pos3) <- headingText n s (Rect pos1 pos2) 

        let (Point _ yPos2) = pos2
        let (Point _ yPos3) = pos3
        let pos4 = positionOffsetY pos3 (-paraPadding)
        if yPos2 > yPos3 then return NewPage else return (Final objs pos4)

leafBlock HorizontalRule (Rect pos1 pos2) 
    | yPos1 - yPos2 < 8 = return NewPage 
    | otherwise = return (Final [obj] (positionOffsetY pos1 (-8)))
    where 
        (Point xPos1 yPos1) = pos1
        (Point _ yPos2) = pos2
        rectWidth = width - margin - xPos1
        bgColor = Color 200 200 200
        startPos = positionOffsetY pos1 (-5)

        obj = pdfCreateRectangleObject (Rectangle startPos rectWidth 1) bgColor
leafBlock _ (Rect pos1 _) = return (Final [] pos1)

headingAttr :: FontAttributes -> Int -> FontAttributes
headingAttr (FontAttrs fontSize fontId fontPath) n = FontAttrs headingSize fontId fontPath
    where 
        headingSize = fromIntegral (fontSize + (7 - fromIntegral n) * headingScale)

inlineAttr :: Inline -> FontAttributes
inlineAttr (Inlines Plain _) = paragraphFontAttr
inlineAttr (Inlines Strong _) = strongFontAttr
inlineAttr (Inlines Emphasis _) = emphasisFontAttr
inlineAttr (Inlines Code _) = codeFontAttr

headingText :: Int -> [Inline] -> Bound -> IO ([Object], Position)
headingText _ [] (Rect _ pos2) = return ([], pos2)
headingText n (inline : inlines) (Rect pos1 pos2) = do 
    let attrs = headingAttr (inlineAttr inline) n
    let (obj, pos3) = textObject inline attrs pos1
    let (Inlines _ line) = inline

    cWidths <- mapM (charWidth attrs) line
    let textWidth = div (sum cWidths) 72
    (objs, _) <- headingText n inlines (Rect (positionOffsetX pos1 textWidth) pos2)
    return (pdfCreateTextObject obj : objs, pos3)

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
        (FontAttrs codeFontSize _ _) = codeFontAttr
        (Point _ yPos1) = pos1 
        (Point _ yPos2) = pos2 

        (text, pos3) = textObject (Inlines Code l) codeFontAttr pos1
        obj = pdfCreateTextObject text

paragraphObject :: [Inline] -> Bound -> IO Node
paragraphObject [] (Rect _ pos2) = return (Final [] pos2)
paragraphObject (inline : ls) (Rect pos1 pos2) = do 
    let (Point xPos1 _) = pos1
    let paraWidth = 72 * (width - margin - xPos1)

    let fontAttrs = inlineAttr inline
    (nextInline, remainingInline) <- wrapText inline "" paraWidth

    let (Inlines _ remainingText) = remainingInline
    let (text, pos3) = textObject nextInline paragraphFontAttr pos1
    let newInlines = if null remainingText then ls else remainingInline : ls

    textObj <- if null newInlines then return text else textJustify text fontAttrs paraWidth
    let (Point _ yPos3) = pos3
    let (Point _ yPos2) = pos2

    let obj = pdfCreateTextObject textObj
    let node 
            | yPos2 > yPos3 = NewPage
            | null newInlines = Final [obj] (positionOffsetY pos3 (- paraPadding))
            | otherwise = Node [obj] (Paragraph newInlines) pos3

    return node

addLine :: String -> IO [String] -> IO [String]
addLine line ss = do 
    ls <- ss
    return (line:ls)

-- | wrapText takes a inline, a string and the line width, and returns
-- a pair of inlines (nextLine, remainingLine) where nextLine is the 
-- largest prefix of inline which fits in the line width, and 
-- remainingLine is the remaining part of the inline.
wrapText :: Inline -> String -> Int -> IO (Inline, Inline)
wrapText (Inlines inlineType "") _ _ = return (Inlines inlineType "", Inlines inlineType "")
wrapText inline line lineWidth = do 
    let (Inlines inlineType text) = inline
    let currentLine = if null line then line else line ++ " "
    let (newLine, remainingLine) = splitSpace currentLine text

    let fontAttrs = inlineAttr inline
    wordCharWidths <- mapM (charWidth fontAttrs) newLine
    let wordWidth = sum wordCharWidths

    let currentText = (Inlines inlineType line, inline)
    let wrappedText = wrapText (Inlines inlineType remainingLine) newLine lineWidth
    if wordWidth > lineWidth then return currentText else wrappedText

splitSpace :: String -> String -> (String, String) 
splitSpace current "" = (current, "")
splitSpace current (' ':remaining) = (current, remaining)
splitSpace current (c:remaining) = splitSpace (current ++ [c]) remaining

charWidth :: FontAttributes -> Char -> IO Int
charWidth (FontAttrs fontSize _ fontPath) c = ft_With_FreeType $ \lib -> 
    ft_With_Face lib fontPath 0 $ \face -> do
        ft_Set_Char_Size face 0 (fontSize * 72) 0 0
        ft_Load_Char face (fromIntegral $ ord c) 4
        slot <- peek . frGlyph =<< peek face
        let (FT_Vector vX _) = gsrAdvance slot
        return (fromIntegral vX)

-- | textJustify calculates the horizontal stretch on spaces required for 
-- a string of text to fill the given width.
textJustify :: Text -> FontAttributes -> Int -> IO Text
textJustify (Text s fontid fontSize _ pos) fontAttrs textWidth = do
    cWidths <- mapM (charWidth fontAttrs) s
    let cWidth = sum cWidths
    let spaceCount = length (filter (' ' ==) s)
    let spaceStretch = fromIntegral (textWidth - cWidth) / fromIntegral (72 * spaceCount)
    let stretch = if spaceCount == 0 then 1.0 else spaceStretch
    return (Text s fontid fontSize stretch pos)

textObject :: Inline -> FontAttributes -> Position -> (Text, Position)
textObject (Inlines _ s) (FontAttrs fontSize fontId _) pos1 = (text, pos2)
    where 
        pos2 = positionOffsetY pos1 (- fromIntegral fontSize)
        text = Text s fontId (fromIntegral fontSize) 1.0 pos2 
