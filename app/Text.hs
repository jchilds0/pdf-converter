module Text (module Text) where
import Markdown (MDTree (Document), Leaf (Heading, HorizontalRule), Block (Leaf, Paragraph, Quote, IndentCode, FencedCode, ListItem), InlineType (Plain, Strong, Emphasis, Code), Inline (Inlines))
import PDF (PDFTree, pdfCreateCatalog, pdfCreatePageTree, pdfCreatePage, Page, Object (Indirect, Inline), Position (Point), pdfCreateTextObject, Text (Text), Dictionary, IndirectType (Dictionary), InlineType (Name), Rectangle (Rectangle), pdfCreateRectangleObject, Color (Color), positionReplaceY, positionOffsetX, positionOffsetY, FontAttributes (FontAttrs))
import FreeType (ft_With_FreeType, ft_Load_Char, FT_FaceRec (frGlyph), ft_With_Face, FT_GlyphSlotRec (gsrAdvance), FT_Vector (FT_Vector), ft_Set_Char_Size)
import Data.Char (ord)
import Foreign (Storable(peek))
import Control.Monad (zipWithM)
import Debug.Trace (trace, traceShow)

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

inlineAttr :: Inline -> FontAttributes
inlineAttr (Inlines Plain _) = paragraphFontAttr
inlineAttr (Inlines Strong _) = strongFontAttr
inlineAttr (Inlines Emphasis _) = emphasisFontAttr
inlineAttr (Inlines Code _) = codeFontAttr

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

headingScale :: Int
headingScale = 2

indent :: Int
indent = 24

start :: Position
start = Point margin (height - 100)

markdownToPDF :: MDTree -> IO PDFTree
markdownToPDF (Document blocks) = do
    pages <- blocksToPages blocks
    return (pdfCreateCatalog (pdfCreatePageTree pages))

-- | Bound is a rectangle with the first position in the top left 
--   and second position in the bottom right, and describes the 
--   remaining space on a page
data Bound = Rect Position Position
    deriving Show

type Width = Int
type Height = Int

boundContainsX :: Bound -> Width -> Bool
boundContainsX (Rect (Point xPos1 _) (Point xPos2 _)) w = xPos1 + w <= xPos2

boundContainsY :: Bound -> Height -> Bool
boundContainsY (Rect (Point _ yPos1) (Point _ yPos2)) h = yPos1 - h >= yPos2

boundWidth :: Bound -> Width
boundWidth (Rect (Point xPos1 _) (Point xPos2 _)) = xPos2 - xPos1

boundHeight :: Bound -> Width
boundHeight (Rect (Point _ yPos1) (Point _ yPos2)) = yPos1 - yPos2

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
leafBlock (Heading n s) bound = do
        (objs, h) <- headingText n s bound

        let (Rect pos1 _) = bound 
        let pos3 = positionOffsetY pos1 (-h - paraPadding)
        if boundContainsY bound h then return (Final objs pos3) else return NewPage
leafBlock HorizontalRule bound
    | not (boundContainsY bound 8) = return NewPage 
    | otherwise = return (Final [obj] (positionOffsetY pos1 (-8)))
    where 
        (Rect pos1 _) = bound
        (Point xPos1 _) = pos1
        rectWidth = width - margin - xPos1
        bgColor = Color 200 200 200
        startPos = positionOffsetY pos1 (-5)

        obj = pdfCreateRectangleObject (Rectangle startPos rectWidth 1) bgColor
leafBlock _ (Rect pos1 _) = return (Final [] pos1)

headingAttr :: FontAttributes -> Int -> FontAttributes
headingAttr (FontAttrs fontSize fontId fontPath) n = FontAttrs headingSize fontId fontPath
    where 
        headingSize = fromIntegral (fontSize + (7 - fromIntegral n) * headingScale)

headingText :: Int -> [Inline] -> Bound -> IO ([Object], Height)
headingText _ [] _ = return ([], 0)
headingText n (inline : inlines) (Rect pos1 pos2) = do 
    let attrs = headingAttr (inlineAttr inline) n
    let (obj, h1) = textObject inline attrs pos1
    let (Inlines _ line) = inline

    tWidth <- stringWidth line attrs
    (objs, h2) <- headingText n inlines (Rect (positionOffsetX pos1 tWidth) pos2)
    return (pdfCreateTextObject obj : objs, max h1 h2)

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
codeLines code (l:ls) bound
    | not (boundContainsY bound codeFontSize) = NewPage
    | otherwise = case codeLines code ls (Rect pos3 pos2) of 
        NewPage -> Node [obj] (code ls) pos3
        Final objs pos4 -> Final (obj:objs) pos4
        Node objs block pos4 -> Node (obj:objs) block pos4
    where 
        (FontAttrs codeFontSize _ _) = codeFontAttr
        (Rect pos1 pos2) = bound

        (text, h) = textObject (Inlines Code l) codeFontAttr pos1
        pos3 = positionOffsetY pos1 (-h)
        obj = pdfCreateTextObject text

paragraphObject :: [Inline] -> Bound -> IO Node
paragraphObject [] (Rect pos1 _) = return (Final [] pos1)
paragraphObject inlines bound = do 
    (texts, newInlines, h) <- paragraphLineInlines inlines bound 
    tWidths <- mapM textWidth texts

    let (Rect pos1 _) = bound
    let (Point xPos1 _) = pos1
    let posSplit = arrangePositions tWidths xPos1 (sum tWidths) (boundWidth bound)

    -- let fontAttrs = inlineAttr inline
    -- textObj <- if null newInlines then return text else textJustify text fontAttrs paraWidth
    textAdjs <- zipWithM textJustify texts posSplit
    let objs = map pdfCreateTextObject textAdjs

    let node 
            | not (boundContainsY bound h) = NewPage
            | null newInlines = Final objs (positionOffsetY pos1 (-h - paraPadding))
            | otherwise = Node objs (Paragraph newInlines) (positionOffsetY pos1 (-h)) 

    return node

-- | paragraphLineInlines takes a list of inlines and a rectangle bound,
--   and returns (nextInlines, remainingInlines), where nextInlines is 
--   the maximum prefix of the inlines list which fits in one line of the 
--   rectangle bound, and inlines = nextInlines ++ remainingInlines
paragraphLineInlines :: [Inline] -> Bound -> IO ([Text], [Inline], Height) 
paragraphLineInlines [] (Rect _ _) = return ([], [], 0)
paragraphLineInlines (inline : ls) bound = do 
    let (Rect pos1 pos2) = bound
    (nextInline, remainingInline) <- wrapText inline "" (boundWidth bound)

    let (Inlines _ remainingText) = remainingInline
    let (text, h1) = textObject nextInline (inlineAttr nextInline) pos1
    textW <- textWidth text

    (paraText, paraInlines, h2) <- paragraphLineInlines ls (Rect (positionOffsetX pos1 textW) pos2)
    let h = max h1 h2

    let (Inlines _ nextLine) = nextInline
    let objs
            | null nextLine = ([], inline : ls, h)
            | null remainingText = (text : paraText, paraInlines, h)
            | otherwise = ([text], remainingInline : ls, h)

    return objs

-- | wrapText takes a inline, a string and the line width, and returns
-- a pair of inlines (nextLine, remainingLine) where nextLine is the 
-- largest prefix of inline which fits in the line width, and 
-- remainingLine is the remaining part of the inline.
wrapText :: Inline -> String -> Int -> IO (Inline, Inline)
wrapText (Inlines inlineType "") line _ = return (Inlines inlineType line, Inlines inlineType "")
wrapText inline line lineWidth = do 
    let (Inlines inlineType text) = inline
    let currentLine = if null line then line else line ++ " "
    let (newLine, remainingLine) = splitSpace currentLine text

    let fontAttrs = inlineAttr inline
    wordWidth <- stringWidth newLine fontAttrs 

    let currentText = (Inlines inlineType line, inline)
    let wrappedText = wrapText (Inlines inlineType remainingLine) newLine lineWidth
    if wordWidth > lineWidth then return currentText else wrappedText

splitSpace :: String -> String -> (String, String) 
splitSpace current "" = (current, "")
splitSpace current " " = (current ++ " ", "")
splitSpace current (' ':remaining) = (current, remaining)
splitSpace current (c:remaining) = splitSpace (current ++ [c]) remaining

charWidth :: FontAttributes -> Char -> IO Int
charWidth (FontAttrs fontSize _ fontPath) c = ft_With_FreeType $ \lib -> 
    ft_With_Face lib fontPath 0 $ \face -> do
        ft_Set_Char_Size face 0 (fromIntegral fontSize * 72) 0 0
        ft_Load_Char face (fromIntegral $ ord c) 4
        slot <- peek . frGlyph =<< peek face
        let (FT_Vector vX _) = gsrAdvance slot
        return (fromIntegral vX)

stringWidth :: String -> FontAttributes -> IO Int
stringWidth text attrs = do
    cWidths <- mapM (charWidth attrs) text
    return (div (sum cWidths) 72)

textWidth :: Text -> IO Int
textWidth (Text text attrs _ _) = stringWidth text attrs

arrangePositions :: [Int] -> Int -> Width -> Width -> [(Int, Width)]
arrangePositions [] _ _ _ = []
arrangePositions (w : ws) currentPos itemsWidth totalWidth = item : arrangePositions ws newPos itemsWidth totalWidth
    where 
        itemWidth = div (totalWidth * w) itemsWidth  
        item = (currentPos, itemWidth)
        newPos = currentPos + itemWidth

-- | textJustify calculates the horizontal stretch on spaces required for 
-- a string of text to fill the given width.
textJustify :: Text -> (Int, Width) -> IO Text
textJustify (Text text fontAttrs _ pos) (xPos, textW) = do
    cWidth <- stringWidth text fontAttrs

    let spaceCount = length (filter (' ' ==) text)
    let spaceStretch = fromIntegral (textW - cWidth) / fromIntegral spaceCount
    let stretch = if spaceCount == 0 then 1.0 else spaceStretch

    let (Point _ yPos) = pos
    return (Text text fontAttrs stretch (Point xPos yPos))

textObject :: Inline -> FontAttributes -> Position -> (Text, Height)
textObject (Inlines _ s) fontAttrs pos1 = (text, fontSize)
    where 
        (FontAttrs fontSize _ _) = fontAttrs
        pos2 = positionOffsetY pos1 (- fontSize)
        text = Text s fontAttrs 1.0 pos2 
