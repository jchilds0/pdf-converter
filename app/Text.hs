module Text (module Text) where
import Markdown (MDTree (Document), Blocks, Leaf (Heading), Block (Leaf, Paragraph, Quote, IndentCode, FencedCode, ListItem), Marker)
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

paragraphFontSize :: Int64
paragraphFontSize = 12

codeFontSize :: Int
codeFontSize = 12

resources :: Dictionary
resources = [
        ("Font", Indirect (Dictionary [
            ("F13", Indirect (Dictionary paragraphFont))
        ]))
    ]

paragraphFont :: Dictionary 
paragraphFont = [
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
    pages <- blocksToPage blocks
    return (pdfCreateCatalog (pdfCreatePageTree [pages]))

blocksToPage :: Blocks -> IO Page
blocksToPage blocks = do 
    objs <- blocksToObjects start blocks
    return (pdfCreatePage objs resources)

blocksToObjects :: Position -> Blocks -> IO [Object]
blocksToObjects _ [] = return []
blocksToObjects pos (b:blocks) = do
    (obj, newPos) <- blockToObject b pos
    objs <- blocksToObjects newPos blocks
    return (obj ++ objs)

blockToObject :: Block -> Position -> IO ([Object], Position)
blockToObject (Leaf l) pos = return (leafBlock l pos)
blockToObject (Paragraph l) pos = paragraphBlock l pos
blockToObject (FencedCode _ ls) pos = return (codeBlock ls pos)
blockToObject (IndentCode _ ls) pos = return (codeBlock ls pos)
blockToObject (Quote blocks) pos = quoteBlocks blocks pos
blockToObject (ListItem m b) pos = listItem m b pos

leafBlock :: Leaf -> Position -> ([Object], Position)
leafBlock (Heading n s) pos1 = ([pdfCreateTextObject obj], pos3)
    where 
        size = paragraphFontSize + (7 - fromIntegral n) * headingScale
        (obj, pos2) = textObject s (fromIntegral size) pos1
        (Point xPos yPos) = pos2
        pos3 = Point xPos (yPos - paraPadding)
leafBlock _ (Point x y) = ([], Point x y)

paragraphBlock :: String -> Position -> IO ([Object], Position)
paragraphBlock line pos = do 
    let (Point xPos _) = pos
    let paraWidth = 72 * (width - margin - xPos)
    ls <- wrapText paragraphFontSize paraWidth "" line 
    paragraphLines ls pos paraWidth

paragraphLines :: [String] -> Position -> Int -> IO ([Object], Position)
paragraphLines [] pos _ = return ([], pos)
paragraphLines [l] pos1 _ = return ([obj], pos3)
    where 
        (text, pos2) = textObject l (fromIntegral paragraphFontSize) pos1
        (Point xPos yPos) = pos2
        pos3 = Point xPos (yPos - paraPadding)
        obj = pdfCreateTextObject text
paragraphLines (l:ls) pos1 paraWidth = do
    let (text1, pos2) = textObject l (fromIntegral paragraphFontSize) pos1
    text2 <- textJustify text1 paraWidth
    let obj = pdfCreateTextObject text2
    (objs, pos3) <- paragraphLines ls pos2 paraWidth
    return (obj:objs, pos3)

codeBlock :: [String] -> Position -> ([Object], Position)
codeBlock ls (Point xPos1 yPos1) = (objs, pos2)
    where
        bgRect = Rectangle (Point xPos1 (yPos1 - 12)) (width - xPos1 - margin) (yPos2 - yPos1)
        bgColor = Color 200 200 200
        bg = pdfCreateRectangleObject bgRect bgColor

        indentPos = Point (xPos1 + indent) (yPos1 - 24)
        (textObjs, pos1) = codeLines ls indentPos
        (Point _ yPos2) = pos1
        pos2 = Point xPos1 (yPos2 - 24)

        objs = bg:textObjs

codeLines :: [String] -> Position -> ([Object], Position)
codeLines [] pos = ([], pos)
codeLines (l:ls) pos1 = (pdfCreateTextObject obj:objs, pos3)
    where 
        (obj, pos2) = textObject l codeFontSize pos1
        (objs, pos3) = codeLines ls pos2

quoteBlocks :: Blocks -> Position -> IO ([Object], Position) 
quoteBlocks blocks (Point xPos1 yPos1) = do
    let indentPos = Point (xPos1 + indent) yPos1
    (objs, pos1) <- quoteBlockItems blocks indentPos
    let Point _ yPos2 = pos1
    let pos2 = Point xPos1 yPos2
    return (objs, pos2)

quoteBlockItems :: Blocks -> Position -> IO ([Object], Position)
quoteBlockItems [] pos = return ([], pos)
quoteBlockItems (b:blocks) pos1 = do
    (obj, pos2) <- blockToObject b pos1
    (objs, pos3) <- quoteBlockItems blocks pos2
    return (obj ++ objs, pos3)

listItem :: Marker -> Block -> Position -> IO ([Object], Position)
listItem _ b pos = blockToObject b pos

fontPath :: String
fontPath = "./fonts/CourierPrime-Regular.ttf"

addLine :: String -> IO [String] -> IO [String]
addLine line ss = do 
    ls <- ss
    return (line:ls)

wrapText :: Int64 -> Int -> String -> String -> IO [String]
wrapText _ _ line "" = return [line]
wrapText fontSize w line text = do 
    let currentLine = if null line then "" else line ++ " "
    let (c1, r1) = splitSpace currentLine text
    nextCharWidth <- mapM (charWidth fontSize) c1
    let nextLineWidth = sum nextCharWidth

    if nextLineWidth <= w then wrapText fontSize w c1 r1 else addLine line (wrapText fontSize w "" text)

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

textJustify :: Text -> Int -> IO Text
textJustify (Text s fontSize _ pos) textWidth = do
    cWidths <- mapM (charWidth (fromIntegral fontSize)) s
    let cWidth = sum cWidths
    let spaceCount = length (filter (' ' ==) s)
    let spaceStretch = fromIntegral (textWidth - cWidth) / fromIntegral (72 * spaceCount)
    let stretch = if spaceCount == 0 then 1.0 else spaceStretch
    return (Text s fontSize stretch pos)

textObject :: String -> Int -> Position -> (Text, Position)
textObject s fontSize (Point x y) = (text, newPos)
    where 
        newPos = Point x (y - fontSize)
        text = Text s fontSize 1.0 newPos
