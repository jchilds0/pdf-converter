module Text (module Text) where
import Markdown (MDTree (Document), Blocks, Leaf (Heading), Block (Leaf, Paragraph, Quote, Code, ListItem), Marker)
import PDF (PDFTree, pdfCreateCatalog, pdfCreatePageTree, pdfCreatePage, Page, Object (Indirect, Inline), Position (Point), pdfCreateTextObject, Text (Text), Dictionary, IndirectType (Dictionary), InlineType (Name))
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

headingScale :: Int64
headingScale = 2

start :: Position
start = Point margin (height - 100)

markdownToPDF :: MDTree -> IO PDFTree
markdownToPDF (Document blocks) = do
    wrappedBlocks <- mapM wrapBlock blocks
    let pageBlocks = pageBreakBlocks (concat wrappedBlocks)
    let pages = map blocksToPage pageBlocks
    return (pdfCreateCatalog (pdfCreatePageTree pages))

blocksToPage :: Blocks -> Page
blocksToPage blocks = pdfCreatePage objs resources
    where 
        objs = blocksToObjects start blocks

blocksToObjects :: Position -> Blocks -> [Object]
blocksToObjects _ [] = []
blocksToObjects pos (b:blocks) = obj ++ objs
    where 
        (obj, newPos) = blockToObject b pos
        objs = blocksToObjects newPos blocks

blockToObject :: Block -> Position -> ([Object], Position)
blockToObject (Leaf l) pos = leafToObjects l pos
blockToObject (Paragraph l) pos = paragraphToObjects [l] pos
blockToObject (Code _ ls) pos = codeToObjects ls pos
blockToObject (Quote blocks) pos = quoteToObjects blocks pos
blockToObject (ListItem m b) pos = listItemToObject m b pos

leafToObjects :: Leaf -> Position -> ([Object], Position)
leafToObjects (Heading n s) pos = ([obj], newPos)
    where 
        size = paragraphFontSize + (7 - fromIntegral n) * headingScale
        (obj, newPos) = textObject s (fromIntegral size) pos
leafToObjects _ (Point x y) = ([], Point x y)

paragraphToObjects :: [String] -> Position -> ([Object], Position)
paragraphToObjects [] pos = ([], pos)
paragraphToObjects (l:ls) pos1 = (obj:objs, pos3)
    where 
        (obj, pos2) = textObject l (fromIntegral paragraphFontSize) pos1
        (objs, pos3) = paragraphToObjects ls pos2

codeToObjects :: [String] -> Position -> ([Object], Position)
codeToObjects [] pos = ([], pos)
codeToObjects (l:ls) pos1 = (obj:objs, pos3)
    where 
        (obj, pos2) = textObject l codeFontSize pos1
        (objs, pos3) = paragraphToObjects ls pos2

quoteToObjects :: Blocks -> Position -> ([Object], Position)
quoteToObjects [] pos = ([], pos)
quoteToObjects (b:blocks) pos1 = (obj ++ objs, pos3)
    where 
        (obj, pos2) = blockToObject b pos1
        (objs, pos3) = quoteToObjects blocks pos2

listItemToObject :: Marker -> Block -> Position -> ([Object], Position)
listItemToObject _ b pos = blockToObject b pos

textObject :: String -> Int -> Position -> (Object, Position)
textObject s fontSize (Point x y) = (obj, newPos)
    where 
        newPos = Point x (y - fontSize - rowPadding)
        text = Text s fontSize newPos
        obj = pdfCreateTextObject text

fontPath :: String
fontPath = "./fonts/CourierPrime-Regular.ttf"

wrapBlock :: Block -> IO Blocks
wrapBlock (Paragraph text) = do
    let paraWidth = width - 2 * margin
    ls <- wrapText paragraphFontSize paraWidth "" text
    let blocks = map Paragraph ls
    return blocks
wrapBlock block = return [block]

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

    if nextLineWidth <= w * 72 then wrapText fontSize w c1 r1 else addLine line (wrapText fontSize w "" text)

splitSpace :: String -> String -> (String, String) 
splitSpace current "" = (current, "")
splitSpace current (' ':remaining) = (current, remaining)
splitSpace current (c:remaining) = splitSpace (current ++ [c]) remaining

charWidth :: Int64 -> Char -> IO Int
charWidth fontSize c = ft_With_FreeType $ \lib -> 
    ft_With_Face lib fontPath 0 $ \face -> do
        ft_Set_Char_Size face 0 (fontSize * 64) 0 0
        ft_Load_Char face (fromIntegral $ ord c) 4
        slot <- peek . frGlyph =<< peek face
        let (FT_Vector vX _) = gsrAdvance slot
        return (fromIntegral vX)

pageBreakBlocks :: Blocks -> [Blocks]
pageBreakBlocks blocks = [blocks]

