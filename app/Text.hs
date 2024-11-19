module Text (module Text) where
import Markdown (MDTree (Document), Blocks, Leaf (Heading), Block (Leaf, Paragraph, Quote, Code, ListItem), Marker)
import PDF (PDFTree, pdfCreateCatalog, pdfCreatePageTree, pdfCreatePage, Page, Object (Indirect, Inline), Position (Point), pdfCreateTextObject, Text (Text), Dictionary, IndirectType (Dictionary), InlineType (Name))
import FreeType (ft_With_FreeType, ft_Load_Char, FT_FaceRec (frGlyph), ft_With_Face, FT_GlyphSlotRec (gsrAdvance), FT_Vector (FT_Vector), ft_Set_Char_Size)
import Foreign (Storable(peek))
import Data.Char (ord)
import Debug.Trace (trace)

margin :: Int
margin = 72 

width :: Int
width = 595

height :: Int
height = 842

paragraphFontSize :: Int
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

headingScale :: Int
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
        fontSize = paragraphFontSize + (7 - n) * headingScale
        (obj, newPos) = textObject s fontSize pos
leafToObjects _ (Point x y) = ([], Point x y)

paragraphToObjects :: [String] -> Position -> ([Object], Position)
paragraphToObjects [] pos = ([], pos)
paragraphToObjects (l:ls) pos1 = (obj:objs, pos3)
    where 
        (obj, pos2) = textObject l paragraphFontSize pos1
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
    let paraWidth = width - margin
    ls <- wrapText paraWidth "" text
    let blocks = map Paragraph ls
    return blocks
wrapBlock block = return [block]

addLine :: String -> IO [String] -> IO [String]
addLine line ss = do 
    ls <- ss
    return (line:ls)

wrapText :: Int -> String -> String -> IO [String]
wrapText _ line "" = return [line]
wrapText w line text = do 
    let currentLine = if null line then "" else line ++ " "
    let (c1, r1) = splitSpace currentLine text
    nextCharWidth <- mapM charWidth c1
    let nextLineWidth = sum nextCharWidth

    if nextLineWidth <= w * 72 then wrapText w c1 r1 else addLine line (wrapText w "" text)

splitSpace :: String -> String -> (String, String) 
splitSpace current "" = (current, "")
splitSpace current (' ':remaining) = (current, remaining)
splitSpace current (c:remaining) = splitSpace (current ++ [c]) remaining

charWidth :: Char -> IO Int
charWidth c = ft_With_FreeType $ \lib -> 
    ft_With_Face lib fontPath 0 $ \face -> do
        ft_Set_Char_Size face 0 (16 * 64) 0 0
        ft_Load_Char face (fromIntegral $ ord c) 4
        slot <- peek . frGlyph =<< peek face
        let (FT_Vector vX _) = gsrAdvance slot
        return (fromIntegral vX)

pageBreakBlocks :: Blocks -> [Blocks]
pageBreakBlocks blocks = [blocks]

