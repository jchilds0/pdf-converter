module Text (module Text) where
import Markdown (MDTree (Document), Blocks, Leaf (Heading), Block (Leaf, Paragraph, Quote, Code, ListItem), Marker)
import PDF (PDFTree, pdfCreateCatalog, pdfCreatePageTree, pdfCreatePage, Page, Object (Indirect, Inline), Position (Point), pdfCreateTextObject, Text (Text), Dictionary, IndirectType (Dictionary), InlineType (Name))

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
rowPadding = 4

headingScale :: Int
headingScale = 3

start :: Position
start = Point margin (height - margin)

markdownToPDF :: MDTree -> PDFTree
markdownToPDF (Document blocks) = pdfCreateCatalog (pdfCreatePageTree pages)
    where
        wrappedBlocks = wrapBlocks blocks
        pageBlocks = pageBreakBlocks wrappedBlocks
        pages = map blocksToPage pageBlocks

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
blockToObject (Paragraph ls) pos = paragraphToObjects ls pos
blockToObject (Code _ ls) pos = codeToObjects ls pos
blockToObject (Quote blocks) pos = quoteToObjects blocks pos
blockToObject (ListItem m b) pos = listItemToObject m b pos

leafToObjects :: Leaf -> Position -> ([Object], Position)
leafToObjects (Heading n s) pos = ([obj], newPos)
    where 
        fontSize = paragraphFontSize + (7 - n) * headingScale
        (obj, newPos) = textToObject s fontSize pos
leafToObjects _ (Point x y) = ([], Point x (y - paragraphFontSize))

paragraphToObjects :: [String] -> Position -> ([Object], Position)
paragraphToObjects [] pos = ([], pos)
paragraphToObjects (l:ls) pos1 = (obj:objs, pos3)
    where 
        (obj, pos2) = textToObject l paragraphFontSize pos1
        (objs, pos3) = paragraphToObjects ls pos2

codeToObjects :: [String] -> Position -> ([Object], Position)
codeToObjects [] pos = ([], pos)
codeToObjects (l:ls) pos1 = (obj:objs, pos3)
    where 
        (obj, pos2) = textToObject l codeFontSize pos1
        (objs, pos3) = paragraphToObjects ls pos2

quoteToObjects :: Blocks -> Position -> ([Object], Position)
quoteToObjects [] pos = ([], pos)
quoteToObjects (b:blocks) pos1 = (obj ++ objs, pos3)
    where 
        (obj, pos2) = blockToObject b pos1
        (objs, pos3) = quoteToObjects blocks pos2

listItemToObject :: Marker -> Block -> Position -> ([Object], Position)
listItemToObject _ b pos = blockToObject b pos

textToObject :: String -> Int -> Position -> (Object, Position)
textToObject s fontSize (Point x y) = (obj, newPos)
    where 
        newPos = Point x (y - fontSize - rowPadding)
        text = Text s fontSize newPos
        obj = pdfCreateTextObject text

wrapBlocks :: Blocks -> Blocks
wrapBlocks = id

pageBreakBlocks :: Blocks -> [Blocks]
pageBreakBlocks blocks = [blocks]

wrapText :: Integer -> String -> [(String, Integer)]
wrapText width text = []
    where 
        ws = words text

