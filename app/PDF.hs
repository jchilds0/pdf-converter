module PDF (module PDF) where 
import Text.Printf (printf)

type Name = String
type Dictionary = [(Name, Object)]
data InlineType = Boolean Bool | Integer Int | String String | Name Name
    deriving (Show, Eq)
data IndirectType = Array [Object] | Dictionary Dictionary | Stream Dictionary [Char]
    deriving (Show, Eq)
data Object = Inline InlineType | Indirect IndirectType
    deriving (Show, Eq)

type ObjectNumber = Int
type GenNumber = Int 
type ByteOffset = Int
type ObjectRef = (ObjectNumber, GenNumber, ByteOffset)

type Page = Object
newtype Pages = Pages Dictionary
    deriving (Show, Eq)

type Catalog = Dictionary
newtype PDFTree = PDFTree Catalog
    deriving (Show, Eq)

type Objects = [(String, ObjectRef)]

pdfHeader :: String
pdfHeader = "%PDF-2.0\n"

pdfXRef:: String
pdfXRef = "startxref\n"

pdfTrailer :: String
pdfTrailer = "%%EOF"

generatePDF :: PDFTree -> String
generatePDF (PDFTree root) = bodyString ++ xrefString ++ trailerString
    where 
        objects = generateObject [] (Indirect (Dictionary root))
        rootRef = snd (head objects)
        bodyObjects = ("", rootRef):tail objects

        (bodyString, newObjects) = generateBody bodyObjects pdfHeader
        xrefString = generateXRef newObjects 
        xrefOffset = length bodyString
        trailerString = generateTrailer (length newObjects) rootRef xrefOffset

generateBody :: Objects -> String -> (String, Objects)
generateBody [] string = (string, [])
generateBody (obj:objects) string = (newString, newObj:newObjects)
    where 
        (objString, objRef) = obj
        (objNum, objGenNum, _) = objRef
        newObjRef = (objNum, objGenNum, length string)
        newObj = (objString, newObjRef)

        tempString = string ++ objString 
        (newString, newObjects) = generateBody objects tempString

generateXRef :: Objects -> String
generateXRef objects = generateXRefEntries xrefHeader 0 objects
    where 
        xrefHeader = printf "xref\n0 %d\n" (length objects)

findObject :: Objects -> ObjectNumber -> (ObjectRef, String)
findObject [] objNum = ((objNum, 65535, 0), "f")
findObject ((_, ref):objects) objNum
    | num1 == objNum = (ref, "n")
    | otherwise = findObject objects objNum
    where 
        (num1, _, _) = ref

generateXRefEntries :: String -> ObjectNumber -> Objects -> String
generateXRefEntries string objNum objects 
    | objNum >= length objects = string
    | otherwise = generateXRefEntries newString (objNum + 1) objects
    where 
        (ref, mode) = findObject objects objNum 
        (_, genNum, offset) = ref
        newString = printf "%s%010d %05d %s\n" string offset genNum mode

generateTrailer :: ObjectNumber -> ObjectRef -> ByteOffset -> String
generateTrailer objNum (rootNum, rootGenNum, _) xrefOffset = trailerString ++ startxrefString
    where 
        size = objNum
        rootString = printf "%d %d R" rootNum rootGenNum
        dict = [
                ("Size", Inline (Integer size)), 
                ("Root", Inline (String rootString))
            ]
        (dictString, _) = encodeDictionary [] "" dict
        trailerString = "trailer <<\n" ++ dictString ++ ">>\n"
        startxrefString = "startxref\n" ++ show xrefOffset ++ "\n" ++ pdfTrailer

generateObject :: Objects -> Object -> Objects
generateObject objects object = (val, ref):newObjects
    where 
        (val, newObjects) = encodeObject objects object
        ref = (length newObjects, 0, 0)

encodeInlineType :: InlineType -> String
encodeInlineType (Boolean bool) = if bool then "true" else "false"
encodeInlineType (Integer int) = show int
encodeInlineType (String string) = string
encodeInlineType (Name string) = "/" ++ string

encodeObjectRef :: ObjectRef -> String
encodeObjectRef (objNum, genNum, _) = printf "%d %d R" objNum genNum

wrapObject :: String -> ObjectNumber -> String
wrapObject objString num = printf "%d 0 %s\n%s%s\n" num "obj" objString "endobj\n"

encodeArray :: Objects -> String -> [Object] -> (String, Objects)
encodeArray objects string [] = (string, objects)
encodeArray objects string (object:objs) = encodeArray newObjects newString objs
    where
        (value, newObjects) = encodeObject objects object
        newString = string ++ "\t" ++ value ++ "\n"

encodeDictionary :: Objects -> String -> Dictionary -> (String, Objects)
encodeDictionary objects string [] = (string, objects)
encodeDictionary objects string ((name, object):objs) = encodeDictionary newObjects newString objs
    where 
        (value, newObjects) = encodeObject objects object
        newString = printf "%s\t/%s %s\n" string name value

encodeIndirectType :: Objects -> IndirectType -> Objects
encodeIndirectType objects (Array as) = arrayObject:newObjects
    where 
        (string, newObjects) = encodeArray objects "" as
        arrayString = "[\n" ++ string ++ "]\n"
        objNumber = length newObjects + 1
        arrayObject = (wrapObject arrayString objNumber, (objNumber, 0, 0))
encodeIndirectType objects (Dictionary dict) = dictObject:newObjects
    where 
        (string, newObjects) = encodeDictionary objects "" dict
        dictString = "<<\n" ++ string ++ ">>\n"
        objNumber = length newObjects + 1
        dictObject = (wrapObject dictString objNumber, (objNumber, 0, 0))
encodeIndirectType objects (Stream dict bytes) = streamObject:newObjects
    where 
        (string, newObjects) = encodeDictionary objects "" dict
        streamString = "<<\n" ++ string ++ ">>\n" ++ "stream\n" ++ bytes ++ "\n" ++ "endstream\n" 
        objNumber = length newObjects + 1
        streamObject = (wrapObject streamString objNumber, (objNumber, 0, 0))

-- | Encode an object and return the string representation for inline types 
--   or the indirect reference for indirect types, along with the new list 
--   of objects
encodeObject :: Objects -> Object -> (String, Objects)
encodeObject objects (Inline obj) = (encodeInlineType obj, objects)
encodeObject objects (Indirect obj) = (value, newObjects)
    where 
        newObjects = encodeIndirectType objects obj
        (_, ref) = head newObjects
        value = encodeObjectRef ref

pdfCreateCatalog :: Pages -> PDFTree 
pdfCreateCatalog (Pages pages) = PDFTree [
        ("Type", Inline (Name "Catalog")),
        ("Version", Inline (Name "PDF-1.7")), 
        ("Pages", Indirect (Dictionary pages)),
        ("MediaBox", Indirect (Array [
            Inline (Integer 0), 
            Inline (Integer 0), 
            Inline (Integer 595), 
            Inline (Integer 842)
        ]))
    ]

pdfCreatePageTree :: [Page] -> Pages
pdfCreatePageTree pages = Pages [
        ("Type", Inline (Name "Pages")),
        ("Kids", Indirect (Array pages)),
        ("Count", Inline (Integer (length pages)))
    ]

pdfCreatePage :: [Object] -> Dictionary -> Page
pdfCreatePage contents resources = Indirect (Dictionary [
         ("Type", Inline (Name "Page")),
         ("Contents", Indirect (Array contents)),
         ("Resources", Indirect (Dictionary resources))
    ])

data Position = Point Int Int
    deriving Show

-- | replace the x position of the first point with 
--   the x position of the second point
positionReplaceX :: Position -> Position -> Position
positionReplaceX (Point _ y) (Point x _) = Point x y

positionOffsetX :: Position -> Int -> Position
positionOffsetX (Point x y) offset = Point (x + offset) y

-- | replace the y position of the first point with 
--   the y position of the second point
positionReplaceY :: Position -> Position -> Position
positionReplaceY (Point x _) (Point _ y) = Point x y

positionOffsetY :: Position -> Int -> Position
positionOffsetY (Point x y) offset = Point x (y + offset)

type FontSize = Int
type Stretch = Float
type FontID = String
type FontPath = String

data FontAttributes = FontAttrs FontSize FontID FontPath
data Text = Text String FontAttributes Stretch Position

transformText :: (String -> String) -> Text -> Text
transformText f (Text text attr s p) = Text (f text) attr s p

pdfCreateTextObject :: Text -> Object
pdfCreateTextObject (Text text (FontAttrs fontSize fontID _) stretch (Point x y)) = Indirect (Stream dict stream)
    where 
        len = length stream
        dict = [("Length", Inline (Integer len))]

        tf = "\t/" ++ fontID ++ " " ++ show fontSize ++ " Tf\n"
        td = "\t" ++ show x ++ " " ++ show y ++ " Td\n"
        tj = "\t(" ++ text ++ ") Tj\n"
        tw = "\t" ++ show stretch ++ " Tw\n"
        stream = "BT\n" ++ tf ++ td ++ tw ++ tj ++ "ET" 

data Color = Color Int Int Int
type Width = Int
type Height = Int
data Rectangle = Rectangle Position Width Height
    deriving Show

rgb :: Color -> String
rgb (Color red green blue) = show r ++ " " ++ show g ++ " " ++ show b
    where 
        r = fromIntegral red / 255.0 :: Float
        g = fromIntegral green / 255.0 :: Float
        b = fromIntegral blue / 255.0 :: Float

pdfCreateRectangleObject :: Rectangle -> Color -> Object
pdfCreateRectangleObject (Rectangle (Point x y) width height) color = Indirect (Stream dict stream) 
    where 
        len = length stream
        dict = [("Length", Inline (Integer len))]

        re = "\t" ++ show x ++ " " ++ show y ++ " " ++ show width ++ " " ++ show height ++ " re\n"
        cStr = "\tDeviceRGB cs " ++ rgb color ++ " sc\n"
        stroke = "\tf\n"
        stream = "\tq\n" ++ re ++ cStr ++ stroke ++ "\tQ\n"
