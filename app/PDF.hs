module PDF (module PDF) where 
import Text.Printf (printf)

type Name = String
type Dictionary = [(Name, Object)]
data InlineType = Boolean Bool | Integer Int | String String | Name Name
data IndirectType = Array [Object] | Dictionary Dictionary | Stream Dictionary [Char]
data Object = Inline InlineType | Indirect IndirectType

type ObjectNumber = Int
type GenNumber = Int 
type ByteOffset = Int
type ObjectRef = (ObjectNumber, GenNumber, ByteOffset)

type Page = Object
newtype Pages = Pages Dictionary
type Catalog = Dictionary
newtype PDFTree = PDFTree Catalog

type Objects = [(String, ObjectRef)]

pdfHeader :: String
pdfHeader = "%PDF-1.7\n"

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
data Text = Text String Int Position

pdfCreateTextObject :: Text -> Object
pdfCreateTextObject (Text text fontSize (Point x y)) = Indirect (Stream dict stream)
    where 
        len = length stream
        dict = [("Length", Inline (Integer len))]
        tf = "\t/F13 " ++ show fontSize ++ " Tf\n"
        td = "\t" ++ show x ++ " " ++ show y ++ " Td\n"
        tj = "\t(" ++ text ++ ") Tj\n"
        stream = "BT\n" ++ tf ++ td ++ tj ++ "ET" 
