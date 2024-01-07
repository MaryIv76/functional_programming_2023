import qualified Data.Map as Map
import Data.Function (on)
import Data.List (sortBy)
import System.IO


data Node = Leaf Char Int | InternalNode Int Node Node deriving (Show)


countFrequencies :: String -> Map.Map Char Int
countFrequencies str = foldl updateFrequency Map.empty str
  where
    updateFrequency freqMap char = Map.insertWith (+) char 1 freqMap


getFreq (Leaf _ freq) = freq
getFreq (InternalNode freq _ _) = freq

getSymbol (Leaf symbol _) = symbol


buildPriorityQueue :: String -> [Node]
buildPriorityQueue str = sortBy (compare `on` getFreq) $ map (\(char, freq) -> Leaf char freq) $ Map.toList $ countFrequencies str


buildHuffmanTree :: String -> Node
buildHuffmanTree str = buildTree $ buildPriorityQueue str
  where
    buildTree [node] = node
    buildTree (node1:node2:queue) = buildTree $ insertNode newNode queue
      where
        newNode = InternalNode (getFreq node1 + getFreq node2) node1 node2
        insertNode newNode [] = [newNode]
        insertNode newNode (x:xs)
          | getFreq newNode <= getFreq x = newNode : x : xs
          | otherwise = x : insertNode newNode xs


getHuffmanCodes :: Node -> Map.Map Char String
getHuffmanCodes tree = Map.fromList $ getCodes "" tree
  where
    getCodes code (Leaf symbol _) = [(symbol, code)]
    getCodes code (InternalNode _ left right) =
      getCodes (code ++ "0") left ++ getCodes (code ++ "1") right


encodeHuffman :: String -> Map.Map Char String -> String
encodeHuffman str huffmanCodes = concatMap (\char -> Map.findWithDefault "" char huffmanCodes) str


invertMap :: (Ord k, Ord v) => Map.Map k v -> Map.Map v k
invertMap = Map.fromList . map (\(k, v) -> (v, k)) . Map.toList

decodeHuffman :: String -> Map.Map String Char -> String
decodeHuffman encodedString huffmanCodes = decodeHuffmanHelper encodedString ""
  where
    decodeHuffmanHelper [] _ = []
    decodeHuffmanHelper (x:xs) acc
      | Map.member (acc ++ [x]) huffmanCodes = Map.findWithDefault '?' (acc ++ [x]) huffmanCodes : decodeHuffmanHelper xs ""
      | otherwise = decodeHuffmanHelper xs (acc ++ [x])


-- encodeString
encodeString :: String -> String
encodeString str = encodeHuffman str huffmanCodes
  where
    huffmanTree = buildHuffmanTree str
    huffmanCodes = getHuffmanCodes huffmanTree


-- decodeString
decodeString :: String -> Map.Map Char String -> String
decodeString str huffmanCodes = decodeHuffman str (invertMap huffmanCodes)


writeHuffmanCodesToFile :: FilePath -> Map.Map Char String -> IO ()
writeHuffmanCodesToFile filePath huffmanCodes = do
  let lines' = map (\(char, code) -> if char == '\n' then '\\' : code else char : code) (Map.toList huffmanCodes)
      contents = unlines lines'
  writeFile filePath contents

readHuffmanCodesFromFile :: FilePath -> IO (Map.Map Char String)
readHuffmanCodesFromFile filePath = do
  contents <- readFile filePath
  let lines' = lines contents
      huffmanCodes = map (\(char:code) -> if char == '\\' then ('\n', code) else (char, code)) lines'
  return (Map.fromList huffmanCodes)


readFromFile :: FilePath -> IO String
readFromFile filePath = do
  contents <- readFile filePath
  return contents

writeToFile :: FilePath -> String -> IO ()
writeToFile filePath contents = writeFile filePath contents


-- encode
encode :: FilePath -> IO ()
encode filePath = do
  str <- readFromFile filePath
  let encodedString = encodeString str
  putStrLn encodedString
  writeToFile ("encoded-" ++ filePath) encodedString
  let huffmanTree = buildHuffmanTree str
      huffmanCodes = getHuffmanCodes huffmanTree
  writeHuffmanCodesToFile "huffmanCodes.txt" huffmanCodes


-- decode
decode :: FilePath -> IO ()
decode filePath = do
  encodedString <- readFromFile filePath
  huffmanCodes <- readHuffmanCodesFromFile "huffmanCodes.txt"
  let decodedString = decodeString encodedString huffmanCodes
  putStrLn decodedString


-- encodeString "mama mila ramu"
-- decodeString (encodeString "mama mila ramu") (getHuffmanCodes (buildHuffmanTree "mama mila ramu"))

-- encode "text.txt"
-- decode "encoded-text.txt"