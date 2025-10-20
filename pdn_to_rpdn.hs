import Data.List
import Data.List.Split
import System.Environment (getArgs)
import System.Directory (doesFileExist)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fileName:_) -> do
      --print fileName
      print fileName
      exists <- doesFileExist fileName
      if exists then do
        contents <- readFile fileName
      --print text
        let games = splitOn "\n\n" contents
        let text = words (games !! 0)
        let moves = getMoves (text)
        print (getResult text)
      --print moves
      --print (moves)
      --print formated moves
        print (splitMoves moves)
      else
        print "File does not exist"
    [] -> do
      print "Please provide a fileName"

-- get score
getResult :: [String] -> String
getResult xs =
  let maybeIdx = elemIndex "[Result" xs
      idx = getIndex maybeIdx
  in
  if idx == 0
  then "Result not found"
  else
    let resultIdx = idx + 1
    in if resultIdx >= length xs
       then "Result not found"
       else cleanResult (xs !! resultIdx)

cleanResult :: String -> String
cleanResult result = substring 1 ((length result) - 3) result

-- get moves
getMoves :: [String] -> [[String]]
getMoves xs = loop 1 (getMoveIdx xs 1)
  where
    loop :: Int -> Int -> [[String]]
    loop idx cond
      | cond == 0  = []
      | otherwise  = getMove xs cond : loop (idx + 1) (getMoveIdx xs (idx + 1))

getMove :: [String] -> Int -> [String]
getMove xs idx =
  let last = (xs !! (idx + 2))
  in
    [(xs !! (idx + 1))] ++ if last == "1-0" || last == "0-1" || last == "1/2-1/2" then [] else [last]


getMoveIdx :: [String] -> Int -> Int
getMoveIdx xs n =
  let place = getPlace n
      maybeIdx = elemIndex place xs
  in getIndex maybeIdx

getPlace :: Int -> String
getPlace n = show n ++ "."

getIndex :: Maybe Int -> Int
getIndex maybeIdx =
  case maybeIdx of
    Just i  -> i
    Nothing -> 0

substring :: Int -> Int -> String -> String
substring start len str = take len (drop start str)

-- parse moves
splitMoves :: [[String]] -> [(Int, [String])]
splitMoves moves = loop 0
  where
    loop :: Int -> [(Int, [String])]
    loop idx
      | idx == length moves = []
      | otherwise = (splitMovePair (moves !! idx)) ++ (loop (idx + 1))

splitMovePair :: [String] -> [(Int, [String])]
splitMovePair moves =
  [splitMove (moves !! 0)] ++ if length moves == 2 then [splitMove (moves !! 1)] else []

splitMove :: String -> (Int, [String])
splitMove move =
  let move_ = splitOneOf "-x" move
      len = length move_
  in
    (len - 1, splitIntoMoves move_ len)

splitIntoMoves :: [String] -> Int -> [String]
splitIntoMoves xs len = loop 0
  where
    loop :: Int -> [String]
    loop idx
      | idx == (len - 1) = []
      | otherwise = (xs !! idx) : (xs !! (idx + 1)) : loop (idx + 1)
