import Data.List
import Data.List.Split
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (unfoldr)

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
        print (gameToString (splitMoves moves))
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
getMoves xs = unfoldr nextMove 1
  where
    nextMove :: Int -> Maybe ([String], Int)
    nextMove n = do
      idx <- getMoveIdx xs n
      let move = getMove xs idx
      return (move, n + 1)

getMove :: [String] -> Int -> [String]
getMove xs idx =
  case drop (idx + 1) xs of
    (first:second:_) ->
      let result = ["1-0", "0-1", "1/2-1/2"]
      in first : if second `elem` result then [] else [second]
    (first:_) -> [first]
    [] -> []


getMoveIdx :: [String] -> Int -> Maybe Int
getMoveIdx xs n = elemIndex (getPlace n) xs

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
splitMoves moves = concatMap (map splitMove) moves

splitMove :: String -> (Int, [String])
splitMove move =
  let parts = splitOneOf "-x" move
  in (length parts - 1, splitIntoMoves parts)

splitIntoMoves :: [String] -> [String]
splitIntoMoves xs = concatMap (\(a, b) -> [a, b]) (zip xs (tail xs))

-- converts game to string
moveToString :: (Int, [String]) -> String
moveToString (n, parts) = intercalate "," (show n : parts)

gameToString :: [(Int, [String])] -> String
gameToString moves = intercalate "," (map moveToString moves)

