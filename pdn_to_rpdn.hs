import Data.List
import Data.List.Split
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (unfoldr)
import Data.Char (isSpace)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fileName:_) -> do
      print fileName
      exists <- doesFileExist fileName
      if exists
        then processFile fileName
        else putStrLn "File does not exist"
    [] -> putStrLn "Please provide a fileName"

-- PROCESS FILE
processFile :: FilePath -> IO ()
processFile fileName = do
  contents <- readFile fileName
  let games = splitOn "\n\n" contents
      nonEmptyGames = filter (not . all isSpace) games  -- skip empty games
  case nonEmptyGames of
    [] -> putStrLn "No games found in file."
    (_) -> do
      let formatedGames = processGames nonEmptyGames
      putStrLn formatedGames

processGames :: [String] -> String
processGames games =
  intercalate "\n\n" (map processGame games)

processGame :: String -> String
processGame game =
  let textWords = words (trim game)
      result = getResult textWords
      moves = getMoves textWords
  in result ++ "\n" ++ (gameToString (splitMoves moves))

-- get score
getResult :: [String] -> String
getResult xs =
  case elemIndex "[Result" xs of
    Nothing -> "Result Not Found"
    Just idx ->
      case drop (idx + 1) xs of
        (res:_) -> cleanResult res
        [] -> "Result Not Found"

cleanResult :: String -> String
cleanResult = filter (`notElem` "\"[]")

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

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


