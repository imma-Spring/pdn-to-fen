import Data.List
import Data.List.Split

main :: IO ()
main = do
  let str = "[Event \"German Open 2004\"]\n\
            \[Date \"2004-05-01\"]\n\
            \[Black \"Morgan, John\"]\n\
            \[White \"Springer, Leo\"]\n\
            \[Site \"Reutlingen\"]\n\
            \[Result \"1-0\"]\n\
            \1. 11-15 22-18 2. 15x22 25x18 3. 8-11 29-25 4. 10-14 25-22 5. 7-10 24-19 6.\n\
            \11-16 27-24 7. 16-20 31-27 8. 9-13 18x9 9. 5x14 19-15 10. 10x19 23x16 11. 12x19\n\
            \24x15 12. 4-8 22-18 13. 14x23 27x18 14. 2-7 26-22 15. 7-10 15-11 16. 8x15 18x11\n\
            \17. 10-14 28-24 18. 20x27 32x23 19. 6-10 22-18 20. 1-5 18x9 21. 5x14 30-26 22.\n\
            \10-15 26-22 23. 14-18 23x14 24. 15-19 1-0"
  let text = words str
  --print text
  print (getResult text)
  let moves = getMoves text
  print (moves)
  print (splitMoves moves)

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
