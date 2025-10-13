import Data.List

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
  let idx = "[Result" `elemIndex` text
  print text
  print (getElement text idx)

getElement :: [[Char]] -> Maybe Int -> [Char]
getElement xs maybeIdx = 
  case maybeIdx of
    Just i -> if i >= 0 && i < length xs
              then xs !! i
              else "No Element"
    Nothing -> "No Element"
