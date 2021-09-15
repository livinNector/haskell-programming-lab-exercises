reverse' :: [a] -> [a]
reverse' x = [x !! i | i <- [length x -1, length x -2 .. 0]]

createListManually :: IO ()
createListManually = do
  putStrLn "Enter the list values seperated by a comma:"
  input <- getLine
  let list = read $ "[" ++ input ++ "]" :: [Int]
  putStrLn "The Entered List is:"
  print list
  putStrLn "The reversed List is:"
  print $ reverse' list

createListUsingRange :: IO ()
createListUsingRange = do
  putStrLn "Enter the start , step and end values seperated by space:"
  input <- getLine
  let start : step : end : xs = read $ "[" ++ input ++ "]" :: [Int]
      list = [start, start + step .. end]
  putStrLn "The list created using range:"
  print list
  putStrLn "The reversed List is:"
  print $ reverse' list

main :: IO ()
main = do
  createListManually
  createListUsingRange
