main = do
  let a = concat $ map (\x -> [x]) [1, 1, 2]
      -- same as a = concat [[1],[1],[2]]
      b = [3, 5]
      c = [8, 13]
  putStrLn "In Monoid, Associative property is true:"
  print $ a ++ (b ++ c)
  print $ (a ++ b) ++ c
  putStrLn "\nAdding list with empty list leave list alone:"
  print $ a ++ []
  print $ [] ++ a