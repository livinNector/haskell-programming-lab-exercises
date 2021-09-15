fibonacci :: Num a => Int -> a
fibonacci n = fibbs !! n
  where 
    fibbs = 0:1: map f [2..]
    f n= fibbs !! (n-1) +fibbs !! (n-2)

main = do
  putStrLn "Enter the number of terms to be printed in the fibonacci sequence:"
  n <- fmap read getLine :: IO Int
  putStrLn $ "The Fibonacci Sequence upto "++ show n ++" terms is :"
  print $ map fibonacci [0..n-1]