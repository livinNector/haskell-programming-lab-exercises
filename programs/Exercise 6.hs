import Data.List (permutations)
insertionSort :: Ord a => [a]->[a]
insertionSort [] = []
insertionSort (x:xs) = insert x $ insertionSort xs
  where
  insert x [] = [x]
  insert x (y:ys)
    | x<=y = x:y:ys
    |otherwise  = y: insert x ys

bubbleSort :: Ord a => [a]->[a]
bubbleSort [] = []
bubbleSort xs = bubbleSort rem ++ big
  where 
    rem = init prevPass
    big = [last prevPass]
    prevPass = bubbleUp xs
    bubbleUp [x] = [x]
    bubbleUp (x:y:ys) = small : small : bubbleUp (big : ys)
      where
        (small, big) = if x>y then (y, x) else (x, y)  

selectionSort :: Ord a => [a]->[a]
selectionSort [] = []
selectionSort xs = small : selectionSort rem
  where
    small = minimum xs
    rem = removeElement small xs
    removeElement x (y:ys)
      | y==x = ys
      |otherwise = y: removeElement x ys

permutationSort :: Ord a => [a]->[a]
permutationSort xs = head [p | p <- permutations xs, isSorted p]
    where
      isSorted x = and  $ zipWith (<=) x (tail x)


mergeSort :: Ord a => [a]->[a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge left right
  where
    left = mergeSort $ take half xs
    right = mergeSort $ drop half xs
    half = length xs `div` 2 
    merge [] xs = xs
    merge xs [] = xs
    merge (l:ls) (r:rs) 
      | l<r = l: merge ls (r:rs)
      |otherwise = r: merge (l:ls) rs

quickSort :: Ord a => [a]->[a]
quickSort [] = []
quickSort (pivot:xs) = left ++ [pivot]++ right
  where 
    left = quickSort [x | x<- xs, x<=pivot] 
    right = quickSort [x | x<- xs, x>pivot]

testAllSorts :: (Ord a, Show a)=> [[a]] -> IO ()
testAllSorts lists = do
  putStrLn "Original Lists:\n"
  print lists
  putStrLn "\nSorted Lists (Using Insertion Sort):\n"
  print $ map insertionSort lists
  putStrLn "\nSorted Lists (Using Bubble Sort):\n"
  print $ map bubbleSort lists
  putStrLn "\nSorted Lists (Using Selection Sort):\n"
  print $ map selectionSort lists
  putStrLn "\nSorted Lists (Using Permutation Sort):\n"
  print $ map permutationSort lists
  putStrLn "\nSorted Lists (Using Merge Sort):\n"
  print $ map mergeSort lists
  putStrLn "\nSorted Lists (Using Quick Sort):\n"
  print $ map quickSort lists

main :: IO ()
main = do
  testAllSorts [
    [6,5,4,34,32,6,2],
    [23,16,32],
    [28,15],
    [46],
    [] ::[Int]]
  