data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Ord)

instance (Show a) => Show (Tree a) where
  show = treeStr 0
    where
      treeStr _ Leaf = ""
      treeStr indent (Node n left right) = rightTree ++ thisNode ++ leftTree
        where
          rightTree = treeStr (indent + 4) right
          thisNode = replicate indent ' ' ++ show (n) ++ "\n"
          leftTree = treeStr (indent + 4) left

{--
Note: The Trees are printed in a left rotated manner.
      The left most node is the root and
      The tree spans from left to right
--}

singleton :: a -> Tree a
singleton n = Node n Leaf Leaf
  
length' :: Tree a -> Int
length' Leaf = 0
length' (Node _ left right) = 1 + leftLength + rightLength
  where
  leftLength = length' left
  rightLength = length' right

append :: Tree a -> Tree a -> Tree a
append Leaf tree = tree
append (Node n l r) tree
  | (length' l) < (length' r) = Node n (append l tree) r
  | otherwise = Node n l (append r tree)

createTree :: [a] -> Tree a
createTree [] = Leaf
createTree (x : xs) = Node x left right
  where
    left = createTree $ take half xs
    right = createTree $ drop half xs
    half = length xs `div` 2

createTreeL :: [a] -> Tree a
createTreeL = (foldl append Leaf) . (map singleton)

createTreeR :: [a] -> Tree a
createTreeR = (foldr (flip append) Leaf) . (map singleton)

main :: IO ()
main = do
  putStrLn "Enter the nodes to be inserted into the tree seperated by comma:"
  input <- getLine
  let 
    nodes = read $ "[" ++ input ++ "]" :: [Int]
    a = createTree nodes
    b = createTreeL nodes
    c = createTreeR nodes
  putStrLn "The Binary tree created using create tree function :"
  print a
  putStrLn "The Binary tree created using foldl :"
  print b
  putStrLn "The Binary tree created using foldr :"
  print c
