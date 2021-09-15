data Tree a = Leaf| Node a (Tree a) (Tree a)
  deriving (Eq, Ord)

instance (Show a) => Show (Tree a) where
  show = treeStr 0 
    where
      treeStr _ Leaf = ""
      treeStr indent (Node n left right) = rightTree ++ thisNode ++ leftTree
        where
          rightTree = treeStr (indent + 4) right
          thisNode = replicate indent ' ' ++ show (n) ++ "\n"
          leftTree = treeStr (indent + 4) left

singleton :: a -> Tree a
singleton n = Node n Leaf Leaf

insertTo :: Ord a => Tree a -> Tree a -> Tree a
insertTo Leaf x = x
insertTo (Node n l r) (Node n1 l1 r1)
  | n1 < n = Node n (insertTo l (Node n1 l1 r1)) r
  | otherwise = Node n l (insertTo r (Node n1 l1 r1))

createTreeL :: Ord a => [a] -> Tree a
createTreeL = (foldl insertTo Leaf) . (map singleton)

createTreeR :: Ord a => [a] -> Tree a
createTreeR = (foldr (flip insertTo) Leaf) . (map singleton)

main :: IO ()
main = do
  putStrLn
    "Enter the nodes to be inserted \
    \into the tree seperated by comma:"
  input <- getLine
  let nodes = read $ "[" ++ input ++ "]" :: [Int]
      a = createTreeL nodes
      b = createTreeR nodes
  putStrLn "The Binary Search Tree created using createTreeL function :"
  print a
  putStrLn "The Binary Search Tree created using createTreeR function :"
  print b
