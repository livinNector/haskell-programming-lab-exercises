data List a = Empty | Node a (List a)

instance Show a => Show (List a) where
  show Empty = "[]"
  show (Node a next) = "[" ++ show a ++ nextStr next
    where
      nextStr Empty = "]"
      nextStr (Node a Empty) = "," ++ show a ++ "]"
      nextStr (Node a next) = "," ++ show a ++ nextStr next

instance Semigroup (List a) where
  Empty <> x = x
  x <> Empty = x
  Node a Empty <> b = Node a b
  Node a next <> b = Node a (next <> b)

instance Monoid (List a) where
  mempty = Empty

n :: a -> List a
n x = Node x Empty

main :: IO ()
main = do
  let a = mconcat $ map n [1, 2, 2]
      b = n 3 <> n 5
      c = n 8 <> n 13
  putStrLn "In Monoid,  Associative property is True:"
  print $ a `mappend` (b `mappend` c)
  print $ (a `mappend` b) `mappend` c
  putStrLn "\nAdding list with empty list leave list alone:"
  print $ a `mappend` Empty
  print $ Empty `mappend` a
