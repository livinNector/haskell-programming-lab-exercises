import Data.List

data Player = X | O | N deriving (Eq, Ord)

instance Show Player where
  show X = "X"
  show O = "O"
  show N = " "

type Board = [[Player]]

data GameState = GameState
  { board :: Board,
    player :: Player
  }
  deriving (Eq, Ord)

instance Show GameState where
  show (GameState board player) =
    "Player: " ++ show player ++ "\n" ++ boardStr
    where
      line = replicate 13 '-'
      boardStr = coverWith line "\n" rows
      rows = map (coverWith "|" " " . map show) board
      coverWith a s list = l ++ foldl1 ((++) . (++ c)) list ++ r
        where
          l = a ++ s; r = s ++ a; c = s ++ a ++ s

emptyState :: GameState
emptyState = GameState (replicate 3 $ replicate 3 N) O

opponent :: GameState -> Player
opponent gState
  | player gState == X = O
  | otherwise = X

type Pos = (Int, Int)

playerIn :: Pos -> GameState -> Maybe Player
playerIn (x, y) gState
  | x < n && y < n = Just ((board gState !! x) !! y)
  | otherwise = Nothing
  where
    n = length $ board gState

move :: GameState -> Pos -> GameState
move gState (x, y) = GameState newBoard (opponent gState)
  where
    newBoard = insertAt x newRow gBoard
    newRow = insertAt y currPlayer row
    (gBoard, row, currPlayer) = (board gState, gBoard !! x, opponent gState)
    insertAt i a list = take i list ++ [a] ++ drop (i + 1) list

isValidMove :: GameState -> Pos -> Bool
isValidMove gState pos = playerIn pos gState == Just N

nextStates :: GameState -> [GameState]
nextStates gState = map (move gState) nextMoves
  where
    nextMoves = [(x, y) | x <- [0 .. n -1], y <- [0 .. n -1], isValidMove gState (x, y)]

winner :: GameState -> Player
winner gState
  | any (all (== X)) winPos = X
  | any (all (== O)) winPos = O
  | otherwise = N
  where
    winPos = b ++ transpose b ++ [diag b, diag $ reverse b]
    b = board gState
    diag x = [(x !! i) !! i | i <- [0 .. length x -1]]

isFinal :: GameState -> Bool
isFinal gState = isFilled gState || winner gState /= N
  where
    isFilled gState = N  `notElem` concat (board gState)

score :: GameState -> Int
score gState
  | winner gState == player gState = 32
  | winner gState == opponent gState = -32
  | otherwise = 0

bestState :: GameState -> Int -> GameState
bestState gState depth = state
  where
    (maxScore, state) = maximum $ zip nextScores nextStates'
    nextScores = map (negamax (depth -1)) nextStates'
    nextStates' = nextStates gState
    negamax depth gState
      | depth == 0 || isFinal gState = score gState
      | otherwise = - bestScore `div` 2
      where
        bestScore = maximum nextScores
        nextScores = map (negamax (depth -1)) (nextStates gState)

getValidPos :: GameState -> IO Pos
getValidPos gState = do
  putStr "Enter a poisition to play: "
  input <- getLine
  let [x, y] = map (+ (-1)) (read ("[" ++ input ++ "]") :: [Int])
  if isValidMove gState (x, y)
    then return (x, y)
    else do
      putStrLn "Invalid Position!"
      getValidPos gState

playX :: GameState -> IO GameState
playX gState = do
  print gState
  pos <- getValidPos gState
  let newGState = move gState pos
  if isFinal newGState
    then return newGState
    else do playO newGState

plyDepth :: Int
plyDepth = 6

playO :: GameState -> IO GameState
playO gState = do
  print gState
  putStrLn "O is Thinking..."
  let newGState = bestState gState plyDepth
  if isFinal newGState
    then return newGState
    else do playX newGState

startGame :: IO ()
startGame = do
  let g = emptyState
  finalState <- playX g
  print finalState
  let winner' = winner finalState
  if winner' == N
    then putStrLn "Match Draw"
    else putStrLn $ show winner' ++ " Won the Game." ++ " Won the Game."

main :: IO ()
main = do
  startGame
