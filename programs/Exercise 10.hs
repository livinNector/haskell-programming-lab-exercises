import Control.Monad
import System.Console.ANSI
import System.IO
import System.Random
import System.Timeout

type Pos = (Int, Int)

data Direction = N | U | D | R | L deriving (Eq, Show)

test = do putStr "hello"

oppositeOf x = case x of
  U -> D
  D -> U
  R -> L
  L -> R

data GameState = GameState
  { snake :: [Pos],
    dir :: Direction,
    food :: Pos,
    score :: Int
  }

(w, h, x1, y1) = (20, 20, 0, 1)

(midX, midY) = (w `div` 2 + x1, h `div` 2 + y1)

center = (midX, midY)

endPoint = (0, h + y1 + 2)

delay = 100000 -- in micro seconds

-- (bullet, star) = ("\9679", "\9733")

(bullet, star) = ("O", "X")

(up, down, right, left, endKey) = ("\ESC[A", "\ESC[B", "\ESC[C", "\ESC[D", "\ESC[F")

withinRange (x, y) =
  ( (x - x2) `mod` w + x2,
    (y - y2) `mod` h + y2
  )
  where
    (x2, y2) = (x1 + 1, y1 + 1)

goto :: Pos -> IO ()
goto (x, y) = setCursorPosition y x

writeAt :: Pos -> String -> IO ()
writeAt pos str = do goto pos; putStr str

clearAt :: Pos -> IO ()
clearAt (x, y) = do goto (x, y); putStrLn " " -- write space

renderBorder = do
  forM_ [(x, y) | x <- [x1, w + x1 + 1], y <- [y1 + 1 .. y1 + h]] (`writeAt` "|")
  forM_ [(x, y) | x <- [x1 + 1 .. x1 + w], y <- [y1, h + y1 + 1]] (`writeAt` "-")

renderSnake g = forM_ (snake g) (`writeAt` bullet)

renderSnakeHead g = writeAt (head $ snake g) bullet

renderSnakeHeadOut g = writeAt (head $ snake g) star

renderFood g = writeAt (food g) "o"

initializeScore = writeAt (0, 0) "Score : 0"

updateScore g = writeAt (8, 0) (show $ score g)

generateFood :: [Pos] -> IO Pos
generateFood snake' = do
  let avaliable =
        [ (x, y) | x <- [x1 + 1 .. x1 + w], y <- [y1 + 1 .. y1 + h], (x, y) `notElem` snake'
        ]
      n = length avaliable
  pos <- (`mod` n) <$> randomIO
  return $ avaliable !! pos

keyMap :: Maybe String -> Direction
keyMap x
  | x == Just up = U
  | x == Just down = D
  | x == Just right = R
  | x == Just left = L
  | otherwise = N

getKey :: IO (Maybe String)
getKey = do
  goto endPoint
  key <- timeout delay $ sequence [getChar, getChar, getChar]
  writeAt endPoint "    "
  return key

initialize :: IO GameState
initialize = do
  clearScreen
  initializeScore
  let snake' = [(midX - x, midY) | x <- [-1 .. 1]]
  food' <- generateFood snake'
  let g = GameState snake' D food' 0
  renderBorder
  sequence_ $ [renderSnake, renderFood] <*> [g]
  return g

nextState :: Direction -> GameState -> IO GameState
nextState nextDir gState = do
  let snake' = snake gState
      currHead = head snake'
      (x, y) = currHead
      currDir = dir gState
      newDir =
        if or $ (nextDir ==) <$> [oppositeOf currDir, N]
          then currDir
          else nextDir
      newHead = withinRange $ case newDir of
        U -> (x, y -1)
        D -> (x, y + 1)
        R -> (x + 1, y)
        L -> (x -1, y)
      takenFood = newHead == food gState
      newSnake =
        if takenFood
          then newHead : endPoint : init snake'
          else newHead : init snake'
      newScore = if takenFood then score gState + 1 else score gState
  newFood <- if takenFood then generateFood newSnake else return (food gState)
  return (GameState newSnake newDir newFood newScore)

move :: GameState -> IO ()
move gState = do
  key <- getKey
  if key == Just endKey
    then do clearScreen; goto (0, 0); return ()
    else do
      newState <- nextState (keyMap key) gState
      let newSnake = snake newState
          tailTip = last $ snake gState
      clearAt tailTip
      sequence_ $ [renderSnakeHead, renderFood, updateScore] <*> [newState]
      if head newSnake `elem` tail newSnake
        then do
          renderSnakeHeadOut newState
          writeAt (midX - 5, midY) "Game Over!"
          goto endPoint
        else move newState

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hideCursor
  g <- initialize
  move g
  showCursor
