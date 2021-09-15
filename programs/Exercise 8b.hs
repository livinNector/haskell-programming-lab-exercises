import Data.Char
import System.IO

readAndWrite fileName newFileName= do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  putStrLn "The contents in the file are:\n"
  putStrLn contents
  let convertedText = map toUpper contents
  writeFile newFileName convertedText
  putStrLn "\nFile Written."
  putStrLn "The contents of the new file are:\n"
  putStrLn convertedText

main :: IO ()
main = do
  putStr "Enter the file path:"
  fileName <- getLine
  readAndWrite fileName (fileName++" converted.txt")
  
