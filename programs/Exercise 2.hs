{-
Function factorial(n)
    Input : n - an integer
    Output: Factorial of the given number

    if n = 0    
        return 1
    if n = 1
        return 1
    return n * factorial(n-1)
-}
factorial :: Int->Int
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x-1) 

main :: IO()
main  = do
    putStrLn "Enter a number to find it's Factorial:"
    n <- getLine
    let n1 = read n :: Int
    print $ factorial n1