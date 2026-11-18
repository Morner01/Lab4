module Main where

import Lab4

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check name expected actual =
  if expected == actual
    then putStrLn ("OK: " ++ name)
    else error ("FAIL: " ++ name ++
               "\n expected: " ++ show expected ++
               "\n actual: " ++ show actual)

main :: IO ()
main = do
  check "factorial 0" 1 (factorial 0)
  check "factorial 5" 120 (factorial 5)

  check "fib 10" 55 (fib 10)
  check "fibFast 50" 12586269025 (fibFast 50)

  check "take 10 fibs"
    [0,1,1,2,3,5,8,13,21,34]
    (take 10 fibs)

  check "sumEvenSquares 3" 56 (sumEvenSquares 3)

  check "takeWhileRec"
    [1,4,9,16,25]
    (takeWhileRec (< 30) [x*x | x <- [1..]])

  check "take 5 primes"
    [2,3,5,7,11]
    (take 5 primes)

  check "evalRPN 1"
    (Just 20.0)
    (evalRPN ["2","3","+","4","*"])

  check "evalRPNString"  
    (Just 20.0)  
    (evalRPNString "2 3 + 4 *")