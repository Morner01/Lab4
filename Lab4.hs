module Lab4 where

import Data.List (foldl')
import Text.Read (readMaybe)

-- Задание 1

factorial :: Integer -> Integer
factorial n
    | n < 0 = error "factorial: n должно быть >= 0"
    | n == 0 = 1
    | otherwise = n * factorial (n - 1)

fib :: Integer -> Integer
fib n
    | n < 0 = error "fib: n должно быть >= 0"
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n - 1) + fib (n - 2)

fibFast :: Integer -> Integer
fibFast n
    | n < 0 = error "fibFast: n должно быть >= 0"
    | otherwise = go n 0 1
    where
        go 0 a _ = a
        go k a b = go (k - 1) b (a + b)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Задание 2

sumEvenSquares :: Int -> Integer
sumEvenSquares n = sum . take n . map (^2) . filter even $ [1..]

takeWhileRec :: (a -> Bool) -> [a] -> [a]
takeWhileRec _ [] = []
takeWhileRec p (x:xs)
    | p x = x : takeWhileRec p xs
    | otherwise = []

primes :: [Integer]
primes = sieve [2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

firstNPrimes :: Int -> [Integer]
firstNPrimes n = take n primes

-- Задание 3. Вариант A

evalRPN :: [String] -> Maybe Double
evalRPN = go []
    where
        go :: [Double] -> [String] -> Maybe Double
        go [x] [] = Just x
        go _ [] = Nothing
        go stack (t:ts) =
            case t of
                "+" -> binary (+)
                "-" -> binary (-)
                "*" -> binary (*)
                "/" -> binary (/)
                "^" -> binary (**)
                "sqrt" -> unary sqrt
                "neg" -> unary negate
                "sum" -> case stack of
                            [] -> Nothing
                            _  -> go [sum stack] ts
                _ -> case readMaybe t of
                        Just x -> go (x:stack) ts
                        Nothing -> Nothing
            where
                binary op = case stack of
                    (y:x:rest) -> go (op x y : rest) ts
                    _ -> Nothing
                unary op = case stack of
                    (x:rest) -> go (op x : rest) ts
                    _ -> Nothing

evalRPNString :: String -> Maybe Double
evalRPNString = evalRPN . words