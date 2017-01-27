{-|
Module      : MillerRabin
Description : Module is a wrapper for all functions required for proper MillerRabin primality test.
Copyright   : (c) Grzegorz Jasinski, Wojtek Chmielarz
License     : MIT

Module provides service for checking if given numbers is prime.
-}
module MillerRabin
  (
    isPrime
  )
  where

import System.Random
import System.IO.Unsafe

-- |The 'isPrime' function is a wrapper for Miller-Rabin
isPrime :: Integer -> Bool
isPrime n = unsafePerformIO (testMillerRabin 100 n)

-- |The 'testMillerRabin' tests primality of numer using Miller-Rabin Algoritm
testMillerRabin :: Int -> Integer -> IO Bool
testMillerRabin k n
   | even n    = return (n == 2)
   | otherwise = do ws <- witnesses k n
                    return $ and [test n (pred n) evens (head odds) a | a <- ws]
  where
    (evens,odds) = span even (iterate (`div` 2) (pred n))

-- |The 'test' tests if number is in given sets
test :: Integral nat => nat -> nat -> [nat] -> nat -> nat -> Bool
test n n_1 evens d a = x `elem` [1,n_1] || n_1 `elem` powers
  where
    x = powerMod n a d
    powers = map (powerMod n a) evens

-- |The 'witnesses' returns witnesses for numer which are sufficient to check
witnesses :: (Num a, Ord a, Random a) => Int -> a -> IO [a]
witnesses k n
  | n < 2047                = return [2]
  | n < 1373653             = return [2,3]
  | n < 9080191             = return [31,73]
  | n < 25326001            = return [2,3,5]
  | n < 3215031751          = return [2,3,5,7]
  | n < 4759123141          = return [2,7,61]
  | n < 1122004669633       = return [2,13,21,1662803]
  | n < 2152302898747       = return [2,3,5,6,11]
  | n < 3474749660383       = return [2,3,5,7,11,13]
  | n < 341550071728321     = return [2,3,5,7,11,13,17]
  | otherwise               = do g <- newStdGen
                                 return $ take k (randomRs (2, n - 1) g)

-- |The 'powerMod' returns x which satisfy x^n `mod` m
powerMod :: Integral nat => nat -> nat -> nat -> nat
powerMod m x n  = f (n - 1) x x `rem` m
  where
  f d a y = if d==0 then y else g d a y
  g i b y | even i    = g (i `quot` 2) (b*b `rem` m) y
          | otherwise = f (i-1) b (b*y `rem` m)
