module Primes
  (
    getRSAKeyPairs
  )
  where

import System.Random
import MillerRabin

randomInteger :: Integer -> Integer -> IO Integer
randomInteger m n = randomRIO (m, n)

properMod :: Integer -> Integer -> Integer
properMod a b
    | a >= 0 = a `mod` b
    | otherwise = properMod (a + b) b

getRandomPrime :: Integer -> Integer -> IO Integer
getRandomPrime m n =
    do
        x <- randomInteger m n
        if isPrime x
            then return x
            else getRandomPrime m n


extendedEuclidean :: Integer -> Integer -> (Integer,Integer)
extendedEuclidean 0 b = (0, 1)
extendedEuclidean a b = let (s, t) = extendedEuclidean (b `mod` a) a
                        in (t - (b `div` a) * s, s)

-- Gets the private exponent for RSA using public exponent e and seed primes p,q
getPrivateExp :: Integer -> Integer -> Integer -> IO Integer
getPrivateExp e p q =
        return (properMod (fst (extendedEuclidean e ((p - 1) * (q - 1))))
          ((p - 1) * (q - 1)))

getPublicExp :: Integer -> Integer -> IO Integer
getPublicExp p q =
    do
        e <- randomInteger 2 ((p - 1) * (q - 1) - 1)
        if gcd e ((p - 1) * (q - 1)) == 1
            then return e
            else getPublicExp p q

getRSAKeyPairs :: IO ((Integer, Integer), (Integer, Integer))
getRSAKeyPairs =
    do
        p <- getRandomPrime 2 255
        q <- getRandomPrime 2 255
        e <- getPublicExp p q
        d <- getPrivateExp e p q
        return ((e, (p * q)), (d, (p * q)))
