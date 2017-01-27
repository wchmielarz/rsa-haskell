{-|
Module      : Keys
Description : Module is a wrapper for all RSA Key generating functions.
Copyright   : (c) Grzegorz Jasinski, Wojtek Chmielarz
License     : MIT

Module provides service for generating RSA Keys.
-}
module Keys
  (
    getRSAKeyPairs,
    properMod,
    extendedEuclidean
  )
  where

import System.Random
import MillerRabin

-- | The 'randomInteger' function returns random number from given interval
randomInteger :: Integer -> Integer -> IO Integer
randomInteger m n = randomRIO (m, n)

-- | The 'properMod' function supports negative argument instead of `mod`
properMod :: Integer -> Integer -> Integer
properMod a b
    | a >= 0 = a `mod` b
    | otherwise = properMod (a + b) b

-- | The 'getRandomPrime' function returns random prime
getRandomPrime :: Integer -> Integer -> IO Integer
getRandomPrime m n =
    do
        x <- randomInteger m n
        if isPrime x
            then return x
            else getRandomPrime m n

-- | The 'extendedEuclidean' function returns tuple which satisfy requirement a*x mod b = 1
extendedEuclidean :: Integer -> Integer -> (Integer,Integer)
extendedEuclidean 0 b = (0, 1)
extendedEuclidean a b = let (s, t) = extendedEuclidean (b `mod` a) a
                        in (t - (b `div` a) * s, s)

-- | The 'getPrivateExp' function returns private exponent for RSA using public exponent e and primes p,q
getPrivateExp :: Integer -> Integer -> Integer -> IO Integer
getPrivateExp e p q =
        return (properMod (fst (extendedEuclidean e ((p - 1) * (q - 1))))
          ((p - 1) * (q - 1)))

-- | The 'getPublicExp' function returns public exponent for RSA using public primes p,q
getPublicExp :: Integer -> Integer -> IO Integer
getPublicExp p q =
    do
        e <- randomInteger 2 ((p - 1) * (q - 1) - 1)
        if gcd e ((p - 1) * (q - 1)) == 1
            then return e
            else getPublicExp p q
            
-- | The 'getRSAKeyPairs' function returns two tuples, public and private key
getRSAKeyPairs :: IO ((Integer, Integer), (Integer, Integer))
getRSAKeyPairs =
    do
        p <- getRandomPrime 2 255
        q <- getRandomPrime 2 255
        e <- getPublicExp p q
        d <- getPrivateExp e p q
        return ((e, (p * q)), (d, (p * q)))
