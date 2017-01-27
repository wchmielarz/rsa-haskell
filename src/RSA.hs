{-|
Module      : RSA
Description : Module provides encryption and decryption of text in RSA cryptosystgem
Copyright   : (c) Grzegorz Jasinski, Wojtek Chmielarz
License     : MIT
-}

module RSA
  (
   encryptText
  )
  where

import Data.Char

-- | The 'getMaximumPower' function returns the largest p, where 2^p <= number.
getMaximumPower :: Int -> Int -> Int
getMaximumPower number power = case () of _
                                            | number == 2^power    -> power
                                            | number < 2^(power+1) -> power
                                            | otherwise            -> getMaximumPower number (power + 1)

-- | The 'getPowerList' function returns list of powers. Those sum of those 2^power is equal given number.
getPowerList :: Int -> [Int]
getPowerList 0 = []
getPowerList 1 = [1]
getPowerList number = let power = getMaximumPower number 0
  in [2^power] ++ getPowerList (number-2^power)

-- | The 'doModuloTo2' function perform operation of exponentation with protection before overflow.
doModuloTo2 :: Int ->Int -> Int -> Int
doModuloTo2 number 0 modulo = number
doModuloTo2 number 1 modulo = number
doModuloTo2 number power modulo = let result = number^2 `mod` modulo
  in doModuloTo2 result (power `div` 2) modulo

-- | The 'encryptNumber' function is encrypting number with key and modulus or decryypting ciphered number with private key an modulus.
encryptNumber :: Int -> Int -> Int -> Int
encryptNumber number key modulus = let powers = getPowerList key
  in foldl (\acc x -> acc * x `mod` modulus) 1 ( map (\p -> doModuloTo2 number p modulus) powers)

-- | The 'encryptText' function is encryptin plain text with public key and modulus or decrypting cipher with private key and modulus.
encryptText :: Int -> Int -> [Char] -> [Char]
encryptText key modulus text = map chr $ map (\number -> encryptNumber number key modulus) $ map ord text
