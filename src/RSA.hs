module RSA
  ( encryptText
  , decryptText
  )
  where

import Data.Char


getMaximumPower :: Int -> Int -> Int
getMaximumPower number power = case () of _
                                            | number == 2^power    -> power
                                            | number < 2^(power+1) -> power
                                            | otherwise            -> getMaximumPower number (power + 1)

getPowerList :: Int -> [Int]
getPowerList 0 = []
getPowerList 1 = [1]
getPowerList number = let power = getMaximumPower number 0
  in [2^power] ++ getPowerList (number-2^power)

doModuloTo2 :: Int ->Int -> Int -> Int
doModuloTo2 number 0 modulo = number
doModuloTo2 number 1 modulo = number
doModuloTo2 number power modulo = let result = number^2 `mod` modulo
  in doModuloTo2 result (power `div` 2) modulo

encryptNumber :: Int -> Int -> Int -> Int
encryptNumber number key modulo = let powers = getPowerList key
  in foldl (\acc x -> acc * x `mod` modulo) 1 ( map (\p -> doModuloTo2 number p modulo) powers)

encryptText :: Int -> Int -> [Char] -> [Char]
encryptText key modulo text = map chr $ map (\number -> encryptNumber number key modulo) $ map ord text

-- | Test
decryptText :: Int -- ^ aaa
            -> Int -- ^ bb
            -> [Char]
            -> [Char]
decryptText key modulo text  = encryptText key modulo text
