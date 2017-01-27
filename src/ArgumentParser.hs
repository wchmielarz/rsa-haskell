{-|
Module      : ArgumentParser
Description : Module provides service for input arguments. It is main function.
Copyright   : (c) Grzegorz Jasinski, Wojtek Chmielarz
License     : MIT

Module provides service for input arguments. It is main function.
-}
module ArgumentParser
  (
    parseInput
  )
  where

import Data.Char
import System.Environment
import System.IO.Error
import Control.Exception
import FileManager
import Primes
import RSA

-- | The 'parseInput' function provides reading arguments and invoking functions. This is IO fucntion.
parseInput :: IO()
parseInput = do
  args <-getArgs
  case (args) of
-- Althought we don't need options, it was added to minimalise need of changing code in future in case of code addition,
    [option, inFileName, outFileName, key, modulus] -> do
      result <- try $ rewriteFile inFileName outFileName $ encryptText (getKey key) (getKey modulus)
      case result of
        Left ex -> exHdlr ex
        Right _ -> putStrLn "Operation completed"

    [option]                                        -> do
      s <- getRSAKeyPairs
      (putStrLn . showDetails) s
    _                                               -> do
      putStrLn "You put wrong arguments"

-- | This 'showDetails' function returns string description of RSA keys
showDetails :: ((Integer, Integer),(Integer,Integer)) -> String
showDetails ((a,b),(c,d)) = "Klucz publiczny:" ++ show (a,b) ++ "\n" ++ "Klucz prywatny:" ++ show (c,d)

-- | This 'exHdlr' function provides service for catched exceptions
exHdlr :: IOError -> IO ()
exHdlr = \ex -> if isDoesNotExistError ex
                then putStrLn "The file doesn't exist!"
                else ioError ex

-- | This 'getKey' function is wrapper to convertStringToInt function
getKey :: [Char] -> Int
getKey key = convertStringToInt key 0

-- | This 'convertStringToInt' function conferts string to number.
convertStringToInt :: [Char]-> Int -> Int
convertStringToInt (x:xs) acc = convertStringToInt xs (acc * 10 + digitToInt x)
convertStringToInt _ acc = acc
