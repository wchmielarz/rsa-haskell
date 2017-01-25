module ArgumentParser
  (
    parseInput
  )
  where

import System.Environment
import System.IO.Error
import Control.Exception

parseInput = do
  (option:_) <- getArgs
  (option:inFileName:outFileName:pkKey:modulus:_) <- getArgs
  case () of _
               | option == "q" -> putStrLn "GenerateKey"
               | option == "e" -> putStrLn "Encryption/Decrtyption"
               | otherwise     -> putStrLn "Help"

exHdlr :: IOError -> IO ()
exHdlr = \ex -> if isDoesNotExistError ex
                then putStrLn "The file doesn't exist!"
                else ioError ex
{-
               main :: IO ()
               main = do
                 result <- try riskyAction
                 case result of
                   Left ex -> exHdlr ex
                   Right _ -> putStrLn "Operation completed"
-}
