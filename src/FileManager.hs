{-|
Module      : FileManager
Description : Module provides file service
Copyright   : (c) Grzegorz Jasinski, Wojtek Chmielarz
License     : MIT

Module provides file service.
-}
module FileManager
  (
    rewriteFile
  ) where

import Data.Char

-- | The 'rewriteFile' is function which perform reading from file, then converting data with given function and rewrite new data to second file.
rewriteFile :: FilePath -> FilePath -> ([Char] -> [Char]) -> IO()
rewriteFile inFileName outFileName f = do
  text <- readFile inFileName
  writeFile outFileName $ f text
