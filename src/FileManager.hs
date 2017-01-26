module FileManager
  (rewriteFile
  ) where

import Data.Char

rewriteFile :: FilePath -> FilePath -> ([Char] -> [Char]) -> IO()
rewriteFile inFileName outFileName f = do
  text <- readFile inFileName
  writeFile outFileName $ f text
