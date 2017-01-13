module RSA
  ( encrypt,
   decrypt
  )
  where

encrypt :: (Int, Int, Int)-> Int
encrypt (t, e, n) = t^e `mod` n

decrypt ::(Integer, Integer, Integer) -> Integer
decrypt (c, d, n) = c^d `mod` n
