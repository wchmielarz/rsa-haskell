module RSA
  ( encrypt,
   decrypt
  )
  where

encrypt :: (Integer, Integer, Integer)-> Integer
encrypt (t, e, n) = t^e `mod` n

decrypt ::(Integer, Integer, Integer) -> Integer
decrypt (c, d, n) = c^d `mod` n
