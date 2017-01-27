import Test.QuickCheck
import Test.HUnit

import MillerRabin
import Keys

foo :: Int -> (Int, Int)
foo x = (1, x)

test1 :: Test
test1 = TestCase (assertEqual "for (isPrime 5)," True (isPrime 5))

test2 :: Test
test2 = TestCase (assertEqual "for (isPrime 5)," False (isPrime 36))

test3 :: Test
test3  = TestCase (assertEqual "for (properMod 3 15)," 3 (properMod 3 15))

test4 :: Test
test4  = TestCase (assertEqual "for (extendedEuclidean 2 5)," (-2,1) (extendedEuclidean 2 5))

main = do
  putStrLn "QuickCheck"

  putStrLn "HUnit"
  runTestTT $ TestList [test1,test2,test3,test4]
