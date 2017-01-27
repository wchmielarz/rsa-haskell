import Test.QuickCheck
import Test.HUnit

import MillerRabin
import Keys

testPrimalityWithPrimeNumber :: Test
testPrimalityWithPrimeNumber = TestCase (assertEqual "for (isPrime 5)," True (isPrime 5))

testPrimalityWithCompositeNumber :: Test
testPrimalityWithCompositeNumber = TestCase (assertEqual "for (isPrime 5)," False (isPrime 36))

testModulo :: Test
testModulo  = TestCase (assertEqual "for (properMod 3 15)," 3 (properMod 3 15))

testExtendedEuclidean :: Test
testExtendedEuclidean  = TestCase (assertEqual "for (extendedEuclidean 2 5)," (-2,1) (extendedEuclidean 2 5))

main = do
  putStrLn "QuickCheck"

  putStrLn "HUnit"
  runTestTT $ TestList [testPrimalityWithPrimeNumber,
                        testPrimalityWithCompositeNumber,
                        testModulo,
                        testExtendedEuclidean]
