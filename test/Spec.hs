import Test.QuickCheck
import Test.HUnit

import MillerRabin
import Keys
import RSA

testPrimalityWithPrimeNumber :: Test
testPrimalityWithPrimeNumber = TestCase (assertEqual "for (isPrime 5)," True (isPrime 5))

testPrimalityWithCompositeNumber :: Test
testPrimalityWithCompositeNumber = TestCase (assertEqual "for (isPrime 5)," False (isPrime 36))

testModulo :: Test
testModulo  = TestCase (assertEqual "for (properMod 3 15)," 3 (properMod 3 15))

testExtendedEuclidean :: Test
testExtendedEuclidean  = TestCase (assertEqual "for (extendedEuclidean 2 5)," (-2,1) (extendedEuclidean 2 5))

prop_Encrypt :: [Char] -> Bool
prop_Encrypt xs = encryptText 17009 23701 xs /= xs
    where types = xs::[Char]

prop_EncryptDecrypt :: [Char] -> Bool
prop_EncryptDecrypt xs = encryptText 3793 23701 (encryptText 17009 23701 xs) == xs
  where types = xs::[Char]

main = do
  putStrLn "\nQuickCheck"
  quickCheck $ prop_EncryptDecrypt "Test of encryption and decryption in QuickCheck. AGH UST Grzegorz Jasinski, Wojtek Chmielarz"
  quickCheck $ prop_Encrypt "Test of encryption and decryption in QuickCheck. AGH UST Grzegorz Jasinski, Wojtek Chmielarz"
  putStrLn "HUnit"
  runTestTT $ TestList [testPrimalityWithPrimeNumber,
                        testPrimalityWithCompositeNumber,
                        testModulo,
                        testExtendedEuclidean]
