{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Parsers.SemVer2
import Data.ByteString (ByteString)
import Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _           = Nothing

ver2_0_0 :: SemVer
ver2_0_0 = SemVer 2 0 0 [] []

runSemVer :: ByteString -> Result SemVer
runSemVer s = parseByteString parseSemVer mempty (s :: ByteString)

main :: IO ()
main = hspec $ do
  let testWith = (maybeSuccess.runSemVer)
  describe "SemVer Parsing" $ do
    it "can parse a simple major, minor, patch version" $ do
      let build a b c = SemVer a b c [] []
      testWith "2.0.0" `shouldBe` (Just ver2_0_0)
      testWith "2.1.0" `shouldBe` (Just $ build 2 1 0)
      testWith "4.5.6" `shouldBe` (Just $ build 4 5 6)
      testWith "4.a.6" `shouldBe` Nothing

    it "can parse a simple version with whitespaces" $ do
      testWith  "2.0.0 " `shouldBe` Just ver2_0_0
      testWith " 2.0.0 " `shouldBe` Just ver2_0_0
      testWith " 2.0.0"  `shouldBe` Just ver2_0_0
      testWith "a 2.0.0" `shouldBe` Nothing

    it "can parse versions with release values" $ do
      let build rs = SemVer 2 0 0 rs []
      let (n1, n1z, n321)   = (NOSI 1, NOSS "1z", NOSI 321)
      let (n321a, nz, nars) = (NOSS "321a", NOSS "z", NOSS "ars")
      testWith "2.0.0-1" `shouldBe` (Just $ build [n1])
      testWith "2.0.0-"  `shouldBe` (Just $ build [])
      testWith "2.0.0-1.321"  `shouldBe` (Just $ build [n1, n321])
      testWith "2.0.0-1.321a" `shouldBe` (Just $ build [n1, n321a])
      testWith "2.0.0-z.ars"  `shouldBe` (Just $ build [nz, nars])
      testWith "2.0.0-1z.ars" `shouldBe` (Just $ build [n1z, nars])
      testWith "2.0.0-1.321.z.ars"  `shouldBe` (Just $ build [n1,n321,nz,nars])

