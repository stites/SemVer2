{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Parsers.SemVer2
import Data.ByteString (ByteString)
import Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _           = Nothing


runSemVer :: ByteString -> Result SemVer
runSemVer s = parseByteString parseSemVer mempty (s :: ByteString)

main :: IO ()
main = hspec $ do
  let testWith = (maybeSuccess.runSemVer)
  let baseVer rs ms = SemVer 1 0 0 rs ms
  let (n1, n1z, n321)   = (NOSI 1, NOSS "1z", NOSI 321)
  let (n321a, nz, nars) = (NOSS "321a", NOSS "z", NOSS "ars")

  describe "SemVer Parsing" $ do
    it "can parse a simple major, minor, patch version" $ do
      let build a b c = SemVer a b c [] []
      testWith "2.0.0" `shouldBe` (Just $ build 2 0 0)
      testWith "2.1.0" `shouldBe` (Just $ build 2 1 0)
      testWith "4.5.6" `shouldBe` (Just $ build 4 5 6)
      testWith "4.a.6" `shouldBe` Nothing

    it "can parse a simple version with whitespaces" $ do
      let ver1_0_0 = baseVer [] []
      testWith  "1.0.0 " `shouldBe` Just ver1_0_0
      testWith " 1.0.0 " `shouldBe` Just ver1_0_0
      testWith " 1.0.0"  `shouldBe` Just ver1_0_0
      testWith "a 1.0.0" `shouldBe` Nothing

    it "can parse versions with release values" $ do
      let relVer = (flip baseVer) []
      testWith "1.0.0-1" `shouldBe` (Just $ relVer [n1])
      testWith "1.0.0-1-" `shouldBe` (Just $ relVer [NOSS "1-"])
      testWith "1.0.0-"  `shouldBe` (Just $ relVer [])
      testWith "1.0.0-1.321"  `shouldBe` (Just $ relVer [n1, n321])
      testWith "1.0.0-1.321a" `shouldBe` (Just $ relVer [n1, n321a])
      testWith "1.0.0-z.ars"  `shouldBe` (Just $ relVer [nz, nars])
      testWith "1.0.0-1z.ars" `shouldBe` (Just $ relVer [n1z, nars])
      testWith "1.0.0-1.321.z.ars"  `shouldBe` (Just $ relVer [n1,n321,nz,nars])

    it "can parse versions with metadata values" $ do
      let metaVer = baseVer []
      testWith "1.0.0+1" `shouldBe` (Just $ metaVer [n1])
      testWith "1.0.0+"  `shouldBe` (Just $ metaVer [])
      testWith "1.0.0+1.321-a"  `shouldBe` (Just $ metaVer [n1, NOSS "321-a"])
      testWith "1.0.0+1.321a" `shouldBe` (Just $ metaVer [n1, n321a])
      testWith "1.0.0+z.ars"  `shouldBe` (Just $ metaVer [nz, nars])
      testWith "1.0.0+1z.ars" `shouldBe` (Just $ metaVer [n1z, nars])
      testWith "1.0.0+1.321.z.ars"  `shouldBe` (Just $ metaVer [n1,n321,nz,nars])

    it "can parse versions with both release and metadata values" $ do
      testWith "1.0.0-1+" `shouldBe` (Just $ baseVer [n1] [])
      testWith "1.0.0-+"  `shouldBe` (Just $ baseVer [] [])
      testWith "1.0.0-+1" `shouldBe` (Just $ baseVer [] [n1])
      testWith "1.0.0-1.321+z.ars"  `shouldBe` (Just $ baseVer [n1, n321] [nz, nars])
      testWith "1.0.0-1.321a+1z" `shouldBe` (Just $ baseVer [n1, n321a] [n1z])
      testWith "1.0.0-z.ars+321"  `shouldBe` (Just $ baseVer [nz, nars] [n321])
      testWith "1.0.0-1.z+1.z" `shouldBe` (Just $ baseVer [n1, nz] [n1, nz])

