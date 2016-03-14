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

    it "can parse a simple version with whitespaces" $ do
      testWith  "2.0.0 " `shouldBe` Just ver2_0_0
      testWith " 2.0.0 " `shouldBe` Just ver2_0_0
      testWith " 2.0.0"  `shouldBe` Just ver2_0_0

