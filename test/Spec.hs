{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Parsers.SemVer2
import Data.ByteString (ByteString)
import Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _           = Nothing

main :: IO ()
main = hspec $ do
  describe "SemVer Parsing" $ do
    it "can parse a simple major, minor, patch version" $ do
      let m  = parseByteString parseSemVer mempty ("2.0.0" :: ByteString)
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (SemVer 2 0 0 [] [])

