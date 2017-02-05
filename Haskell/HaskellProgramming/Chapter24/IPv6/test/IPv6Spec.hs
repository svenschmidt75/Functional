module IPv6Spec (spec) where

import Test.Hspec
import qualified Text.Trifecta as TF

import Lib


spec :: Spec
spec =
    describe "parse IPv6" $ do
        it "Test 1" $ do
            -- TF.Result does not have an eq instance, so need to
            let result = TF.parseString parseIPv6Address mempty "0:0:0:0:0:ffff:ac10:fe01"
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress6 0 281473568538113)
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 2" $ do
            let result = TF.parseString parseIPv6Address mempty "0:0:0:0:0:ffff:cc78:f"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress6 0 281474112159759
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
