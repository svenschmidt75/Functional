module IPv6Spec (spec) where

import Test.Hspec
import qualified Text.Trifecta as TF

import Lib
    ( IPAddress6 (..)
    , parseIPv6Address
    , iPv6ToDecimal
    )


spec :: Spec
spec =
    describe "parse IPv6" $ do
        it "Test 1 - with leading zeros" $ do
            -- TF.Result does not have an eq instance, so need to
            let result = TF.parseString parseIPv6Address mempty "0:0:0:0:0:ffff:ac10:fe01"
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress6 0 281473568538113
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 2 - without leading zeros" $ do
            -- TF.Result does not have an eq instance, so need to
            let result = TF.parseString parseIPv6Address mempty "ffff:ac10:fe01"
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress6 0 281473568538113
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 3 - with leading zeros" $ do
            let result = TF.parseString parseIPv6Address mempty "0:0:0:0:0:ffff:cc78:f"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress6 0 281474112159759
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 4 - with zeros within" $ do
            let result = TF.parseString parseIPv6Address mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success ip -> iPv6ToDecimal ip `shouldBe` 338288524927261089654163772891438416681
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 5 - similar to test 3, but with ::" $ do
            let result = TF.parseString parseIPv6Address mempty "FE80::0202:B3FF:FE1E:8329"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success ip -> iPv6ToDecimal ip `shouldBe` 338288524927261089654163772891438416681
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 6 - with ::" $ do
            let result = TF.parseString parseIPv6Address mempty "2001:DB8::8:800:200C:417A"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success ip -> iPv6ToDecimal ip `shouldBe` 42540766411282592856906245548098208122
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 7 - with trailing zeros" $ do
            -- TF.Result does not have an eq instance, so need to
            let result = TF.parseString parseIPv6Address mempty "1:0:0:0:0:0:0:0"
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress6 281474976710656 0
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 8 - 1::" $ do
            -- TF.Result does not have an eq instance, so need to
            let result = TF.parseString parseIPv6Address mempty "1::"
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress6 281474976710656 0
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 9" $ do
            -- TF.Result does not have an eq instance, so need to
            let result = TF.parseString parseIPv6Address mempty "2001:0db8:0a0b:12f0:0000:0000:0000:0001"
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress6 0x20010db80a0b12f0 0x1
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 10" $ do
            -- TF.Result does not have an eq instance, so need to
            let result = TF.parseString parseIPv6Address mempty "2001:db8:0:1:1:1:1:1"
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress6 0x20010db800000001 0x0001000100010001
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
