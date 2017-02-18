module ShowIPv6Spec (spec) where

import Test.Hspec

import Lib
    ( IPAddress6 (..)
    )


spec :: Spec
spec =
    describe "show IPv6" $ do
        it "Test 1 - with leading zeros" $ do
            let ip = IPAddress6 0xFE80000000000000 0x0202B3FFFE1E8329
            show ip `shouldBe` "FE80::0202:B3FF:FE1E:8329"



        --     -- TF.Result does not have an eq instance, so need to
        --     let result = TF.parseString parseIPv6Address mempty "0:0:0:0:0:ffff:ac10:fe01"
        --     -- manually unpack
        --     case result of
        --         TF.Success ip -> ip `shouldBe` IPAddress6 0 281473568538113
        --         -- fail this test...
        --         TF.Failure err -> show err `shouldBe` "False"
        -- it "Test 2 - without leading zeros" $ do
        --     -- TF.Result does not have an eq instance, so need to
        --     let result = TF.parseString parseIPv6Address mempty "ffff:ac10:fe01"
        --     -- manually unpack
        --     case result of
        --         TF.Success ip -> ip `shouldBe` IPAddress6 0 281473568538113
        --         -- fail this test...
        --         TF.Failure err -> show err `shouldBe` "False"
        -- it "Test 3 - with leading zeros" $ do
        --     let result = TF.parseString parseIPv6Address mempty "0:0:0:0:0:ffff:cc78:f"
        --     -- TF.Result does not have an eq instance, so need to
        --     -- manually unpack
        --     case result of
        --         TF.Success ip -> ip `shouldBe` IPAddress6 0 281474112159759
        --         -- fail this test...
        --         TF.Failure err -> show err `shouldBe` "False"
        -- it "Test 4 - with zeros within" $ do
        --     let result = TF.parseString parseIPv6Address mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
        --     -- TF.Result does not have an eq instance, so need to
        --     -- manually unpack
        --     case result of
        --         TF.Success ip -> iPv6ToDecimal ip `shouldBe` 338288524927261089654163772891438416681
        --         -- fail this test...
        --         TF.Failure err -> show err `shouldBe` "False"
        -- it "Test 5 - similar to test 3, but with ::" $ do
        --     let result = TF.parseString parseIPv6Address mempty "FE80::0202:B3FF:FE1E:8329"
        --     -- TF.Result does not have an eq instance, so need to
        --     -- manually unpack
        --     case result of
        --         TF.Success ip -> iPv6ToDecimal ip `shouldBe` 338288524927261089654163772891438416681
        --         -- fail this test...
        --         TF.Failure err -> show err `shouldBe` "False"
        -- it "Test 6 - with ::" $ do
        --     let result = TF.parseString parseIPv6Address mempty "2001:DB8::8:800:200C:417A"
        --     -- TF.Result does not have an eq instance, so need to
        --     -- manually unpack
        --     case result of
        --         TF.Success ip -> iPv6ToDecimal ip `shouldBe` 42540766411282592856906245548098208122
        --         -- fail this test...
        --         TF.Failure err -> show err `shouldBe` "False"
        -- it "Test 7 - with trailing zeros" $ do
        --     -- TF.Result does not have an eq instance, so need to
        --     let result = TF.parseString parseIPv6Address mempty "1:0:0:0:0:0:0:0"
        --     -- manually unpack
        --     case result of
        --         TF.Success ip -> ip `shouldBe` IPAddress6 281474976710656 0
        --         -- fail this test...
        --         TF.Failure err -> show err `shouldBe` "False"
        -- it "Test 8 - 1::" $ do
        --     -- TF.Result does not have an eq instance, so need to
        --     let result = TF.parseString parseIPv6Address mempty "1::"
        --     -- manually unpack
        --     case result of
        --         TF.Success ip -> ip `shouldBe` IPAddress6 281474976710656 0
        --         -- fail this test...
        --         TF.Failure err -> show err `shouldBe` "False"
