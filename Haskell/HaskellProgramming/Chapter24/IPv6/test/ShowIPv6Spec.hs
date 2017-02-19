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
            show ip `shouldBe` "FE80::202:B3FF:FE1E:8329"
        it "Test 2" $ do
            let ip = IPAddress6 0x0 0xffffac10fe01
            show ip `shouldBe` "FFFF:AC10:FE01"
        it "Test 3 - 1" $ do
            let ip = IPAddress6 0x0 0x1
            show ip `shouldBe` "1"
        it "Test 4 - 1::" $ do
            let ip = IPAddress6 0x1 0x0
            show ip `shouldBe` "1::"
        it "Test 5 - 1::1" $ do
            let ip = IPAddress6 0x1 0x1
            show ip `shouldBe` "1::1"
