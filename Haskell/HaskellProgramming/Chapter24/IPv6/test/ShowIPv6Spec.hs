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
            show ip `shouldBe` "fe80::202:b3ff:fe1e:8329"
        it "Test 2" $ do
            let ip = IPAddress6 0x0 0xffffac10fe01
            show ip `shouldBe` "ffff:ac10:fe01"
        it "Test 3 - 1" $ do
            let ip = IPAddress6 0x0 0x1
            show ip `shouldBe` "1"
        it "Test 4 - 1::" $ do
            let ip = IPAddress6 0x1 0x0
            show ip `shouldBe` "1::"
        it "Test 5 - 1::1" $ do
            let ip = IPAddress6 0x1 0x1
            show ip `shouldBe` "1::1"
        it "Test 6" $ do
            let ip = IPAddress6 0x20010db80a0b12f0 0x1
            show ip `shouldBe` "2001:db8:a0b:12f0::1"
        it "Test 7" $ do
            let ip = IPAddress6 0x20010db800000001 0x0001000100010001
            show ip `shouldBe` "2001:db8:0:1:1:1:1:1"
