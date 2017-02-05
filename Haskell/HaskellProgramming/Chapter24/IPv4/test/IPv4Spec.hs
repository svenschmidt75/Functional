module IPv4Spec (spec) where

import Test.Hspec
import qualified Text.Trifecta as TF

import Lib


spec :: Spec
spec =
    describe "parse IPv4" $ do
        it "Test 1" $ do
            let result = TF.parseString parseIPv4Address mempty "172.16.254.1"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress 2886794753
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "Test 2" $ do
            let result = TF.parseString parseIPv4Address mempty "204.120.0.15"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success ip -> ip `shouldBe` IPAddress 3430416399
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "4 digits in 1st octet - fail" $ do
            let result = TF.parseString parseIPv4Address mempty "2104.120.0.15"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _ -> expectationFailure "parse should have failed!"
                TF.Failure _ -> return ()
        it "4 digits in 2nd octet - fail" $ do
            let result = TF.parseString parseIPv4Address mempty "210.1120.0.15"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _ -> expectationFailure "parse should have failed!"
                TF.Failure _ -> return ()
        it "4 digits in 3rd octet - fail" $ do
            let result = TF.parseString parseIPv4Address mempty "210.120.1110.15"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _ -> expectationFailure "parse should have failed!"
                TF.Failure _ -> return ()
        it "4 digits in 4th octet - fail" $ do
            let result = TF.parseString parseIPv4Address mempty "210.120.111.1115"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _ -> expectationFailure "parse should have failed!"
                TF.Failure _ -> return ()
