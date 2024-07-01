module Utils (getAssetContent, shouldBeRight) where

import Data.ByteString
import Test.Hspec.Expectations

getAssetContent :: String -> IO ByteString
getAssetContent fp = Data.ByteString.readFile $ "test/assets/" <> fp

shouldBeRight :: Either String a -> (a -> Expectation) -> Expectation
shouldBeRight (Left err) _ = expectationFailure err
shouldBeRight (Right value) f = f value
