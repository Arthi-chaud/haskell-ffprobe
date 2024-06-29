module Utils where

import Data.ByteString
import Test.Hspec.Expectations

getAssetContent :: String -> IO ByteString
getAssetContent fp = Data.ByteString.readFile $ "test/assets/" <> fp

shouldBeJust :: Maybe a -> (a -> Expectation) -> Expectation
shouldBeJust Nothing _ = expectationFailure "Expected Just, got Nothing"
shouldBeJust (Just value) f = f value
