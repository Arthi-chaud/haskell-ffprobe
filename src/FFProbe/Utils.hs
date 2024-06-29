module FFProbe.Utils (parseMaybe) where

import Data.Aeson.Types (Parser)
import Text.Read (readMaybe)

-- | applies `readMaybe` to a value, and turns the result into a Parser
parseMaybe :: (Read a) => String -> Parser a
parseMaybe n = case readMaybe n of
    Nothing -> fail "Failed to read value"
    Just x -> pure x
