module FFProbe.Utils (parseReadable, parseOptionalValue) where

import Data.Aeson.Types (Parser)
import Text.Read (readMaybe)

-- | applies `readMaybe` to a value, and turns the result into a Parser
parseReadable :: (Read a) => String -> Parser a
parseReadable n = case readMaybe n of
    Nothing -> fail "Failed to read value"
    Just x -> pure x

-- | applies `readMaybe` to a value, and turns the result into a Parser
parseOptionalValue :: (Read a) => Maybe String -> Parser (Maybe a)
parseOptionalValue Nothing = pure Nothing
parseOptionalValue (Just v) = Just <$> parseReadable v
