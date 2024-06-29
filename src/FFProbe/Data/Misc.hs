module FFProbe.Data.Misc where

import Data.Aeson
import Data.Aeson.Key (toString)
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (unpack)

type TagList = [(String, TagValue)]

data TagValue
    = StringTag String
    | IntTag Integer
    | FloatTag Float
    | -- | If the constructor is 'Other', the String is a JSON representation of the value
      Other String

parseTags :: Value -> Parser TagList
parseTags = withObject "Tags" $ \kvmap -> do
    pure $ map (bimap toString parseTag) (toList kvmap)
    where
        parseTag (String v) = StringTag $ unpack v
        parseTag x = Other $ show x

instance FromJSON TagValue where
    parseJSON (String v) = return $ StringTag $ unpack v
    parseJSON x = return $ Other $ show x
