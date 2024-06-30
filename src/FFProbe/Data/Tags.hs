module FFProbe.Data.Tags where

import Data.Aeson
import Data.Aeson.Key (toString)
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Scientific (floatingOrInteger)
import Data.Text (unpack)

type TagList = [(String, TagValue)]

data TagValue
    = StringTag String
    | IntTag Integer
    | FloatTag Float
    | -- | If the constructor is 'Other', the String is a JSON representation of the value
      Other String
    | Null
    deriving (Show, Eq)

parseTags :: Value -> Parser TagList
parseTags = withObject "Tags" $ \kvmap -> do
    pure $ map (bimap toString parseTag) (toList kvmap)
    where
        parseTag (String v) = StringTag $ unpack v
        parseTag x = Other $ show x

instance FromJSON TagValue where
    parseJSON (String v) = return $ StringTag $ unpack v
    parseJSON (Number v) = return $ either FloatTag IntTag (floatingOrInteger v)
    parseJSON Data.Aeson.Null = return FFProbe.Data.Tags.Null
    parseJSON x = return $ Other $ show x

class HasTags a where
    getTags :: a -> TagList

lookupTag :: (HasTags a) => String -> a -> Maybe TagValue
lookupTag key obj = lookup key (getTags obj)
