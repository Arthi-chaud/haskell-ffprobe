module FFProbe.Data.Tags (TagList, TagValue (..), HasTags (..), lookupTag) where

import Data.Aeson
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

instance FromJSON TagValue where
    parseJSON (String v) = return $ StringTag $ unpack v
    parseJSON (Number v) = return $ either FloatTag IntTag (floatingOrInteger v)
    parseJSON Data.Aeson.Null = return FFProbe.Data.Tags.Null
    parseJSON x = return $ Other $ show x

class HasTags a where
    getTags :: a -> TagList

-- | Lookup a tag in a TagList, using a key
lookupTag :: (HasTags a) => String -> a -> Maybe TagValue
lookupTag key obj = lookup key (getTags obj)
