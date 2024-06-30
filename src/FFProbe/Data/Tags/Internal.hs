module FFProbe.Data.Tags.Internal (parseTags) where

import Data.Aeson
import Data.Aeson.Key (toString)
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (unpack)
import FFProbe.Data.Tags

parseTags :: Value -> Parser TagList
parseTags = withObject "Tags" $ \kvmap -> do
    pure $ map (bimap toString parseTag) (toList kvmap)
    where
        parseTag (String v) = StringTag $ unpack v
        parseTag x = Other $ show x
