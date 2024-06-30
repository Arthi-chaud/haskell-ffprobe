module FFProbe.Data.Chapter where

import Data.Aeson
import FFProbe.Data.Tags
import FFProbe.Utils (parseReadable)
import Prelude hiding (id)

data Chapter = Chapter
    { id :: Integer,
      -- | Example: "1/1000000000"
      timeBase :: String,
      -- | The timestamp, in seconds, of the start of the chapter
      startTime :: Float,
      -- | The timestamp, in seconds, of the end of the chapter
      endTime :: Float,
      -- | Additional tags
      tags :: TagList
    }

instance HasTags Chapter where
    getTags = tags

-- | Gets the duration of the chapter, in seconds
duration :: Chapter -> Float
duration chapter = endTime chapter - startTime chapter

-- | Retrieves the title of the chapter, from its tags
title :: Chapter -> Maybe String
title chapter = do
    value <- lookupTag "title" chapter
    case value of
        StringTag t -> return t
        _ -> Nothing

instance FromJSON Chapter where
    parseJSON = withObject "Chapter Entry" $ \v -> do
        id <- v .: "id"
        timeBase <- v .: "time_base"
        startTime <- parseReadable =<< v .: "start_time"
        endTime <- parseReadable =<< v .: "end_time"
        tags <- parseTags =<< v .: "tags"
        return Chapter {..}
