module FFProbe.Data.Chapter where

import Data.Aeson
import FFProbe.Data.Misc
import Text.Read (readMaybe)
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

-- | Gets the duration of the chapter, in seconds
duration :: Chapter -> Float
duration chapter = endTime chapter - startTime chapter

-- | Retrieves the title of the chapter, from its tags
title :: Chapter -> Maybe String
title chapter = do
    value <- lookup "title" (tags chapter)
    case value of
        StringTag t -> return t
        _ -> Nothing

instance FromJSON Chapter where
    parseJSON = withObject "Chapter Entry" $ \v -> do
        id <- v .: "id"
        timeBase <- v .: "time_base"
        sstartTime <- v .: "start_time"
        sendTime <- v .: "end_time"
        tags <- parseTags =<< v .: "tags"
        startTime <- parseMaybe sstartTime
        endTime <- parseMaybe sendTime
        return Chapter {..}
        where
            parseMaybe n = case readMaybe n of
                Nothing -> fail "Failed to read value to float"
                Just x -> pure x
