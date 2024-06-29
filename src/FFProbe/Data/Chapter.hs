module FFProbe.Data.Chapter where
import FFProbe.Data.Misc


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

