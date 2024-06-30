module FFProbe.Data.Format where

import Data.Aeson.Types
import FFProbe.Data.Tags
import FFProbe.Utils (parseReadable)

data Format = Format
    { filename :: String,
      streamsCount :: Integer,
      programsCount :: Integer,
      streamGroupsCount :: Integer,
      formatName :: String,
      formatLongName :: String,
      startTime :: Float,
      -- | Duration in seconds
      duration :: Float,
      -- | Size of the file, in bytes
      size :: Integer,
      bitrate :: Integer,
      probeScore :: Integer,
      tags :: TagList,
      -- | The aeson object for the entire JSON received from ffprobe.
      raw :: Object
    }

instance HasTags Format where
    getTags = tags

instance FromJSON Format where
    parseJSON = withObject "Format" $ \v -> do
        let raw = v
        filename <- v .: "filename"
        streamsCount <- v .: "nb_streams"
        programsCount <- v .: "nb_programs"
        streamGroupsCount <- v .: "nb_stream_groups"
        formatName <- v .: "format_name"
        formatLongName <- v .: "format_long_name"
        startTime <- parseReadable =<< v .: "start_time"
        duration <- parseReadable =<< v .: "duration"
        size <- parseReadable =<< v .: "size"
        bitrate <- parseReadable =<< v .: "bit_rate"
        probeScore <- v .: "probe_score"
        tags <- parseTags =<< v .: "tags"
        return Format {..}
