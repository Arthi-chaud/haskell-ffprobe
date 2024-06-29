module FFProbe.Data.Format where

import Data.Aeson.Types hiding (parseMaybe)
import FFProbe.Data.Tags
import FFProbe.Utils (parseMaybe)

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
      tags :: TagList
    }

instance HasTags Format where
    getTags = tags

instance FromJSON Format where
    parseJSON = withObject "Format" $ \v -> do
        filename <- v .: "filename"
        streamsCount <- v .: "nb_streams"
        programsCount <- v .: "nb_programs"
        streamGroupsCount <- v .: "nb_stream_groups"
        formatName <- v .: "format_name"
        formatLongName <- v .: "format_long_name"
        startTime <- parseMaybe =<< v .: "start_time"
        duration <- parseMaybe =<< v .: "duration"
        size <- parseMaybe =<< v .: "size"
        bitrate <- parseMaybe =<< v .: "bit_rate"
        probeScore <- v .: "probe_score"
        tags <- parseTags =<< v .: "tags"
        return Format {..}
