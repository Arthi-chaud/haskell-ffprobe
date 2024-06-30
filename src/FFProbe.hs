module FFProbe (
    ffprobe,
    FFProbeData(..)
) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.ByteString.Char8 (pack)
import FFProbe.Data.Chapter (Chapter)
import FFProbe.Data.Format (Format)
import FFProbe.Data.Stream (Stream)
import FFProbe.Exec (execFFProbe)
import GHC.Generics (Generic)

data FFProbeData = FFProbeData
    { streams :: [Stream],
      chapters :: [Chapter],
      format :: Format
    }
    deriving (Generic)

instance FromJSON FFProbeData

ffprobe :: String -> IO (Either String FFProbeData)
ffprobe path = do
    probeRes <- execFFProbe path
    return $ case probeRes of
        Right rawJson -> eitherDecodeStrict (pack rawJson)
        Left err -> Left err
