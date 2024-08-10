module FFProbe (
    ffprobe,
    FFProbeData (..),
) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
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

-- | Runs the ffprobe coomands, and parse its output
ffprobe :: String -> IO (Either String FFProbeData)
ffprobe path = do
    probeRes <- execFFProbe path
    return $ case probeRes of
        Right rawJson -> eitherDecodeStrict (encodeUtf8 $ pack rawJson)
        Left err -> Left err
