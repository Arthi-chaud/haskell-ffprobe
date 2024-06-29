module FFProbe (
    ffprobe,
) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.ByteString.Char8 (pack)
import FFProbe.Data.Chapter (Chapter)
import FFProbe.Data.Format (Format)
import FFProbe.Exec (execFFProbe)
import GHC.Generics (Generic)

data FFProbeData = FFProbeData
    { chapters :: [Chapter],
      format :: Format
    }
    deriving (Generic)

instance FromJSON FFProbeData

ffprobe :: String -> IO (Either String FFProbeData)
ffprobe path = do
    probeRes <- execFFProbe path
    case probeRes of
        Right rawJson -> return $ eitherDecodeStrict (pack rawJson)
        Left err -> return $ Left err
