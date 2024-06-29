module FFProbe (
    ffprobe,
) where

import FFProbe.Exec (execFFProbe)

ffprobe :: String -> IO (Either String String)
ffprobe path = execFFProbe path
