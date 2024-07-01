# haskell-ffprobe

This package provides Haskell bindings for the `ffprobe` command.

## Example

```haskell
import FFProbe
import FFProbe.Data.Format (duration, formatName)
import FFProbe.Data.Stream (codecLongName)
import System.Environment

main :: IO ()
main = do
    fileName:_ <- getArgs
    ffprobeRes <- ffprobe fileName
    case ffprobeRes of
        Left err -> putStrLn $ "An error occured: " ++ err
        Right ffprobeData -> do
            print $ formatName (format ffprobeData)
            print $ duration (format ffprobeData)
            print $ length (chapters ffprobeData)
            print $ codecLongName $ head (streams ffprobeData)
```
