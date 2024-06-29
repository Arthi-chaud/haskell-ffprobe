module FFProbe.Exec (execFFProbe) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import System.Process (readCreateProcess, shell)

-- | Runs ffprobes, returns the output of the command
execFFProbe :: String -> IO (Either String String)
execFFProbe path = try_ (readCreateProcess process input) <&> first show
    where
        try_ :: IO String -> IO (Either IOException String)
        try_ = try
        input = ""
        process = shell command
        command = unwords ["ffprobe", unwords ffprobeArgs, "'" ++ path ++ "'"]
        ffprobeArgs =
            [ "-v",
              "quiet",
              "-print_format",
              "json",
              "-show_format",
              "-show_streams",
              "-show_chapters"
            ]
