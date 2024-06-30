module FFProbe.Data.Stream where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text hiding (index)
import FFProbe.Data.Tags (HasTags (..), TagList, parseTags)
import FFProbe.Utils
import Prelude hiding (id)

data StreamType
    = VideoStream
    | AudioStream
    | SubtitleStream
    | DataStream
    | Attachment
    | Other String
    deriving (Eq, Show)

instance FromJSON StreamType where
    parseJSON (String "video") = return VideoStream
    parseJSON (String "audio") = return AudioStream
    parseJSON (String "subtitle") = return SubtitleStream
    parseJSON (String "data") = return DataStream
    parseJSON (String "attachment") = return Attachment
    parseJSON (String s) = return $ Other (unpack s)
    parseJSON x = return $ Other (show x)

data StreamDisposition = StreamDisposition
    { isDefault :: Bool,
      isDub :: Bool,
      isOriginal :: Bool,
      isComment :: Bool,
      isLyrics :: Bool,
      isKaraoke :: Bool,
      isForced :: Bool,
      isHearingImpaired :: Bool,
      isVisualImpaired :: Bool,
      isCleanEffects :: Bool,
      isAttachedPic :: Bool,
      isTimedThumbnails :: Bool,
      isNonDiegetic :: Bool,
      isCaptions :: Bool,
      isDescriptions :: Bool,
      isMetadata :: Bool,
      isDependent :: Bool,
      isStillImage :: Bool
    }

instance FromJSON StreamDisposition where
    parseJSON = withObject "Disposition" $ \v -> do
        let getValue key = (parseDispositionValue =<< v .: key) <|> pure False
        isDefault <- getValue "default"
        isDub <- getValue "dub"
        isOriginal <- getValue "original"
        isComment <- getValue "comment"
        isLyrics <- getValue "lyrics"
        isKaraoke <- getValue "karaoke"
        isForced <- getValue "forced"
        isHearingImpaired <- getValue "hearing_impaired"
        isVisualImpaired <- getValue "visual_impaired"
        isCleanEffects <- getValue "clean_effects"
        isAttachedPic <- getValue "attached_pic"
        isTimedThumbnails <- getValue "attached_pic"
        isNonDiegetic <- getValue "non_diegetic"
        isCaptions <- getValue "captions"
        isDescriptions <- getValue "descriptions"
        isMetadata <- getValue "metadata"
        isDependent <- getValue "dependent"
        isStillImage <- getValue "still_image"
        return StreamDisposition {..}
        where
            parseDispositionValue :: Int -> Parser Bool
            parseDispositionValue 0 = return False
            parseDispositionValue 1 = return True
            parseDispositionValue n = fail $ "Expected 0 or 1. Got " ++ show n

data Stream = Stream
    { index :: Integer,
      codecName :: String,
      codecLongName :: String,
      codecType :: String,
      streamType :: StreamType,
      codecTagString :: String,
      -- | Example: "0x0000"
      codecTag :: String,
      rFrameRate :: String,
      averageFrameRate :: String,
      timeBase :: String,
      startPts :: Integer,
      startTime :: Float,
      -- | Duration of the stream, in seconds
      duration :: Maybe Float,
      bitRate :: Maybe Integer,
      bitsPerRawSample :: Maybe Integer,
      bitsPerSample :: Maybe Integer,
      framesCount :: Maybe Integer,
      tags :: TagList,
      disposition :: StreamDisposition,
      fieldOrder :: Maybe String,
      profile :: Maybe String,
      width :: Maybe Integer,
      height :: Maybe Integer,
      hasBFrames :: Maybe Integer,
      -- TODO RATIO
      sampleAspectRatio :: Maybe String,
      displayAspectRatio :: Maybe String,
      pixFmt :: Maybe String,
      level :: Maybe Integer,
      colorRange :: Maybe String,
      colorSpace :: Maybe String,
      sampleFmt :: Maybe String,
      sampleRate :: Maybe Integer,
      channels :: Maybe Integer,
      channelLayout :: Maybe String,
      -- | The aeson object for the entire JSON received from ffprobe.
      raw :: Object
    }

instance HasTags Stream where
    getTags = tags

instance FromJSON Stream where
    parseJSON = withObject "Stream" $ \o -> do
        let raw = o
        index <- o .: "index"
        codecName <- o .: "codec_name"
        codecLongName <- o .: "codec_long_name"
        codecType <- o .: "codec_type"
        streamType <- parseJSON (String $ pack codecType)
        codecTagString <- o .: "codec_tag_string"
        codecTag <- o .: "codec_tag"
        rFrameRate <- o .: "r_frame_rate"
        averageFrameRate <- o .: "avg_frame_rate"
        timeBase <- o .: "time_base"
        startPts <- o .: "start_pts"
        startTime <- parseReadable =<< o .: "start_time"
        duration <- parseOptionalValue =<< o .:? "duration"
        bitRate <- parseOptionalValue =<< o .:? "bit_rate"
        bitsPerRawSample <- parseOptionalValue =<< o .:? "bits_per_raw_sample"
        bitsPerSample <- o .:? "bits_per_sample"
        framesCount <- o .:? "nb_frames"
        tags <- parseTags =<< o .: "tags"
        disposition <- o .: "disposition"
        fieldOrder <- o .:? "field_order"
        profile <- o .:? "profile"
        width <- o .:? "width"
        height <- o .:? "height"
        hasBFrames <- o .:? "has_b_frames"
        sampleAspectRatio <- o .:? "sample_aspect_ratio"
        displayAspectRatio <- o .:? "display_aspect_ratio"
        pixFmt <- o .:? "pix_fmt"
        level <- o .:? "level"
        colorRange <- o .:? "color_range"
        colorSpace <- o .:? "color_space"
        sampleFmt <- o .:? "sample_fmt"
        sampleRate <- parseOptionalValue =<< o .:? "sample_rate"
        channels <- o .:? "channels"
        channelLayout <- o .:? "channel_layout"
        return Stream {..}
