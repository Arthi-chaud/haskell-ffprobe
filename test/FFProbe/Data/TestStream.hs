module FFProbe.Data.TestStream (specs) where

import Data.Aeson
import FFProbe.Data.Stream
import FFProbe.Data.Tags (TagValue (StringTag), lookupTag)
import Test.Hspec
import Utils
import Prelude hiding (id)

specs :: Spec
specs = describe "Stream Parsing" $ do
    it "Audio Stream" $ do
        rawJson <- getAssetContent "stream-audio.json"
        shouldBeRight
            (eitherDecodeStrict rawJson)
            ( \stream -> do
                index stream `shouldBe` 1
                codecName stream `shouldBe` "pcm_s16le"
                codecLongName stream `shouldBe` "PCM signed 16-bit little-endian"
                codecType stream `shouldBe` "audio"
                streamType stream `shouldBe` AudioStream
                codecTagString stream `shouldBe` "[0][0][0][0]"
                codecTag stream `shouldBe` "0x0000"
                sampleFmt stream `shouldBe` Just "s16"
                sampleRate stream `shouldBe` Just 48000
                channels stream `shouldBe` Just 2
                bitsPerSample stream `shouldBe` Just 16
                rFrameRate stream `shouldBe` "0/0"
                averageFrameRate stream `shouldBe` "0/0"
                rFrameRate stream `shouldBe` "0/0"
                timeBase stream `shouldBe` "1/1000"
                startPts stream `shouldBe` 0
                startTime stream `shouldBe` 0.0
                bitRate stream `shouldBe` Just 1536000
                isDefault (disposition stream) `shouldBe` True
                isForced (disposition stream) `shouldBe` False
                isNonDiegetic (disposition stream) `shouldBe` False
                lookupTag "language" stream `shouldBe` Just (StringTag "eng")
            )
    it "Video Stream" $ do
        rawJson <- getAssetContent "stream-video.json"
        shouldBeRight
            (eitherDecodeStrict rawJson)
            ( \stream -> do
                index stream `shouldBe` 0
                codecName stream `shouldBe` "h264"
                codecLongName stream `shouldBe` "H.264 / AVC / MPEG-4 AVC / MPEG-4 part 10"
                codecType stream `shouldBe` "video"
                streamType stream `shouldBe` VideoStream
                codecTagString stream `shouldBe` "[0][0][0][0]"
                codecTag stream `shouldBe` "0x0000"
                width stream `shouldBe` Just 720
                height stream `shouldBe` Just 576
                hasBFrames stream `shouldBe` Just 2
                sampleAspectRatio stream `shouldBe` Just "64:45"
                displayAspectRatio stream `shouldBe` Just "16:9"
                pixFmt stream `shouldBe` Just "yuv420p"
                level stream `shouldBe` Just 30
                colorRange stream `shouldBe` Just "tv"
                fieldOrder stream `shouldBe` Just "tb"
                bitsPerSample stream `shouldBe` Nothing
                rFrameRate stream `shouldBe` "25/1"
                averageFrameRate stream `shouldBe` "25/1"
                timeBase stream `shouldBe` "1/1000"
                startPts stream `shouldBe` 0
                startTime stream `shouldBe` 0.0
                bitRate stream `shouldBe` Nothing
                bitsPerRawSample stream `shouldBe` Just 8
                isDefault (disposition stream) `shouldBe` False
                lookupTag "ENCODER" stream `shouldBe` Just (StringTag "Lavc60.31.102 libx264")
            )
