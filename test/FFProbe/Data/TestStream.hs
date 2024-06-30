module FFProbe.Data.TestStream (specs) where

import Data.Aeson
import FFProbe.Data.Stream
import FFProbe.Data.Stream (
    Stream (disposition),
    StreamDisposition (isNonDiegetic),
 )
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
