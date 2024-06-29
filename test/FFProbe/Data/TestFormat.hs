module FFProbe.Data.TestFormat (specs) where

import Data.Aeson
import FFProbe.Data.Format
import FFProbe.Data.Tags
import Test.Hspec
import Utils
import Prelude hiding (id)

specs :: Spec
specs = describe "Format Parsing" $ do
    it "Should Parse the correct values" $ do
        rawJson <- getAssetContent "format.json"
        shouldBeRight
            (eitherDecodeStrict rawJson)
            ( \format -> do
                filename format `shouldBe` "A.mkv"
                streamsCount format `shouldBe` 5
                programsCount format `shouldBe` 0
                streamGroupsCount format `shouldBe` 0
                formatName format `shouldBe` "matroska,webm"
                formatLongName format `shouldBe` "Matroska / WebM"
                startTime format `shouldBe` 0.0
                duration format `shouldBe` 5272.96
                bitrate format `shouldBe` 5406763
                length (tags format) `shouldBe` 1
                lookupTag "ENCODER" format `shouldBe` Just (StringTag "Lavf60.16.100")
            )
