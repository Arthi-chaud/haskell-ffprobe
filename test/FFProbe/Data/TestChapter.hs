module FFProbe.Data.TestChapter (specs) where

import Data.Aeson
import FFProbe.Data.Chapter
import Test.Hspec
import Utils
import Prelude hiding (id)

specs :: Spec
specs = describe "Chapter Parsing" $ do
    it "Should Parse the correct values" $ do
        rawJson <- getAssetContent "chapter.json"
        shouldBeRight
            (eitherDecodeStrict rawJson)
            ( \chapter -> do
                FFProbe.Data.Chapter.id chapter `shouldBe` 8
                timeBase chapter `shouldBe` "1/1000000000"
                startTime chapter `shouldBe` 0.0
                endTime chapter `shouldBe` 117.56
                length (tags chapter) `shouldBe` 1
                title chapter `shouldBe` Just "Chapter 01"
            )
