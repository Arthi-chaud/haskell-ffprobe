module FFProbe.TestFFProbe where

import FFProbe
import Test.Hspec
import Utils (shouldBeRight)
import FFProbe.Data.Stream
import FFProbe.Data.Chapter

specs :: Spec
specs = describe "Running ffprobe" $ do
    it "Should Run ffprobe and give the correct values" $ do
        res <- ffprobe "test/assets/test.mp4"
        shouldBeRight
            res
            ( \ffprobeData -> do
                -- Testing streams
                length (streams ffprobeData) `shouldBe` 3
                let videoStream = filter isVideoStream (streams ffprobeData)
                length videoStream `shouldBe` 1
                let audioStream = filter isAudioStream (streams ffprobeData)
                length audioStream `shouldBe` 1
                -- Testing Chapters
                length (chapters ffprobeData) `shouldBe` 3
                title (head (chapters ffprobeData)) `shouldBe` Just "Start"
                title (chapters ffprobeData !! 1) `shouldBe` Just "Middle"
                title (chapters ffprobeData !! 2) `shouldBe` Just "End"
            )