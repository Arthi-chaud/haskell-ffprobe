module FFProbe.TestFFProbe (specs) where

import FFProbe
import FFProbe.Data.Chapter
import FFProbe.Data.Stream
import Test.Hspec
import Utils (shouldBeRight)

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
    it "Should Succeed at parsing unicode characters" $ do
        res <- ffprobe "test/assets/test-unicode.mp4"
        shouldBeRight
            res
            ( \ffprobeData -> do
                title (head (chapters ffprobeData)) `shouldBe` Just "Début"
            )
