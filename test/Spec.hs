import qualified FFProbe.Data.TestChapter as TestChapter
import Test.Hspec

main :: IO ()
main = hspec $ do
    TestChapter.specs
