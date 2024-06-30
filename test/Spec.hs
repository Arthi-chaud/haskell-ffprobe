import qualified FFProbe.Data.TestChapter as TestChapter
import qualified FFProbe.Data.TestFormat as TestFormat
import qualified FFProbe.Data.TestStream as TestStream
import qualified FFProbe.TestFFProbe as TestFFProbe
import Test.Hspec

main :: IO ()
main = hspec $ do
    TestChapter.specs
    TestFormat.specs
    TestStream.specs
    TestFFProbe.specs
