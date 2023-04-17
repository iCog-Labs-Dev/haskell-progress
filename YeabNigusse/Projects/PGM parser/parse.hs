import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Char (isSpace)

data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Int64
}deriving(Show)
