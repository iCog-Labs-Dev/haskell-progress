import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder as B
import Data.Foldable
wave :: [Float]
wave = map sin [0.0 .. 48000]