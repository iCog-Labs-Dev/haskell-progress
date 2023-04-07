module Transpose (transpose) where
import qualified Data.List as L

transpose :: [String] -> [String]
transpose lines = L.transpose lines
