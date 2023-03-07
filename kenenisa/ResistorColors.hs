module ResistorColors (Color(..), value) where
import qualified Data.Map as Map

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum, Bounded,Ord)

colors :: Map.Map Color String
colors = Map.fromList [(Black, "0"),(Brown, "1"),(Red, "2"),(Orange, "3"),(Yellow, "4"),(Green, "5"),(Blue, "6"),(Violet, "7"),(Grey, "8"),(White, "9")]

value :: (Color, Color) -> Int
value (a, b) = read (Map.findWithDefault "0" a colors ++ Map.findWithDefault "0" b colors) :: Int
