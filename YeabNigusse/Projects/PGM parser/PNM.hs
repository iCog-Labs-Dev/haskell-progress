module PNM where
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap{
       grayWidth :: Int,
       grayHight :: Int,
       grayMax :: Int,
       grayData :: L.ByteString
}deriving(Eq)

instance Show Greymap where
     show  (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 = undefined

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
               | prefix `L8.isPrefixOf` str = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
               | otherwise = Nothing


getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
                  Nothing -> Nothing
                  Just (num,rest)
                        | num <= 0 -> Nothing
                        | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count = fromIntegral n 
                     both@(prefix,_) = L.splitAt count str
                    in if L.length prefix < count
                                     then Nothing
                                     else Just both