module Beer (song) where

buildSong :: Int -> String 
buildSong 0 =  "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
buildSong 1 = "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n\n" ++ buildSong 0
buildSong 2 = "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n\n" ++ buildSong 1
buildSong n = show n ++ " bottles of beer on the wall, "++ show n ++" bottles of beer.\nTake one down and pass it around, " ++ show (n-1) ++ " bottles of beer on the wall.\n\n" ++ buildSong (n-1)

song :: String
song = buildSong 99
