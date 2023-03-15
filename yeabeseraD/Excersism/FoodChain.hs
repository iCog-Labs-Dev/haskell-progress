module FoodChain (song) where
import Data.Char (toLower)

song :: String
song = (unlines . map songStanza) [Fly, Spider, Bird, Cat, Dog, Goat, Cow, Horse]

data LivingThing =
    Fly|
    Spider|
    Bird|
    Cat|
    Dog|
    Goat|
    Cow|
    Horse
    deriving (Enum, Show)

swallowed :: LivingThing -> String
swallowed Horse ="She's dead, of course!"
swallowed Fly = "I don't know why she swallowed the fly. Perhaps she'll die.\n"
swallowed Spider = "It wriggled and jiggled and tickled inside her.\n"
swallowed Bird = "How absurd to swallow a bird!\n"
swallowed Cat =  "Imagine that, to swallow a cat!\n"
swallowed Dog =  "What a hog, to swallow a dog!\n"
swallowed Goat = "Just opened her throat and swallowed a goat!\n"
swallowed Cow =  "I don't know how she swallowed a cow!\n"

songStanza :: LivingThing -> String
songStanza Fly = oldLadySwallowed (toString Fly) ++ swallowed Fly
songStanza Horse = oldLadySwallowed (toString Horse) ++ swallowed Horse
songStanza xs = oldLadySwallowed (toString xs) ++ swallowed xs ++ recursiveSongLine xs

recursiveSongLine :: LivingThing -> String
recursiveSongLine Fly = swallowed Fly
recursiveSongLine Bird = "She swallowed the bird to catch the " ++
    unwords ("spider that" : tail (words (swallowed Spider))) ++ "\n" ++
    recursiveSongLine (pred Bird)
recursiveSongLine other = "She swallowed the "++toString other++
    " to catch the "++ toString (pred other) ++ ".\n" ++
    recursiveSongLine (pred other)


toString :: LivingThing -> String
toString xs = strXs
    where strXs = toLower (head temp) : tail temp
          temp = show xs

oldLadySwallowed :: String -> String
oldLadySwallowed livingThing = "I know an old lady who swallowed a "++livingThing++".\n"