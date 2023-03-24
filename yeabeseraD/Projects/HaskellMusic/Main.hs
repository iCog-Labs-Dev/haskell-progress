import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import Data.Foldable ( Foldable(fold) )

import System.Process ( runCommand )
import Text.Printf (printf)

type Seconds = Float
type Samples = Float
type Hz = Float
type Pulse = Float
type Semitone = Float
type Beat = Float

volume :: Float
volume = 0.5

outPutFilePath :: FilePath
outPutFilePath = "output.bin"

sampleRate :: Samples
sampleRate = 48000

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beat
bpm = 120

beatDuration :: Seconds
beatDuration = 60.0 / bpm

f :: Semitone -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Semitone -> Seconds -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map (* volume) $ zipWith3 (\x y z -> x*y*z) release attack output
    where step = (hz * 2 * pi) / sampleRate
          
          attack :: [Pulse]
          attack = map (min 1.0 ) [0.0, 0.001 ..]

          release :: [Pulse]
          release = reverse $ take (length output) attack

          output = map (sin . (*step)) [0.0 .. sampleRate * duration]

song :: [Semitone]
song = [0, 2, 4, 5, 4, 0, 2, 4, 7, 9, 7, 4, 5, 4]

wave :: [Pulse]
wave = concatMap (`note` duration) song
    where duration = 0.5

save :: FilePath -> IO ()
save filepath = BL.writeFile filepath byteStrings
    where builders = map B.floatLE wave
          builder = fold builders
          byteStrings = B.toLazyByteString builder

play :: IO ()
play = do
    save outPutFilePath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outPutFilePath
    return ()


