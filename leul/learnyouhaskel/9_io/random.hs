
import System.Random

dies seed = let generator = mkStdGen seed
            in random generator :: Int