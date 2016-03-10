import Rarecoal.FreqSum (FreqSumHeader(..), FreqSumEntry(..), parseFreqSum, printFreqSum)

import Control.Error (runScript)
import qualified Options.Applicative as OP

main :: IO ()
main = do
    fns <- OP.info (OP.helper <*> options) (OP.progDesc "concatenates multiple freqSum files")
    runWithOptions fns
  where
    options = OP.many1' (OP.strArgument (OP.metavar "FILE" <> OP.help "input file(s)"))

runWithOptions :: [FilePath] -> IO ()
runWithOptions fns = runScript . runManaged $ do
    
    fs <- map parseFreqSum 