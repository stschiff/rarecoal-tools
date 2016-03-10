import Rarecoal.FreqSum (parseFreqSum, printFreqSum)

import Control.Error (runScript, tryAssert)
import Control.Monad (forM_, msum)
import Control.Monad.Managed (runManaged, managed)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Options.Applicative as OP
import System.IO (withFile, IOMode(..))

main :: IO ()
main = OP.execParser parser >>= runWithOptions
  where
    parser = OP.info (OP.helper <*> options) (OP.progDesc "concatenates multiple freqSum files")
    options = OP.some (OP.strArgument (OP.metavar "FILES" <> OP.help "input file(s)"))

runWithOptions :: [FilePath] -> IO ()
runWithOptions fns = runManaged $ do
    handles <- sequence [managed (withFile fn ReadMode) | fn <- fns]
    liftIO . runScript $ do
        fs <- mapM parseFreqSum handles
        let (firstHeader:restHeaders) = map fst fs
        forM_ restHeaders $ \h -> tryAssert "freqSum headers not identical" $ firstHeader == h
        let newEntries = msum . map snd $ fs
        printFreqSum (firstHeader, newEntries)
