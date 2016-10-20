import OrderedZip (orderedZip)
import Rarecoal.FreqSum (FreqSumEntry(..), FreqSumHeader(..), parseFreqSum, printFreqSum)

import Control.Error (runScript, scriptIO)
import Data.Monoid ((<>))
import qualified Options.Applicative as OP
import Pipes ((>->))
import qualified Pipes.Prelude as P
import System.IO (openFile, IOMode(..), hClose, stdin)

data MyOpts = MyOpts FilePath FilePath

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    parser = MyOpts <$> OP.argument OP.str (OP.metavar "freqSumFile1" <>
                                        OP.help "file 1, put - for stdin")
                    <*> OP.argument OP.str (OP.metavar "freqSumFile2" <> OP.help "file 2")
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "merge two freqSumFiles into one.")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts f1 f2) = runScript $ do
    h1 <- if f1 == "-" then return stdin else scriptIO $ openFile f1 ReadMode
    h2 <- scriptIO $ openFile f2 ReadMode
    (FreqSumHeader names1 counts1, entries1) <- parseFreqSum h1
    (FreqSumHeader names2 counts2, entries2) <- parseFreqSum h2
    let n1 = length names1
        n2 = length names2
    let combinedEntries = orderedZip comp entries1 entries2 >-> P.map (freqSumCombine n1 n2)
        newHeader = FreqSumHeader (names1 ++ names2) (counts1 ++ counts2)
    printFreqSum (newHeader, combinedEntries)
    scriptIO . hClose $ h1
    scriptIO . hClose $ h2
  where
    comp fs1 fs2 = (fsChrom fs1, fsPos fs1) `compare` (fsChrom fs2, fsPos fs2)

freqSumCombine :: Int -> Int -> (Maybe FreqSumEntry, Maybe FreqSumEntry) -> FreqSumEntry
freqSumCombine _ n2 (Just fs1, Nothing) = fs1 {fsCounts = fsCounts fs1 ++ replicate n2 0}
freqSumCombine n1 _ (Nothing, Just fs2) = fs2 {fsCounts = replicate n1 0 ++ fsCounts fs2}
freqSumCombine _ n2 (Just fs1, Just fs2) = 
    if fsRef fs1 == fsRef fs2 && fsAlt fs1 == fsAlt fs2
        then fs1 {fsCounts = fsCounts fs1 ++ fsCounts fs2}
        else fs1 {fsCounts = fsCounts fs1 ++ replicate n2 0}
freqSumCombine _ _ (Nothing, Nothing) = undefined