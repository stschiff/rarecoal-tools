{-# LANGUAGE OverloadedStrings #-}
import OrderedZip (orderedZip)
import Rarecoal.Formats.FreqSum (FreqSumEntry(..), FreqSumHeader(..), parseFreqSum, printFreqSum)

import Control.Error (runScript, scriptIO, Script, errLn, throwE)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import qualified Options.Applicative as OP
import Pipes ((>->))
import qualified Pipes.Prelude as P
import System.IO (openFile, IOMode(..), hClose, stdin)
import Turtle (format, d, (%))

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
    let combinedEntries =   orderedZip comp entries1 entries2
                        >-> P.mapM (freqSumCombine n1 n2)
                        >-> P.filter isJust
                        >-> P.map fromJust
        newHeader = FreqSumHeader (names1 ++ names2) (counts1 ++ counts2)
    printFreqSum (newHeader, combinedEntries)
    scriptIO . hClose $ h1
    scriptIO . hClose $ h2
  where
    comp fs1 fs2 = (fsChrom fs1, fsPos fs1) `compare` (fsChrom fs2, fsPos fs2)

freqSumCombine :: Int -> Int -> (Maybe FreqSumEntry, Maybe FreqSumEntry) ->
    Script (Maybe FreqSumEntry)
freqSumCombine _ n2 (Just fs1, Nothing) =
    return . Just $ fs1 {fsCounts = fsCounts fs1 ++ replicate n2 0}
freqSumCombine n1 _ (Nothing, Just fs2) =
    return . Just $ fs2 {fsCounts = replicate n1 0 ++ fsCounts fs2}
freqSumCombine _ n2 (Just fs1, Just fs2) =
    if fsRef fs1 == fsRef fs2 && fsAlt fs1 == fsAlt fs2 then
        return . Just $ fs1 {fsCounts = fsCounts fs1 ++ fsCounts fs2}
    else
        if fsRef fs1 == fsAlt fs2 && fsAlt fs1 == fsRef fs2 then do
            scriptIO . errLn $
                format ("flipping alleles in position "%d%":"%d) (fsChrom fs1)
                (fsPos fs1)
            return . Just $ fs1 {fsCounts = fsCounts fs1 ++ flipAlleles (fsCounts fs2)}
        else do
            scriptIO . errLn $
                format ("skipping position "%d%":"%d%
                " due to inconsistent alleles") (fsChrom fs1) (fsPos fs1)
            return . Just $ fs1 {fsCounts = fsCounts fs1 ++ replicate n2 0}
freqSumCombine _ _ (Nothing, Nothing) = throwE "should not happen"

flipAlleles :: [Int] -> [Int]
flipAlleles alleles = map flipAllele alleles
  where
    flipAllele 0 = 2
    flipAllele 1 = 1
    flipAllele 2 = 0
    flipAllele x = x
