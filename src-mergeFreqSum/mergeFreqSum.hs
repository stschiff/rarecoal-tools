{-# LANGUAGE OverloadedStrings #-}
import OrderedZip (orderedZip)
import SequenceFormats.FreqSum (FreqSumEntry(..), FreqSumHeader(..), readFreqSumStdIn, 
    readFreqSumFile, printFreqSumStdOut)

import Control.Error (errLn)
import Control.Exception (AssertionFailed(..), throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Monoid ((<>))
import qualified Options.Applicative as OP
import Pipes ((>->), Pipe, runEffect, cat, for, yield)
import Pipes.Safe (runSafeT)
import Turtle (format, d, s, (%))

data ConditionOn = C1 | C2 | Both | NoCondition deriving (Eq)

data MyOpts = MyOpts FilePath FilePath ConditionOn Bool

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    parser = MyOpts <$> OP.argument OP.str (OP.metavar "freqSumFile1" <>
                                        OP.help "file 1, put - for stdin")
                    <*> OP.argument OP.str (OP.metavar "freqSumFile2" <> OP.help "file 2")
                    <*> OP.option (readConditionOn <$> OP.str)
                        (OP.long "conditionOn" <> OP.metavar "POP" <> OP.value NoCondition <>
                        OP.help "three options: 1 (keep only sites that are present in the first \
                        \file); 2 (keep only sites that are present in the second file), Both \
                        \(keep only sites that are present in both files). By default, no \
                        \conditioning happens, so the union of both files is output, with missing \
                        \sites flagged as missing or homRef according to the --fillHomRef option") 
                    <*> OP.switch (OP.long "fillHomRef" <>
                        OP.help "treat sites that are missing in one file as hom-ref instead of \
                        \missing")
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "merge two freqSumFiles into one.")
    readConditionOn :: String -> ConditionOn
    readConditionOn "1" = C1
    readConditionOn "2" = C2
    readConditionOn "Both" = Both
    readConditionOn _ = error "cannot parse --conditionOn value"

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts f1 f2 cond fillHomRef) = runSafeT $ do
    (FreqSumHeader names1 counts1, entries1) <- if f1 == "-" then readFreqSumStdIn else 
        readFreqSumFile f1
    (FreqSumHeader names2 counts2, entries2) <- readFreqSumFile f2
    let combinedEntries = (orderedZip comp entries1 entries2 >> return ()) >->
            freqSumCombine counts1 counts2 cond fillHomRef
        newHeader = FreqSumHeader (names1 ++ names2) (counts1 ++ counts2)
    runEffect $ combinedEntries >-> printFreqSumStdOut newHeader
  where
    comp fs1 fs2 = (fsChrom fs1, fsPos fs1) `compare` (fsChrom fs2, fsPos fs2)

freqSumCombine :: (MonadIO m) => [Int] -> [Int] -> ConditionOn -> Bool ->
    Pipe (Maybe FreqSumEntry, Maybe FreqSumEntry) FreqSumEntry m ()
freqSumCombine counts1 counts2 cond fillHomRef = for cat $ \nextPair -> do
    let n1 = length counts1
        n2 = length counts2
    let filler = if fillHomRef then Just 0 else Nothing
    case nextPair of
        (Just fs1, Nothing) -> do
            when (cond /= C2 && cond /= Both) . yield $
                fs1 {fsCounts = fsCounts fs1 ++ replicate n2 filler}
        (Nothing, Just fs2) -> do
            when (cond /= C1 && cond /= Both) . yield $
                fs2 {fsCounts = replicate n1 filler ++ fsCounts fs2}
        (Just fs1, Just fs2) -> do
            let FreqSumEntry chrom pos r1 a1 freqs1 = fs1
                FreqSumEntry _ _ r2 a2 freqs2 = fs2
            if r1 == r2 && (a1 == a2 || a1 == '.' || a2 == '.')
            then do
                let altAllele = if a1 == '.' then a2 else a1
                yield $ fs1 {fsAlt = altAllele, fsCounts = freqs1 ++ freqs2}
            else
                if r1 == a2 && a1 == r2 then do
                    liftIO . errLn $
                        format ("flipping alleles in position "%s%":"%d) chrom pos
                    yield $ fs1 {fsCounts = freqs1 ++ flipAlleles counts2 freqs2}
                else do
                    liftIO . errLn $
                        format ("position "%s%":"%d%" has inconsistent alleles. Skipping") chrom pos
        (Nothing, Nothing) -> liftIO . throwIO $ AssertionFailed "freqSumCombine: should not happen"

flipAlleles :: [Int] -> [Maybe Int] -> [Maybe Int]
flipAlleles counts alleles = zipWith flipAllele counts alleles
  where
    flipAllele n (Just x) = Just (n - x)
    flipAllele _ Nothing = Nothing
