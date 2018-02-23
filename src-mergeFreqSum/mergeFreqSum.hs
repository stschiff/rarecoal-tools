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
import Pipes ((>->), Pipe, runEffect, await, yield)
import Pipes.Safe (runSafeT)
import Turtle (format, d, s, (%))

data ConditionOn = C1 | C2 | NoCondition deriving (Eq)

data MyOpts = MyOpts FilePath FilePath ConditionOn Bool

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    parser = MyOpts <$> OP.argument OP.str (OP.metavar "freqSumFile1" <>
                                        OP.help "file 1, put - for stdin")
                    <*> OP.argument OP.str (OP.metavar "freqSumFile2" <> OP.help "file 2")
                    <*> OP.option (readConditionOn <$> OP.str)
                        (OP.long "conditionOn" <> OP.metavar "POP" <> OP.value NoCondition <>
                        OP.help "keep only sites from the first (1) or second (2) input.") 
                    <*> OP.switch (OP.long "fillHomRef" <>
                        OP.help "treat sites that are missing in one file as hom-ref instead of \
                        \missing")
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "merge two freqSumFiles into one.")
    readConditionOn :: String -> ConditionOn
    readConditionOn "1" = C1
    readConditionOn "2" = C2
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
    return ()
  where
    comp fs1 fs2 = (fsChrom fs1, fsPos fs1) `compare` (fsChrom fs2, fsPos fs2)

freqSumCombine :: (MonadIO m) => [Int] -> [Int] -> ConditionOn -> Bool ->
    Pipe (Maybe FreqSumEntry, Maybe FreqSumEntry) FreqSumEntry m ()
freqSumCombine counts1 counts2 cond fillHomRef = do
    let n1 = length counts1
        n2 = length counts2
    nextPair <- await
    let filler = if fillHomRef then Just 0 else Nothing
    case nextPair of
        (Just fs1, Nothing) -> do
            when (cond /= C2) . yield $ fs1 {fsCounts = fsCounts fs1 ++ replicate n2 filler}
        (Nothing, Just fs2) -> do
            when (cond /= C1) . yield $ fs2 {fsCounts = replicate n1 filler ++ fsCounts fs2}
        (Just fs1, Just fs2) -> do
            if fsRef fs1 == fsRef fs2 && fsAlt fs1 == fsAlt fs2 then
                yield $ fs1 {fsCounts = fsCounts fs1 ++ fsCounts fs2}
            else
                if fsRef fs1 == fsAlt fs2 && fsAlt fs1 == fsRef fs2 then do
                    liftIO . errLn $
                        format ("flipping alleles in position "%s%":"%d) (fsChrom fs1) (fsPos fs1)
                    yield $ fs1 {fsCounts = fsCounts fs1 ++ flipAlleles counts2 (fsCounts fs2)}
                else do
                    liftIO . errLn $
                        format ("position "%s%":"%d%" has inconsistent alleles. Skipping")
                            (fsChrom fs1) (fsPos fs1)
        (Nothing, Nothing) -> liftIO . throwIO $ AssertionFailed "freqSumCombine: should not happen"

flipAlleles :: [Int] -> [Maybe Int] -> [Maybe Int]
flipAlleles counts alleles = zipWith flipAllele counts alleles
  where
    flipAllele n (Just x) = Just (n - x)
    flipAllele _ Nothing = Nothing
