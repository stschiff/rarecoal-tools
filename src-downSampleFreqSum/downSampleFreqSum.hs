{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.FreqSum (FreqSumHeader(..), FreqSumEntry(..), readFreqSumStdIn, 
    printFreqSumStdOut)

import Control.Error (runScript, tryAssert, scriptIO, Script, tryJust)
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Version (showVersion)
import qualified Options.Applicative as OP
import Pipes ((>->), runEffect)
import qualified Pipes.Prelude as P
import System.Random (randomIO)
import Paths_rarecoal_tools (version)

data MyOpts = MyOpts String Int

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc ("downSampleFreqSum version " ++
        showVersion version ++ ": A tool for downsampling a freqSum file. If \
        \a column is -1, the downsampled column will also have -1. \
        \Reads from stdin"))

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.strArgument (OP.metavar "<NAME>" <> OP.help "the name of the population \
                                    \to sample from")
                <*> OP.argument OP.auto (OP.metavar "<N_AFTER>" <>
                                OP.help "the new number of haplotypes to downsample to")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts name nAfter) = runScript $ do
    (FreqSumHeader names counts, entries) <- readFreqSumStdIn
    index <- tryJust "did not find name" $ (pack name) `lookup` zip names [0..]
    let nBefore = counts !! index
    tryAssert "nBefore has to be >= nAfter" $ nBefore >= nAfter
    let newEntries = entries >-> P.mapM (downSample index nBefore nAfter)
        newCounts = take index counts ++ [nAfter] ++ drop (index + 1) counts
    runEffect $ newEntries >-> printFreqSumStdOut (FreqSumHeader names newCounts)

downSample :: Int -> Int -> Int -> FreqSumEntry -> Script FreqSumEntry
downSample pos nBefore nAfter fs = do
    tryAssert "position outside bounds" $ pos < length (fsCounts fs)
    newFS <- case fsCounts fs !! pos of
        Nothing -> return fs 
        Just c -> do
            newK <- scriptIO $ sampleWithoutReplacement nBefore c nAfter
            let newCounts = take pos (fsCounts fs) ++ [Just newK] ++ drop (pos + 1) (fsCounts fs)
            return fs {fsCounts = newCounts}
    return newFS

sampleWithoutReplacement :: Int -> Int -> Int -> IO Int
sampleWithoutReplacement n k howMany = go n k howMany 0
  where
    go _ _ 0 ret = return ret
    go _ 0 _ ret = return ret
    go n' k' howMany' ret = do
        val <- bernoulli $ fromIntegral k' / fromIntegral n'
        if val
        then go (n' - 1) (k' - 1) (howMany' - 1) (ret + 1)
        else go (n' - 1) k' (howMany' - 1) ret

bernoulli :: Double -> IO Bool
bernoulli p = (<p) <$> randomIO
