{-# LANGUAGE OverloadedStrings #-}

import Rarecoal.Formats.FreqSum (FreqSumHeader(..), FreqSumEntry(..), parseFreqSum, printFreqSum)

import Control.Error (runScript, tryAssert, scriptIO, Script, tryJust)
import Data.Monoid ((<>))
import qualified Options.Applicative as OP
import Pipes ((>->))
import qualified Pipes.Prelude as P
import System.IO (stdin)
import System.Random (randomIO)

data MyOpts = MyOpts String Int

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "Tool for downsampling a freqSum file. If \
                    \-> expressiona column is -1, the downsampled column will also have -1. \
                    \Reads from stdin")

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.strArgument (OP.metavar "<NAME>" <> OP.help "the name of the population \
                                    \to sample from")
                <*> OP.argument OP.auto (OP.metavar "<N_AFTER>" <>
                                OP.help "the new number of haplotypes to downsample to")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts name nAfter) = runScript $ do
    (FreqSumHeader names counts, entries) <- parseFreqSum stdin
    index <- tryJust "did not find name" $ name `lookup` zip names [0..]
    let nBefore = counts !! index
    tryAssert "nBefore has to be >= nAfter" $ nBefore >= nAfter
    let newEntries = entries >-> P.mapM (downSample index nBefore nAfter)
        newCounts = take index counts ++ [nAfter] ++ drop (index + 1) counts
    printFreqSum (FreqSumHeader names newCounts, newEntries)

downSample :: Int -> Int -> Int -> FreqSumEntry -> Script FreqSumEntry
downSample pos nBefore nAfter fs = do
    tryAssert "position outside bounds" $ pos < length (fsCounts fs)
    let counts = fsCounts fs !! pos
    newFS <- if (counts <= 0) then return fs else do
        newK <- scriptIO $ sampleWithoutReplacement nBefore counts nAfter
        let newCounts = take pos (fsCounts fs) ++ [newK] ++ drop (pos + 1) (fsCounts fs)
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
