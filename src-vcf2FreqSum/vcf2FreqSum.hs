{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.FreqSum (FreqSumEntry(..), FreqSumHeader(..), printFreqSumStdOut)
import SequenceFormats.VCF (readVCFfromStdIn, vcfToFreqSumEntry, VCFheader(..), VCFentry)
import SequenceFormats.Utils (Chrom(..))

import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import qualified Data.Text as T
import Pipes (Pipe, runEffect, (>->), cat, for, yield)
import qualified Pipes.Prelude as P
import Turtle hiding (cat, e)

main :: IO ()
main = do
    maybeNames <- options "convert a multi-sample VCF, read from stdin, into a freqSum file" 
        optParser
    run maybeNames

optParser :: Parser (Maybe [Text])
optParser = optional . fmap (T.splitOn ",") $ optText "names" 'n'
    "A comma-separated list of names to replace the VCF names with, if given."

run :: Maybe [Text] -> IO ()
run maybeNames = do
    (VCFheader _ names, vcfProd) <- readVCFfromStdIn
    newNames <- case maybeNames of
        Nothing -> return names
        Just names' -> do
            when (length names' /= length names) $ error "nr of given names must match names in VCF"
            return names'
    let fsHeader = FreqSumHeader newNames (replicate (length newNames) 2)
    lastPos <- newIORef (Chrom "", 0)
    runEffect $ vcfProd >-> processVCFentry >-> P.filterM (removeDuplicatePositions lastPos) >->
        printFreqSumStdOut fsHeader

processVCFentry :: Pipe VCFentry FreqSumEntry IO ()
processVCFentry = for cat $ \vcfEntry -> do
    case vcfToFreqSumEntry vcfEntry of
        Left e -> liftIO . err . unsafeTextToLine $ format ("skipping invalid vcfEntry "%w%
            ". Reason: "%s) vcfEntry (T.pack e)
        Right a -> yield a

removeDuplicatePositions :: IORef (Chrom, Int) -> FreqSumEntry -> IO Bool
removeDuplicatePositions lastPos (FreqSumEntry chrom pos _ _ _) = do
    (chrom', pos') <- readIORef lastPos
    let ret = if chrom' == chrom && pos' == pos then False else True
    writeIORef lastPos (chrom, pos)
    return ret

