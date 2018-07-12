{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.FreqSum (FreqSumEntry(..), readFreqSumStdIn, printFreqSumStdOut, 
    FreqSumHeader(..))
import SequenceFormats.Utils (liftParsingErrors, Chrom(..))

import Control.Applicative (optional)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace)
import Data.Text (unpack, Text)
import qualified Data.Attoparsec.Text as A
-- import Debug.Trace (trace)
import Data.Version (showVersion)
import Pipes (Producer, runEffect, yield, (>->), next, cat)
import Pipes.Attoparsec (parsed)
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT)
import qualified Pipes.Text.IO as PT
import Prelude hiding (FilePath)
import qualified Text.PrettyPrint.ANSI.Leijen as PT
import Turtle hiding (stdin, cat)
import Paths_rarecoal_tools (version)

type BedEntry = (Chrom, Int, Int)
data IntervalStatus = BedBehind | FSwithin | BedAhead

argParser :: Parser (Maybe FilePath, Maybe Double, Maybe Text)
argParser = (,,) <$>
    optional (optPath "bed" 'b' "a bed file that contains the regions to be included") <*>
    optional (optDouble "missingness" 'm' "the maximum missingness allowed (0-1). Default=1") <*>
    optional (optText "sampleMissingness" 's' "an optional sample name to condition on having \
        \no missingness")

desc :: Description
desc = Description $ PT.text ("filterFreqSum version " ++ showVersion version ++
    ": a program to filter freqSum files.")

main :: IO ()
main = runSafeT $ do
    (maybeBedFile, maybeMissingness, maybeSampleMissingness) <-
        options desc argParser
    (fsHeader, fsBody) <- readFreqSumStdIn
    let fsProd = case maybeBedFile of
            Nothing -> fsBody
            Just bedFile ->
                let textProd = PT.readFile . unpack . format fp $ bedFile
                    bedProd = parsed bedFileParser textProd >>= liftParsingErrors
                in  filterThroughBed bedProd fsBody
    let missingnessFilterPipe = case maybeMissingness of
            Nothing -> cat
            Just m -> P.filter (missingnessFilter m (fshCounts fsHeader))
    let sampleMissingnessFilterPipe = case maybeSampleMissingness of
            Nothing -> cat
            Just s' ->
                let samplePos = fst . head . filter ((==s') . snd) . zip [0..] . fshNames $ fsHeader
                in  P.filter (sampleMissingnessFilter samplePos)
    runEffect $ fsProd >-> missingnessFilterPipe >-> sampleMissingnessFilterPipe >-> 
        printFreqSumStdOut fsHeader

filterThroughBed :: (Monad m) => Producer BedEntry m () -> Producer FreqSumEntry m () ->
    Producer FreqSumEntry m ()
filterThroughBed bedProd fsProd = do
    b <- lift $ next bedProd
    let (bedCurrent, bedRest) = case b of
            Left _ -> error "Bed file empty or not readable"
            Right r -> r
    f' <- lift $ next fsProd
    let (fsCurrent, fsRest) = case f' of
            Left _ -> error "FreqSum stream empty or not readable"
            Right r -> r
    go bedCurrent fsCurrent bedRest fsRest
  where
    go bedCurrent fsCurrent bedRest fsRest = do
        let recurseNextBed = do
                b <- lift $ next bedRest
                case b of
                    Left () -> return ()
                    Right (nextBed, bedRest') -> go nextBed fsCurrent bedRest' fsRest
            recurseNextFS = do
                f' <- lift $ next fsRest
                case f' of
                    Left () -> return ()
                    Right (nextFS, fsRest') -> go bedCurrent nextFS bedRest fsRest'
        case bedCurrent `checkIntervalStatus` fsCurrent of
            BedBehind -> recurseNextBed
            BedAhead -> recurseNextFS
            FSwithin -> do
                yield fsCurrent
                recurseNextFS

missingnessFilter :: Double -> [Int] -> FreqSumEntry -> Bool
missingnessFilter m hapNums fs =
    let num = sum [n | (n, freq) <- zip hapNums (fsCounts fs), freq == Nothing]
        denom = sum hapNums
    in  (fromIntegral num / fromIntegral denom) <= m

sampleMissingnessFilter :: Int -> FreqSumEntry -> Bool
sampleMissingnessFilter filterPos fs = (fsCounts fs) !! filterPos /= Nothing
    

checkIntervalStatus :: BedEntry -> FreqSumEntry -> IntervalStatus
checkIntervalStatus (bedChrom, bedStart, bedEnd) (FreqSumEntry fsChrom' fsPos' _ _ _) =
    case bedChrom `compare` fsChrom' of
        LT -> BedBehind
        GT -> BedAhead
        EQ -> if bedStart + 1 > fsPos' then
                  BedAhead
              else
                  if bedEnd < fsPos' then BedBehind else FSwithin

bedFileParser :: A.Parser BedEntry
bedFileParser = (,,) <$> chrom <* A.skipSpace <*> A.decimal <* A.skipSpace <*> A.decimal <* 
    A.endOfLine
  where
    chrom = Chrom <$> A.takeTill isSpace
