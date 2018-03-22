{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.FreqSum (FreqSumEntry(..), readFreqSumStdIn, printFreqSumStdOut, 
    FreqSumHeader(..))
import SequenceFormats.Utils (liftParsingErrors)
import Control.Applicative (optional)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Text (unpack, Text)
import qualified Data.Attoparsec.Text as A
import Pipes (Producer, runEffect, yield, (>->), next, Pipe, cat, for)
import Pipes.Attoparsec (parsed)
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT)
import qualified Pipes.Text.IO as PT
import Prelude hiding (FilePath)
import Turtle hiding (stdin, cat)

type BedEntry = (Text, Int, Int)
data IntervalStatus = BedBehind | FSwithin | BedAhead

argParser :: Parser (Maybe FilePath, Double)
argParser = (,) <$>
    optional (optPath "bed" 'b' "a bed file that contains the regions to be included") <*>
    optDouble "missingness" 'm' "the maximum missingness allowed. Default=1.0 (no filter)"

main :: IO ()
main = do
    (maybeBedFile, missingness) <- options "script to filter a freqSum file" argParser
    let bedFilter = case maybeBedFile of
            Nothing -> cat
            Just bedFile ->
                let textProd = PT.readFile . unpack . format fp $ bedFile
                    bedProd = parsed bedFileParser textProd >>= liftParsingErrors
                in  filterThroughBed bedProd
    runSafeT $ do
        (fsHeader, fsBody) <- readFreqSumStdIn
        runEffect $ fsBody >-> bedFilter >->
            P.filter (missingnessFilter missingness (fshCounts fsHeader)) >->
            printFreqSumStdOut fsHeader

filterThroughBed :: (Monad m) => Producer BedEntry m () -> Pipe FreqSumEntry FreqSumEntry m ()
filterThroughBed bedProd = do
    b <- lift $ next bedProd
    let (bedCurrent, bedRest) = case b of
            Left _ -> error "Bed file empty or not readable"
            Right r -> r
    for cat (go bedCurrent bedRest)
  where
    go bedCurrent bedRest fsCurrent = do
        case bedCurrent `checkIntervalStatus` fsCurrent of
            BedBehind -> do
                b <- lift $ next bedRest
                case b of
                    Left () -> return ()
                    Right (nextBed, bedRest') -> go nextBed bedRest' fsCurrent
            BedAhead -> return ()
            FSwithin -> do
                yield fsCurrent                

missingnessFilter :: Double -> [Int] -> FreqSumEntry -> Bool
missingnessFilter m hapNums fs =
    let num = sum [n | (n, f) <- zip hapNums (fsCounts fs), f == Nothing]
        denom = sum hapNums
    in  (fromIntegral num / fromIntegral denom) <= m

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
    chrom = A.takeTill isSpace
