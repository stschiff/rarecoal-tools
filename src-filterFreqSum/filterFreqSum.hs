{-# LANGUAGE OverloadedStrings #-}

import Rarecoal.FreqSum (FreqSumEntry(..), parseFreqSum, liftErrors)

import Control.Error (runScript, scriptIO, Script)
import Control.Monad.Trans.Class (lift)
import Data.Text (unpack)
import OrderedZip (orderedZip)
import qualified Data.Attoparsec.Text as A
import Pipes (Producer, runEffect, yield, (>->), next)
import Pipes.Attoparsec (parsed)
import qualified Pipes.Prelude as P
import qualified Pipes.Text as PT
import qualified Pipes.Text.IO as PT
import System.IO (stdin, IOMode(..), withFile)
import Turtle hiding (stdin)

type BedEntry = (Int, Int, Int)
data IntervalStatus = BedBehind | FSwithin | BedAhead

argParser = optPath "bed" 'b' "a bed file that contains the regions to be included"

main = do
    bedFile <- options "script to filter a freqSum file through a mask" argParser
    withFile (unpack . format fp $ bedFile) ReadMode $ \h -> runScript $ do
        (fsHeader, fsBody) <- parseFreqSum stdin
        scriptIO $ print fsHeader
        let bedProd = parsed bedFileParser (PT.fromHandle h) >>= liftErrors
        runEffect $ filterThroughBed bedProd fsBody >-> P.print

filterThroughBed :: Producer BedEntry Script () -> Producer FreqSumEntry Script () ->
    Producer FreqSumEntry Script ()
filterThroughBed bedProd fsProd = do
    b <- lift $ next bedProd
    let (bedCurrent, bedRest) = case b of
            Left r -> error "Bed file empty or not readable"
            Right r -> r
    f <- lift $ next fsProd
    let (fsCurrent, fsRest) = case f of
            Left r -> error "FreqSum stream empty or not readable"
            Right r -> r
    go bedCurrent fsCurrent bedRest fsRest
  where
    go bedCurrent fsCurrent bedRest fsRest = do
        let (bedChrom, bedStart, bedEnd) = bedCurrent
            FreqSumEntry fsChrom' fsPos' _ _ _ = fsCurrent
            recurseNextBed = do
                b <- lift $ next bedRest
                case b of
                    Left () -> return ()
                    Right (nextBed, bedRest') -> go nextBed fsCurrent bedRest' fsRest
            recurseNextFS = do
                f <- lift $ next fsRest
                case f of
                    Left () -> return ()
                    Right (nextFS, fsRest') -> go bedCurrent nextFS bedRest fsRest'
        case bedCurrent `checkIntervalStatus` fsCurrent of
            BedBehind -> recurseNextBed
            BedAhead -> recurseNextFS
            FSwithin -> do
                yield fsCurrent
                recurseNextFS
        
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
bedFileParser = (,,) <$> A.decimal <* A.skipSpace <*> A.decimal <* A.skipSpace <*> A.decimal <* 
    A.endOfLine
    
    