{-# LANGUAGE OverloadedStrings #-}

import Rarecoal.Formats.FreqSum (FreqSumEntry(..), parseFreqSum, liftErrors)
import qualified Codec.Compression.GZip as Gzip
import Control.Error (runScript, scriptIO, Script)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import Data.Text (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text as A
import Pipes (Producer, runEffect, yield, (>->), next)
import Pipes.Attoparsec (parsed)
import qualified Pipes.Prelude as P
import qualified Pipes.Text as PT
import System.IO (stdin)
import Turtle hiding (stdin)

type BedEntry = (Int, Int, Int)
data IntervalStatus = BedBehind | FSwithin | BedAhead

argParser :: Parser String
argParser = unpack . format fp <$>
    optPath "bed" 'b' "a bed file that contains the regions to be included"

main :: IO ()
main = do
    bedFile <- options "script to filter a freqSum file through a mask" argParser
    lazyBs <- BL.readFile bedFile
    let decompressedLazyBs = if ".gz" `isSuffixOf` bedFile then Gzip.decompress lazyBs else lazyBs
        lazyT = decodeUtf8 decompressedLazyBs
        textProd = PT.fromLazy lazyT
    runScript $ do
        (fsHeader, fsBody) <- parseFreqSum stdin
        scriptIO $ print fsHeader
        let bedProd = parsed bedFileParser textProd >>= liftErrors
        runEffect $ filterThroughBed bedProd fsBody >-> P.print


filterThroughBed :: Producer BedEntry Script () -> Producer FreqSumEntry Script () ->
    Producer FreqSumEntry Script ()
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
