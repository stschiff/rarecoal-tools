import SequenceFormats.FreqSum (FreqSumEntry(..), readFreqSumStdIn, FreqSumHeader(..))
import SequenceFormats.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern, showHistogram)

import Control.Error (scriptIO, runScript, tryRight)
import Control.Foldl (purely, Fold(..), list)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T
-- import Debug.Trace (trace)
import Lens.Family2 (view)
import qualified Options.Applicative as OP
import Pipes.Group (groupsBy, folds)
import qualified Pipes.Prelude as P

data MyOpts = MyOpts Int Int64 Bool Bool

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "Tool to convert a freqSum file, read \
    \from stdin, to to a histogram file as needed for rarecoal.")

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option OP.auto (OP.long "maxM"
                                       <> OP.short 'm'
                                       <> OP.metavar "INT"
                                       <> OP.value 10
                                       <> OP.showDefault
                                       <> OP.help "Specify the maximum allele count per population")
                <*> OP.option OP.auto (OP.long "nrCalledSites" <> OP.short 'N' <>
                                       OP.help "set the total nr of called sites. This sets the \
                                       \number of non-variant sites (via the pattern \
                                       \consisting of zeros only) such that the total number of \
                                       \sites matches the number given. This number is \
                                       \important for estimating population sizes correctly, see \
                                       \the README for instructions.")
                <*> OP.switch (OP.long "removeMissing" <> OP.short 'r' <> OP.help "remove sites \
                               \at which any selected column is missing (-1). By default, \
                               \missing data is interpreted as reference alleles.")
                <*> OP.switch (OP.long "jackknife" <> OP.short 'j' <>
                    OP.help "run with weighted jackknife error estimate using chromosome-drop-out. \
                        \Warning: This assumes that input is ordered by chromosome. It doesn't \
                        \necessarily have to be sorted, but all sites from one chromosome need \
                        \to be consecutive.")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts maxM nrCalledSites removeMissing jackknife) = runScript $ do
    (FreqSumHeader names counts, entries) <- readFreqSumStdIn
    let entriesByChromosome = view (groupsBy (\fs1 fs2 -> fsChrom fs1 == fsChrom fs2)) entries
        folder = (,) <$> buildPatternHist removeMissing maxM <*> getNrSites removeMissing
        patternHistAndLengthProd = purely folds folder entriesByChromosome
    patternHistsAndLengths <- purely P.fold list patternHistAndLengthProd
    let patternHists = map fst patternHistsAndLengths
        nrSites = map snd patternHistsAndLengths
        totalNrSites = sum nrSites
        mergedHist = Map.unionsWith (+) patternHists
    let jackknifeEstimatesDict =
            if jackknife then
                let effChromLengths =
                        [fromIntegral n / fromIntegral totalNrSites * fromIntegral nrCalledSites |
                         n <- nrSites]
                in  Just . Map.fromList $ do
                    key <- Map.keys mergedHist
                    let countsPerChromosome = map (Map.findWithDefault 0 key) patternHists
                        (jackknifeMean, jackknifeVar) =
                            computeJackknife effChromLengths countsPerChromosome
                    return (key, (jackknifeMean, jackknifeVar))
            else Nothing

    let hist = RareAlleleHistogram names counts 1 maxM [] [] nrCalledSites mergedHist
                                   jackknifeEstimatesDict
    outs <- tryRight $ showHistogram hist
    scriptIO $ T.putStr outs

buildPatternHist :: Bool -> Int -> Fold FreqSumEntry (Map.Map SitePattern Int64)
buildPatternHist removeMissing maxM = Fold step Map.empty id
  where
    step m fse =
        let pat = fsCounts fse
        in  if removeMissing && any (==Nothing) pat then m
            else
                let newPat = map (maybe 0 id) pat
                in  if sum newPat <= maxM then Map.insertWith (\_ v -> v + 1) newPat 1 m else m

-- see F.M.T.A. Busing, E. Meijer, and R. van der Leeden. Delete-m jackknife for unequal
-- Statistics and Computing, 9:3–8, 1999.
computeJackknife :: [Double] -> [Int64] -> (Double, Double)
computeJackknife effectiveChromLengths countsPerChrom =
    let countsPerChrom' = map fromIntegral countsPerChrom
        totalCounts = sum countsPerChrom'
        m = effectiveChromLengths
        n = sum m
        theta = totalCounts / n
        thetaMinus = [(totalCounts - c) / (n - mj) | (c, mj) <- zip countsPerChrom' m]
        g = fromIntegral $ length m
        thetaJ = g * theta - sum [(n - mj) * thetaMinusJ / n |
                                  (mj, thetaMinusJ) <- zip m thetaMinus]
        h = [n / mj | mj <- m]
        tau = [hj * theta - (hj - 1.0) * thetaMinusJ | (hj, thetaMinusJ) <- zip h thetaMinus]
        sigmaSquare = sum [(tauJ - thetaJ) ^ (2 :: Int) / (hj - 1.0) | (tauJ, hj) <- zip tau h] / g
    in  (theta, sqrt sigmaSquare)

getNrSites :: Bool -> Fold FreqSumEntry Int64
getNrSites removeMissing = Fold step 0 id
  where
    step count fse =
        let pat = fsCounts fse
        in  if removeMissing && any (==Nothing) pat then count
            else
                let newPat = map (maybe 0 id) pat
                in  if sum newPat > 0 then count + 1 else count
