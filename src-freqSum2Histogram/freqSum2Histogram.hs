import Rarecoal.FreqSum (FreqSumEntry(..), parseFreqSum, FreqSumHeader(..))
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), setNrCalledSites, showHistogram)

import Control.Error (scriptIO, runScript, tryRight)
import Control.Foldl (purely, Fold(..))
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import qualified Options.Applicative as OP
import qualified Pipes.Prelude as P
import System.IO (stdin)

data MyOpts = MyOpts Int Int64 Bool

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

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts maxM nrCalledSites removeMissing) = runScript $ do
    (FreqSumHeader names counts, entries) <- parseFreqSum stdin
    (patternHist, _) <- purely P.fold' buildPatternHist entries
    let hist = RareAlleleHistogram names counts 0 maxM [] [] patternHist
    hist' <- tryRight $ setNrCalledSites nrCalledSites hist
    outs <- tryRight $ showHistogram hist'
    scriptIO $ T.putStr outs
  where
    buildPatternHist = Fold step Map.empty id
    step m fse =
        case mkPat fse of
            Pattern pat ->
                if removeMissing
                then
                    if any (<0) pat then m else Map.insertWith (\_ v -> v + 1) (Pattern pat) 1 m
                else
                    let newPat = map (\p -> max 0 p) pat
                    in  Map.insertWith (\_ v -> v + 1) (Pattern newPat) 1 m
            Higher -> Map.insertWith (\_ v -> v + 1) Higher 1 m
    mkPat = makePattern . fsCounts
    makePattern vec = if isHigherAF vec then Higher else Pattern vec
    isHigherAF = (>maxM) . sum
