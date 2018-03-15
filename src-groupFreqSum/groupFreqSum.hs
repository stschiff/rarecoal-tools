{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.FreqSum (FreqSumEntry(..), readFreqSumStdIn, FreqSumHeader(..), printFreqSumStdOut)

import Control.Error (runScript, tryRight, assertErr)
import Data.List.Split (splitPlaces, splitOn)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Options.Applicative as OP
import Pipes ((>->), runEffect)
import qualified Pipes.Prelude as P

data MyOpts = MyOpts [Int] Double [String]

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "This tool merges samples or groups into \
                        \larger groups, adding up the allele counts. It reads from stdin")

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option OP.auto (OP.short 'g' <> OP.long "groups" <>
                                       OP.metavar "[NGROUP1,NGROUP2]" <>
                                       OP.help "comma-separated list of numbers that \
                                       \specify how to join sample or groups, surrounded by \
                                       \square brackets. Example: -n [20,20,1] specifies \
                                       \that you want to merge the first twenty samples/groups \
                                       \into one, and sample 21 through 40, and then \
                                       \have the last group separate. See README for instructions.")
                <*> OP.option OP.auto (OP.short 'm' <> OP.long "missingThreshold" <>
                    OP.help "Sets the \
                    \level of missingness needed to declare a group as missing. If set to 0, it \
                    \means that if any sample in a group has missing data at a site, declare that \
                    \group at that site as missing. 1 means that only if all in a group have \
                    \missing data, declare the group as missing." <> OP.value 0.0 <> OP.showDefault)
                <*> OP.option (splitOn "," <$> OP.str) (OP.short 'n' <> OP.long "names" <>
                        OP.metavar "NAME_GROUP1,NAME_GROUP2,..." <>
                        OP.help "specify the new names for each group as comma-separated list")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts groups missingThreshold newNames) = runScript $ do
    (FreqSumHeader _ sizes, entries) <- readFreqSumStdIn
    let newSizes = map sum . splitPlaces groups $ sizes
        newEntries = entries >-> P.mapM (tryRight . groupFreqSum groups missingThreshold sizes)
    runEffect $ newEntries >-> printFreqSumStdOut (FreqSumHeader (map T.pack newNames) newSizes)

groupFreqSum :: [Int] -> Double -> [Int] -> FreqSumEntry -> Either T.Text FreqSumEntry
groupFreqSum groups missingThreshold sizes fs = do
    assertErr "number of samples doesn't match nVec" $ sum groups == length (fsCounts fs)
    let newCounts = do
            tuples <- splitPlaces groups (zip sizes (fsCounts fs))
            let nrHaps =  sum . map fst $ tuples
                nrMissing = sum . map fst . filter ((==Nothing) . snd) $ tuples
                nrCounts = sum . catMaybes . map snd $ tuples
                missingness = fromIntegral nrMissing / fromIntegral nrHaps
            if missingness > missingThreshold
            then return Nothing
            else return $ Just nrCounts
    return fs {fsCounts = newCounts}
