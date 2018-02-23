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

data MyOpts = MyOpts [Int] Bool [String]

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
                <*> OP.switch (OP.short 'm' <> OP.long "removeMissing" <> OP.help "if one \
                    \individual/group has missing data (-1) declare the whole group as \
                    \missing data. Default behaviour is interpreting missing data as \
                    \reference calls")
                <*> OP.option (splitOn "," <$> OP.str) (OP.short 'n' <> OP.long "names" <>
                        OP.metavar "NAME_GROUP1,NAME_GROUP2,..." <>
                        OP.help "specify the new names for each group as comma-separated list")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts groups missing newNames) = runScript $ do
    (FreqSumHeader _ counts, entries) <- readFreqSumStdIn
    let newCounts = map sum . splitPlaces groups $ counts
        newEntries = entries >-> P.mapM (tryRight . groupFreqSum groups missing)
    runEffect $ newEntries >-> printFreqSumStdOut (FreqSumHeader (map T.pack newNames) newCounts)

groupFreqSum :: [Int] -> Bool -> FreqSumEntry -> Either T.Text FreqSumEntry
groupFreqSum groups missing fs = do
    assertErr "number of samples doesn't match nVec" $ sum groups == length (fsCounts fs)
    let newCounts = map sum' . splitPlaces groups $ fsCounts fs
    return fs {fsCounts = newCounts}
  where
    sum' values =
        if missing then
            if any (==Nothing) values then Nothing else Just . sum . catMaybes $ values
        else
            Just . sum . catMaybes $ values
