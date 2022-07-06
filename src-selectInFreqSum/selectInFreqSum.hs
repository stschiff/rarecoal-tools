{-# LANGUAGE OverloadedStrings #-}

import Utils (PopNotFoundException(..))

import Control.Exception (throwIO)
import Data.List.Split (splitOn)
import Data.Version (showVersion)
import qualified Options.Applicative as OP
import Pipes ((>->), runEffect)
import qualified Pipes.Prelude as P
import Paths_rarecoal_tools (version)
import SequenceFormats.FreqSum (FreqSumEntry(..), FreqSumHeader(..), readFreqSumStdIn, 
    printFreqSumStdOut)

data MyOpts = MyOpts [String]

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc ("selectInFreqSum version " ++
        showVersion version ++ ": a tool to select columns from a freqSum file, read from stdin"))

parser :: OP.Parser MyOpts
parser = MyOpts <$>
    OP.option (splitOn "," <$> OP.str) (OP.short 'n' <> OP.long "names" <>
        OP.metavar "NAME1,NAME2,..." <> OP.help "comma-separated list of names to select")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts names) = do
    (FreqSumHeader oldNames oldCounts, entries) <- readFreqSumStdIn
    indices <- mapM (findIndex oldNames) names
    let newEntries = entries >-> P.map (selectColumns indices)
        newCounts = map (oldCounts!!) indices
        newHeader = FreqSumHeader names newCounts
    runEffect $ newEntries >-> printFreqSumStdOut newHeader
  where
    findIndex xs x = case (x `lookup` zip xs [0..]) of
        Nothing -> throwIO . PopNotFoundException $ "could not find name: " ++ show x
        Just r -> return r

selectColumns :: [Int] -> FreqSumEntry -> FreqSumEntry
selectColumns indices fs =
    let newCounts = map ((fsCounts fs) !!) indices
    in  fs {fsCounts = newCounts}
