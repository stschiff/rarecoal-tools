{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.FreqSum (FreqSumEntry(..), FreqSumHeader(..), readFreqSumStdIn, 
    printFreqSumStdOut)

import Control.Error (runScript, tryJust)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (pack)
import qualified Options.Applicative as OP
import Pipes ((>->), runEffect)
import qualified Pipes.Prelude as P
import Turtle (format, s, (%))

data MyOpts = MyOpts [String]

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "Selects columns from a freqSum file, \
                                           \read from stdin")

parser :: OP.Parser MyOpts
parser = MyOpts <$>
    OP.option (splitOn "," <$> OP.str) (OP.short 'n' <> OP.long "names" <>
        OP.metavar "NAME1,NAME2,..." <> OP.help "comma-separated list of names to select")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts names) = runScript $ do
    (FreqSumHeader oldNames oldCounts, entries) <- readFreqSumStdIn
    indices <- mapM (findIndex oldNames) (map pack names)
    let newEntries = entries >-> P.map (selectColumns indices)
--                             >-> P.filter ((>0) . sum . catMaybes . fsCounts)
        newCounts = map (oldCounts!!) indices
        newHeader = FreqSumHeader (map pack names) newCounts
    runEffect $ newEntries >-> printFreqSumStdOut newHeader
  where
    findIndex xs x = tryJust (format ("could not find name: "%s) x) $ x `lookup` zip xs [0..]

selectColumns :: [Int] -> FreqSumEntry -> FreqSumEntry
selectColumns indices fs =
    let newCounts = map ((fsCounts fs) !!) indices
    in  fs {fsCounts = newCounts}
