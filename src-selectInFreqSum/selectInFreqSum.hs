import Rarecoal.FreqSum (FreqSumEntry(..), FreqSumHeader(..), parseFreqSum, printFreqSum)

import Control.Error (runScript, tryJust)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import qualified Options.Applicative as OP
import Pipes ((>->))
import qualified Pipes.Prelude as P
import System.IO (stdin)

data MyOpts = MyOpts [String]

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "Selects columns from a freqSum file, \
                                           \read from stdin")

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option (splitOn "," <$> OP.str) (OP.short 'n' <> OP.long "names" <>
                OP.metavar "NAME1,NAME2,..." <> OP.help "comma-separated list of names to select")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts names) = runScript $ do
    (FreqSumHeader oldNames oldCounts, entries) <- parseFreqSum stdin
    indices <- mapM (findIndex oldNames) names
    let newEntries = entries >-> P.map (selectColumns indices) >->
                                 P.filter ((>0) . sum . filter (>=0) . fsCounts)
        newCounts = map (oldCounts!!) indices
        newHeader = FreqSumHeader names newCounts
    printFreqSum (newHeader, newEntries)
  where
    findIndex xs x = tryJust ("could not find name: " ++ x) $ x `lookup` zip xs [0..] 

selectColumns :: [Int] -> FreqSumEntry -> FreqSumEntry
selectColumns indices fs =
    let newCounts = map ((fsCounts fs) !!) indices
    in  fs {fsCounts = newCounts}
