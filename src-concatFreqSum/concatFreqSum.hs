{-# LANGUAGE OverloadedStrings #-}
import SequenceFormats.FreqSum (readFreqSumFile, printFreqSumStdOut)

import Control.Exception (AssertionFailed(..))
import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Data.Version (showVersion)
import qualified Options.Applicative as OP
import Pipes (runEffect, (>->))
import Pipes.Safe (runSafeT)
import Paths_rarecoal_tools (version)

main :: IO ()
main = OP.execParser parser >>= runWithOptions
  where
    parser = OP.info (OP.helper <*> options) (OP.progDesc ("concatFreqSum version " ++
        showVersion version ++ ": Concatenates multiple freqSum files together, \
            \checking that each header is the same"))
    options = OP.some (OP.strArgument (OP.metavar "FILES" <> OP.help "input file(s)"))

runWithOptions :: [FilePath] -> IO ()
runWithOptions fns = runSafeT $ do
    allFreqSums <- mapM readFreqSumFile fns
    let (firstHeader:restHeaders) = map fst allFreqSums
        freqSumProds = map snd allFreqSums
    when (any (/=firstHeader) restHeaders) $
        throwM (AssertionFailed "freqSum headers not identical")
    runEffect $ sequence_ freqSumProds >-> printFreqSumStdOut firstHeader
