{-# LANGUAGE OverloadedStrings #-}
import SequenceFormats.FreqSum (readFreqSumFile, printFreqSumStdOut)

import Control.Exception (AssertionFailed(..))
import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Data.Monoid ((<>))
import qualified Options.Applicative as OP
import Pipes (runEffect, (>->))
import Pipes.Safe (runSafeT)

main :: IO ()
main = OP.execParser parser >>= runWithOptions
  where
    parser = OP.info (OP.helper <*> options) (OP.progDesc "concatenates multiple freqSum files")
    options = OP.some (OP.strArgument (OP.metavar "FILES" <> OP.help "input file(s)"))

runWithOptions :: [FilePath] -> IO ()
runWithOptions fns = runSafeT $ do
    allFreqSums <- mapM readFreqSumFile fns
    let (firstHeader:restHeaders) = map fst allFreqSums
        freqSumProds = map snd allFreqSums
    when (any (/=firstHeader) restHeaders) $
        throwM (AssertionFailed "freqSum headers not identical")
    runEffect $ sequence_ freqSumProds >-> printFreqSumStdOut firstHeader
