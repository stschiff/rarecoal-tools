{-# LANGUAGE BangPatterns #-}

import Rarecoal.RareAlleleHistogram (readHistogramFromHandle, showHistogram, 
                                     RareAlleleHistogram(..), SitePattern)

import Control.Error (runScript, tryRight, errLn, scriptIO, Script, tryJust, tryAssert)
import Control.Foldl (impurely, FoldM(..))
import Control.Lens ((&), (%~), ix)
import Control.Monad (replicateM_, when)
import Control.Monad.Trans.Class (lift)
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import qualified Options.Applicative as OP
import Pipes (yield, for, Producer)
import qualified Pipes.Prelude as P
import System.IO (stdin, IOMode(..), openFile)
import System.Random (randomIO)

data MyOpts = MyOpts String Int FilePath

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "sample a number of haplotypes \
                                \(independently at each site) from a population in a histogram")

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.strOption (OP.short 'n' <> OP.long "name" <>
                            OP.metavar "<NAME>" <> OP.help "the population (by name) from \
                                                            \which to sample")
                <*> OP.option OP.auto (OP.short 'n' <> OP.long "howMany" <> OP.metavar "<INT>" <> 
                                    OP.help "how many samples should be drawn at each site")
                <*> OP.strOption (OP.short 'i' <> OP.long "hist" <>
                                    OP.metavar "<path-to-histogram>" <>
                                    OP.help "the input histogram file, set - for stdin")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts name howMany histPath) = runScript $ do
    handle <- if histPath == "-" then return stdin else scriptIO $ openFile histPath ReadMode
    hist <- readHistogramFromHandle handle
    hist' <- makeNewHist name howMany hist
    outs <- tryRight $ showHistogram hist'
    scriptIO $ T.putStr outs

makeNewHist :: String -> Int -> RareAlleleHistogram -> Script RareAlleleHistogram
makeNewHist name howMany hist = do
    queryIndex <- tryJust ("could not find name: " ++ name) $ lookup name (zip (raNames hist) [0..])
    when (raJackknifeEstimates hist /= Nothing) $ do
        scriptIO $ errLn "handling histograms with jackknife estimates is not yet implemented."
    let histRows = M.toList (raCounts hist)
        nVec = raNVec hist
        patternProducer = mapM_ yield histRows
        sampledProducer = for patternProducer (sampleFromPattern queryIndex howMany nVec)
    (patternMap, _) <- impurely P.foldM' makeMap sampledProducer
    let newNVec = (nVec & ix queryIndex %~ (\v -> v - howMany)) ++ [howMany]
    return hist {raNVec = newNVec, raCounts = patternMap, raJackknifeEstimates = Nothing}

sampleFromPattern :: Int -> Int -> [Int] -> (SitePattern, Int64) ->
                     Producer (SitePattern, Int64) Script ()
sampleFromPattern queryIndex howMany nVec (pattern, count) = do
    lift . scriptIO $ errLn ("processing pattern " ++ show pattern)
    let n = nVec !! queryIndex
        k = pattern !! queryIndex
    if k == 0 then
        yield (pattern ++ [0], count)
    else
        replicateM_ (fromIntegral count) $ do
            newK <- lift $ sampleWithoutReplacement n k howMany
            let newPat = (pattern & ix queryIndex %~ (\v -> v - newK)) ++ [newK]
            yield (newPat, 1)

sampleWithoutReplacement :: Int -> Int -> Int -> Script Int
sampleWithoutReplacement n k howMany = go n k howMany 0
  where
    go !_ !_ !0 !ret = return ret
    go !_ !0 !_ !ret = return ret
    go !n' !k' !howMany' !ret = do
        let noSuccess = choose (n' - k') howMany' / choose n' howMany'
        val <- bernoulli noSuccess
        if val then
            return ret
        else do
            go (n' - 1) (k' - 1) (howMany' - 1) (ret + 1)

choose :: Int -> Int -> Double
choose _ 0 = 1
choose n k = product [fromIntegral (n + 1 - j) / fromIntegral j | j <- [1..k]]
        
bernoulli :: Double -> Script Bool
bernoulli p = (<p) <$> scriptIO randomIO 

makeMap :: FoldM Script (SitePattern, Int64) (M.Map SitePattern Int64)
makeMap = FoldM step initial extract
  where
    step m (p, c) = return $ M.insertWith (+) p c m
    initial = return M.empty
    extract = return 
