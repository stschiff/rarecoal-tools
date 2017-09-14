{-# LANGUAGE TemplateHaskell #-}

import Data.List.Split (splitPlaces)
import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Options.Applicative as OP
import Rarecoal.Formats.RareAlleleHistogram(RareAlleleHistogram(..), showHistogram)
import Control.Error (runScript, scriptIO, assertErr, tryRight)
import Data.Int (Int64)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.IO as T

data MyOpts = MyOpts [Int] Int Int64 [String]

main :: IO ()
main = OP.execParser opts >>= mainWithOptions
  where
    parser =
        MyOpts <$> OP.option (map read . splitOn "," <$> OP.str)
            (OP.short 'n' <> OP.long "nVec" <> OP.metavar "N1,N2,..." <>
                OP.help "comma-separated list of the number in each subgroup") <*>
            OP.option OP.auto (OP.short 'm' <> OP.long "maxM" <> OP.metavar "<INT>" <>
                OP.help "maximum allele count") <*>
            OP.option OP.auto (OP.short 'N' <> OP.long "nrCalledSites" <>
                OP.metavar "INT" <> OP.help "total length of the genome simulated (not just \
                \the number of segregating sites)") <*>
            OP.option (splitOn "," <$> OP.str) (OP.long "names" <> OP.metavar "NAME1,NAME2,..." <>
                OP.help "comma-separated list of names of the groups")
    opts = OP.info (OP.helper <*> parser)
                        (OP.progDesc "converts ms-format output from simulations into a \
                                        \histogram as used for Rarecoal. Expects a matrix of \'1\' \
                                        \and \'0\', where each line corresponds to a single \
                                        \chromosome.")

mainWithOptions :: MyOpts -> IO ()
mainWithOptions (MyOpts nVec maxAf nrCalledSites names) = runScript $
    scriptIO B.getContents >>= tryRight . makeHist nVec maxAf names nrCalledSites
                           >>= tryRight . showHistogram
                           >>= scriptIO . T.putStr

makeHist :: [Int] -> Int -> [String] -> Int64 -> B.ByteString -> Either String RareAlleleHistogram
makeHist nVec maxAf names nrCalledSites s = do
    let loci = B.transpose . B.lines $ s
    assertErr "nVec doesn't sum up to correct number of samples" $ B.length (head loci) == sum (map fromIntegral nVec)
    let getFreqSum = map (length . filter (=='1')) . splitPlaces nVec . B.unpack
        freqSums = map getFreqSum loci
        insert m k = if (sum k <= maxAf) then M.insertWith (+) k 1 m else m
        counts = foldl insert M.empty $ freqSums
    return $ RareAlleleHistogram names nVec 0 maxAf [] [] nrCalledSites counts Nothing
