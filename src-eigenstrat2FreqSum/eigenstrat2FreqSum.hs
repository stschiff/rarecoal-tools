{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.Eigenstrat (readEigenstrat, EigenstratIndEntry(..), 
    EigenstratSnpEntry(..), GenoLine, GenoEntry(..))
import SequenceFormats.FreqSum (printFreqSumStdOut, FreqSumHeader(..), FreqSumEntry(..))

import Data.Text (unpack)
import Data.Vector (toList)
import Data.Version (showVersion)
import Pipes (runEffect, (>->))
import Pipes.Safe (runSafeT)
import qualified Pipes.Prelude as P
import Prelude hiding (FilePath)
import qualified Text.PrettyPrint.ANSI.Leijen as PT
import Turtle
import Paths_rarecoal_tools (version)

desc :: Description
desc = Description $ PT.text ("eigenstrat2FreqSum version " ++ showVersion version ++
    ": a program to convert eigenstrat files into a freqSum file")

main :: IO ()
main = do
    (genoFile, snpFile, indFile) <- options desc optionsParser
    runSafeT $ do
        (indEntries, prod) <- readEigenstrat (unpack . format fp $ genoFile)
            (unpack . format fp $ snpFile) (unpack . format fp $ indFile)
        let names = [n | EigenstratIndEntry n _ _ <- indEntries]
            fsh = FreqSumHeader names [2 | _ <- names]
        runEffect $ prod >-> P.map convertToFreqSum >-> printFreqSumStdOut fsh

convertToFreqSum :: (EigenstratSnpEntry, GenoLine) -> FreqSumEntry
convertToFreqSum (EigenstratSnpEntry chrom pos ref alt, genoLine) =
    FreqSumEntry chrom pos ref alt counts
  where
    counts = map makeCounts . toList $ genoLine
    makeCounts HomRef = Just 0
    makeCounts Het = Just 1
    makeCounts HomAlt = Just 2
    makeCounts Missing = Nothing

optionsParser :: Parser (FilePath, FilePath, FilePath)
optionsParser = (,,) <$> optPath "geno" 'g' "input eigenstrat-geno file" <*> 
    optPath "snp" 's' "input eigenstrat-snp file" <*>
    optPath "ind" 'i' "input eigenstrat-ind file"

