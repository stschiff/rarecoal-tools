{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.FreqSum (FreqSumEntry(..), readFreqSumStdIn, FreqSumHeader(..), printFreqSumStdOut)

import Data.List.Utils (addToAL)
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Pipes ((>->), runEffect)
import qualified Pipes.Prelude as P
import Prelude hiding (FilePath)
import Turtle

data MyOpts = MyOpts (Either [GroupInput] FilePath) Double
data GroupInput = GroupInput Text [Text]

main :: IO ()
main = do
    MyOpts groupInput missingThreshold <-
        options "group columns of a freqSum file into groups, summing up the allele counts" 
            optParser
    groupDefinitions <- getGroupDefinitions groupInput
    runWithGroupDefinitions groupDefinitions missingThreshold
    
    
optParser :: Parser MyOpts
optParser = MyOpts <$> (parseGroupInputByCommand <|> parseGroupInputByFile) <*> parseMissingness

parseGroupInputByCommand :: Parser (Either [GroupInput] FilePath)
parseGroupInputByCommand = Left <$> many parseGroupInput

parseGroupInput :: Parser GroupInput
parseGroupInput = processGroupInputString <$> optText "group" 'g' "A group definition of the form \
    \groupname:sample1,sample2,..., where groupname is the name for the group, and sample1, \
    \sample2 denote columns of the incoming freqSum file. Those can also be already groups. \
    \This option can be given several times, once for each new group. Note that columns not \
    \listed in any group are not output, so if you want to keep individual columns as they are \
    \you need to define degenerate groups with just one column and the same name as before. Note \
    \that explicitly a sample can appear in multiple groups."

processGroupInputString :: Text -> GroupInput
processGroupInputString str =
    let [gname, samplesStr] = T.splitOn ":" str
        samples = T.splitOn "," samplesStr
    in  GroupInput gname samples

parseGroupInputByFile :: Parser (Either [GroupInput] FilePath)
parseGroupInputByFile = Right <$> optPath "groupFile" 'f' "A file with two columns: First \
    \the sample or column name, and second the group name. This is a convenient name to input a \
    \complex setup with many columns and many groups. Note that a sample can occur in multiple \
    \groups."

getGroupDefinitions :: Either [GroupInput] FilePath -> IO [(Text, [Text])]
getGroupDefinitions groupInput = do
    groups <- case groupInput of
        Left directGroupInputs -> return [(n, s') | GroupInput n s' <- directGroupInputs]
        Right fp' -> parseGroupsFromFile fp'
    echo . unsafeTextToLine $ format ("Using the following group definitions:"%w) groups
    return groups
    
parseGroupsFromFile :: FilePath -> IO [(Text, [Text])]
parseGroupsFromFile fp' = fold (lineToText <$> input fp') groupFold
  where
    groupFold = Fold step initial extract
    step ret next =
        let [sample, name] = cut space next
        in  case name `lookup` ret of
                Just previousSamples -> addToAL ret name (sample:previousSamples)
                Nothing -> addToAL ret name [sample]
    initial = []
    extract = id

parseMissingness :: Parser Double
parseMissingness = maybe 0.0 id <$> optional (optDouble "missingThreshold" 'm' "Sets the \
    \level of missingness needed to declare a group as missing. If set to 0, it \
    \means that if any sample in a group has missing data at a site, declare that \
    \group at that site as missing. 1 means that only if all in a group have \
    \missing data, declare the group as missing. Default=0.0")

runWithGroupDefinitions :: [(Text, [Text])] -> Double -> IO ()
runWithGroupDefinitions groups missingThreshold = do
    (FreqSumHeader names sizes, entries) <- readFreqSumStdIn
    let groupsWithIndices = do
            let namesWithIndices = zip names [0..]
            (groupName, groupSamples) <- groups
            let sampleIndices = do
                    sampleName <- groupSamples
                    case sampleName `lookup` namesWithIndices of
                        Just i -> return i
                        Nothing -> error $ "could not find sample name " ++ show sampleName
            return (groupName, sampleIndices)
    let newSizes = do
            (_, sampleIndices) <- groupsWithIndices
            return . sum . map (sizes!!) $ sampleIndices
    let newEntries = entries >-> P.map (groupFreqSum groupsWithIndices missingThreshold sizes)
    runEffect $ newEntries >-> printFreqSumStdOut (FreqSumHeader (map fst groups) newSizes)

groupFreqSum :: [(Text, [Int])] -> Double -> [Int] -> FreqSumEntry -> FreqSumEntry
groupFreqSum groupsWithIndices missingThreshold sizes fs =
    let alleleCountsVec = V.fromList $ zip (fsCounts fs) sizes
        newCounts = do
            (_, sampleIndices) <- groupsWithIndices
            let nrHaps = sum [snd (alleleCountsVec V.! i) | i <- sampleIndices]
                nrMissing = sum $ do
                    i <- sampleIndices
                    Nothing <- return . fst $ alleleCountsVec V.! i
                    return $ snd (alleleCountsVec V.! i)
                nrCounts = sum $ do
                    i <- sampleIndices
                    Just c <- return . fst $ alleleCountsVec V.! i
                    return c
                missingness = fromIntegral nrMissing / fromIntegral nrHaps
            if missingness > missingThreshold
            then return Nothing
            else return $ Just nrCounts
    in  fs {fsCounts = newCounts}
