{-# LANGUAGE OverloadedStrings #-}

module Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), readHistogramFromHandle,
                            SitePattern, readHistogram, showHistogram, showSitePattern) where

import Control.Applicative (optional)
import Control.Error (Script, scriptIO, assertErr, throwE)
import Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.Attoparsec.Text as A
import Data.Char (isAlphaNum)
import Data.Int (Int64)
import Data.List (intercalate, sortBy)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Pipes.Attoparsec (parse)
import qualified Pipes.Text.IO as PT
import System.IO (Handle, openFile, IOMode(..), hClose)

data RareAlleleHistogram = RareAlleleHistogram {
    raNames :: [String],
    raNVec :: [Int],
    raMinAf :: Int,
    raMaxAf :: Int,
    raConditionOn :: [Int],
    raExcludePatterns :: [SitePattern],
    raTotalNrSites :: Int64,
    raCounts :: Map.Map SitePattern Int64,
    raJackknifeEstimates :: Maybe (Map.Map SitePattern (Double, Double))
}

type SitePattern = [Int]
showSitePattern :: SitePattern -> String
showSitePattern nVec = intercalate "," . map show $ nVec

showHistogram :: RareAlleleHistogram -> Either String T.Text
showHistogram hist = do
    assertErr "can only print histogram with minAf=1 due to format-legacy" $ raMinAf hist == 1
    assertErr "can only print histogram with no conditioning due to format-legacy" $
        null (raConditionOn hist)
    assertErr "can only print histogram with no exclude pattern due to format-legacy" $
        null (raExcludePatterns hist)
    let head0 = T.concat ["NAMES=", T.pack . intercalate "," . raNames $ hist]
        head1 = T.concat ["N=", T.pack . intercalate "," . map show . raNVec $ hist]
        head2 = T.concat ["MAX_M=", T.pack . show . raMaxAf $ hist]
        head3 = T.concat ["TOTAL_SITES=", T.pack . show . raTotalNrSites $ hist]
        body = do
            (k, v) <- sorted
            case raJackknifeEstimates hist of
                Nothing -> [T.intercalate " " [T.pack . showSitePattern $ k, T.pack . show $ v]]
                Just jkHist -> do
                    let Just (jkMean, jkSE) = k `Map.lookup` jkHist
                    return $ T.intercalate " " [T.pack . showSitePattern $ k, T.pack . show $ v,
                                                T.pack . show $ jkMean, T.pack . show $ jkSE]
    return $ T.unlines (head0:head1:head2:head3:body)
  where
    sorted = sortBy (\(_, v1) (_, v2)  -> compare v2 v1) $ Map.toList (raCounts hist)

readHistogram :: FilePath -> Script RareAlleleHistogram
readHistogram path = do
    h <- scriptIO $ openFile path ReadMode
    hist <- readHistogramFromHandle h
    scriptIO $ hClose h
    return hist

readHistogramFromHandle :: Handle -> Script RareAlleleHistogram
readHistogramFromHandle handle = do
    res <- evalStateT (parse parseHistogram) . PT.fromHandle $ handle
    case res of
        Nothing -> throwE "histogram file exhausted too early"
        Just (Left err) -> throwE $ "Histogram parsing error: " ++ show err
        Just (Right hist) -> return hist
    
parseHistogram :: A.Parser RareAlleleHistogram
parseHistogram = do
    names <- map T.unpack <$> parseNames
    nVec <- parseNVec
    maxM <- parseMaxM
    totalNrSites <- parseTotalNrSites
    body <- parseBody
    let countHist = Map.fromList $ [(k, c) | (k, c, _) <- body]
        jkHist = case head body of
            (_, _, Just _) -> Just . Map.fromList $ [(k, (jkMean, jkSE)) |
                                                     (k, _, Just (jkMean, jkSE)) <- body]
            _ -> Nothing
    return $ RareAlleleHistogram names nVec 1 maxM [][] totalNrSites countHist jkHist
  where
    parseNames = A.string "NAMES=" *> name `A.sepBy1` A.char ',' <* A.endOfLine
    name = A.takeWhile1 (\c -> isAlphaNum c || c == '_')
    parseNVec = A.string "N=" *> A.decimal `A.sepBy1` A.char ',' <* A.endOfLine
    parseMaxM = A.string "MAX_M=" *> A.decimal <* A.endOfLine
    parseTotalNrSites = A.string "TOTAL_SITES=" *> A.decimal <* A.endOfLine

parseBody :: A.Parser [(SitePattern, Int64, Maybe (Double, Double))]
parseBody = A.many1 patternLine
  where
    patternLine = (,,) <$> parsePattern <* A.space <*> parseLargeInt <*> optional parseJackknife <* 
        A.endOfLine
    parsePattern = A.decimal `A.sepBy1` A.char ','
    parseLargeInt = read <$> A.many1 A.digit
    parseJackknife = (,) <$> (A.space *> A.double) <* A.space <*> A.double
