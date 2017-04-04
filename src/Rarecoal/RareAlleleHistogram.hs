{-# LANGUAGE OverloadedStrings #-}

module Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), readHistogramFromHandle,
                            SitePattern(..), setNrCalledSites, readHistogram, showHistogram) where

import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum)
import Data.List (intercalate, sortBy)
import Control.Monad (when)
import Data.Int (Int64)
import Control.Error (Script, scriptIO, assertErr, throwE, justErr)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import qualified Pipes.Text.IO as PT
import System.IO (Handle, openFile, IOMode(..), hClose)
import Control.Monad.Trans.State.Strict (evalStateT)
import Pipes.Attoparsec (parse)

data RareAlleleHistogram = RareAlleleHistogram {
    raNames :: [String],
    raNVec :: [Int],
    raMinAf :: Int,
    raMaxAf :: Int,
    raConditionOn :: [Int],
    raExcludePatterns :: [[Int]],
    raCounts :: Map.Map SitePattern Int64
}

data SitePattern = Pattern [Int] | Higher deriving (Eq, Ord)
instance Show SitePattern where
    show (Pattern nVec) = intercalate "," . map show $ nVec
    show Higher = "HIGHER"

showHistogram :: RareAlleleHistogram -> Either String T.Text
showHistogram hist = do
    assertErr "can only print histogram with minAf=1 due to format-legacy" $ raMinAf hist == 0
    assertErr "can only print histogram with no conditioning due to format-legacy" $ length (raConditionOn hist) == 0
    let head0 = T.concat ["NAMES=", T.pack . intercalate "," . raNames $ hist]
        head1 = T.concat ["N=", T.pack . intercalate "," . map show . raNVec $ hist]
        head2 = T.concat ["MAX_M=", T.pack . show . raMaxAf $ hist]
        body = [T.intercalate " " [T.pack . show $ k, T.pack . show $ v] | (k, v) <- sorted]
    return $ T.unlines (head0:head1:head2:body)
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
parseHistogram = RareAlleleHistogram <$> (map T.unpack <$> parseNames) <*> parseNVec <*> pure 0 <*> 
                                         parseMaxM <*> pure [] <*> pure [] <*> parseBody
  where
    parseNames = A.string "NAMES=" *> name `A.sepBy1` A.char ',' <* A.endOfLine
    name = A.takeWhile1 (\c -> isAlphaNum c || c == '_')
    parseNVec = A.string "N=" *> A.decimal `A.sepBy1` A.char ',' <* A.endOfLine
    parseMaxM = A.string "MAX_M=" *> A.decimal <* A.endOfLine

parseBody :: A.Parser (Map.Map SitePattern Int64)
parseBody = Map.fromList <$> A.many1 patternLine
  where
    patternLine = do
        pat <- parsePattern <|> parseHigher
        _ <- A.space
        num <- parseLargeInt
        _ <- A.endOfLine
        return (pat, num)
    parsePattern = Pattern <$> A.decimal `A.sepBy1` A.char ','
    parseHigher = A.string "HIGHER" *> pure Higher
    parseLargeInt = read <$> A.many1 A.digit
        
setNrCalledSites :: Int64 -> RareAlleleHistogram -> Either String RareAlleleHistogram
setNrCalledSites newNrCalledSites hist = do
    let zeroKey = Pattern $ replicate (length $ raNVec hist) 0
        nrZeroCalls = Map.findWithDefault 0 zeroKey (raCounts hist)
        nrCalledSites = Map.foldr (+) 0 $ raCounts hist
        nrNonZeroCalls = nrCalledSites - nrZeroCalls
        newZeroCalls = newNrCalledSites - nrNonZeroCalls
    when (newZeroCalls < 0) $ Left "Illegal nrCalledSites" 
    let newHistBody = Map.insert zeroKey newZeroCalls (raCounts hist)
    return $ hist {raCounts = newHistBody}
